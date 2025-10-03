const std = @import("std");
const c = @cImport({
    @cInclude("whisper.h");
    @cInclude("AudioToolbox/AudioToolbox.h");
});

pub const STTBackend = struct {
    allocator: std.mem.Allocator,
    whisper_ctx: *c.whisper_context,
    whisper_state: ?*c.whisper_state,
    audio_buffer: std.ArrayList(f32),
    is_recording: bool,
    audio_queue: c.AudioQueueRef,
    audio_format: c.AudioStreamBasicDescription,
    current_text: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) !STTBackend {
        const model_path = "/Users/bret.horne/git/elisp/macos-module/models/ggml-base.en.bin";

        std.debug.print("[STT] Attempting to load Whisper model from {s}\n", .{model_path});

        // Use exact same pattern as CLI example
        var cparams = c.whisper_context_default_params();
        cparams.use_gpu = false;    // Force disable GPU for Emacs module stability
        cparams.flash_attn = false; // Disable flash attention too

        // Use no_state version to avoid C++ exception in state initialization
        const whisper_ctx = c.whisper_init_from_file_with_params_no_state(model_path, cparams);

        if (whisper_ctx == null) {
            std.debug.print("[STT] Failed to load Whisper model\n", .{});
            return error.WhisperInitFailed;
        }

        std.debug.print("[STT] Whisper model loaded successfully (CPU only)\n", .{});

        // Setup audio format for recording - whisper needs 16kHz
        var format = std.mem.zeroes(c.AudioStreamBasicDescription);
        format.mSampleRate = 16000.0;
        format.mFormatID = c.kAudioFormatLinearPCM;
        format.mFormatFlags = c.kAudioFormatFlagIsFloat | c.kAudioFormatFlagIsPacked;
        format.mFramesPerPacket = 1;
        format.mChannelsPerFrame = 1;
        format.mBitsPerChannel = 32;
        format.mBytesPerPacket = 4;
        format.mBytesPerFrame = 4;

        return STTBackend{
            .allocator = allocator,
            .whisper_ctx = whisper_ctx.?,
            .whisper_state = null, // Will be initialized when needed
            .audio_buffer = std.ArrayList(f32).init(allocator),
            .is_recording = false,
            .audio_queue = null,
            .audio_format = format,
            .current_text = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *STTBackend) void {
        if (self.is_recording) {
            self.stopRecording() catch {};
        }

        // Free whisper state if allocated
        if (self.whisper_state) |state| {
            c.whisper_free_state(state);
        }

        // Free whisper context
        c.whisper_free(self.whisper_ctx);
        self.audio_buffer.deinit();
        self.current_text.deinit();
    }

    // Audio queue callback
    fn audioQueueCallback(
        userData: ?*anyopaque,
        queue: c.AudioQueueRef,
        buffer: c.AudioQueueBufferRef,
        startTime: [*c]const c.AudioTimeStamp,
        numPackets: u32,
        packetDesc: [*c]const c.AudioStreamPacketDescription,
    ) callconv(.C) void {
        _ = startTime;
        _ = numPackets;
        _ = packetDesc;

        const self = @as(*STTBackend, @ptrCast(@alignCast(userData.?)));

        std.debug.print("[STT] Audio callback called, is_recording={}\n", .{self.is_recording});
        if (self.is_recording) {
            // Convert buffer data to f32 samples
            const samples = @as([*]f32, @ptrCast(@alignCast(buffer.*.mAudioData)));
            const sample_count = buffer.*.mAudioDataByteSize / 4;
            std.debug.print("[STT] Processing {} samples, byte_size={}\n", .{sample_count, buffer.*.mAudioDataByteSize});

            // Append to our buffer
            self.audio_buffer.appendSlice(samples[0..sample_count]) catch {};
            std.debug.print("[STT] Audio buffer now {} samples\n", .{self.audio_buffer.items.len});

            // Re-enqueue buffer
            _ = c.AudioQueueEnqueueBuffer(queue, buffer, 0, null);
        }
    }

    pub fn startRecording(self: *STTBackend) !void {
        if (self.is_recording) {
            return;
        }

        // Clear previous audio
        self.audio_buffer.clearRetainingCapacity();
        self.current_text.clearRetainingCapacity();

        // Create audio queue
        const result = c.AudioQueueNewInput(
            &self.audio_format,
            audioQueueCallback,
            self,
            null,
            null,
            0,
            &self.audio_queue,
        );

        if (result != 0) {
            return error.AudioQueueCreationFailed;
        }

        // Allocate and enqueue buffers
        const buffer_size: u32 = 16384; // 1024 samples * 4 bytes * 4 buffers
        var i: u32 = 0;
        while (i < 3) : (i += 1) {
            var buffer: c.AudioQueueBufferRef = undefined;
            _ = c.AudioQueueAllocateBuffer(self.audio_queue, buffer_size, &buffer);
            _ = c.AudioQueueEnqueueBuffer(self.audio_queue, buffer, 0, null);
        }

        // Start recording
        const start_result = c.AudioQueueStart(self.audio_queue, null);
        std.debug.print("[STT] AudioQueueStart result: {}\n", .{start_result});

        self.is_recording = true;
        std.debug.print("[STT] Whisper recording started, is_recording={}\n", .{self.is_recording});
    }

    pub fn stopRecording(self: *STTBackend) !void {
        if (!self.is_recording) {
            return;
        }

        // Stop audio queue
        _ = c.AudioQueueStop(self.audio_queue, 1);
        _ = c.AudioQueueDispose(self.audio_queue, 1);

        self.is_recording = false;
        self.audio_queue = null;


        // Process final audio with Whisper
        if (self.audio_buffer.items.len > 0) {
            self.processWithWhisper();
        }

        std.debug.print("[STT] Recording stopped\n", .{});
    }

    fn processWithWhisper(self: *STTBackend) void {
        std.debug.print("[STT] Processing audio with Whisper\n", .{});

        // Initialize state if not already done
        if (self.whisper_state == null) {
            std.debug.print("[STT] Initializing whisper state\n", .{});
            self.whisper_state = c.whisper_init_state(self.whisper_ctx);
            if (self.whisper_state == null) {
                std.debug.print("[STT] Failed to initialize whisper state\n", .{});
                return;
            }
        }

        var params = c.whisper_full_default_params(c.WHISPER_SAMPLING_GREEDY);

        // Configure for better recognition
        params.n_threads = 2;  // Use fewer threads
        params.translate = false;  // Don't translate
        params.suppress_blank = false;  // Keep blank segments
        params.suppress_non_speech_tokens = false;  // Keep all tokens
        params.no_context = true;  // Don't use context from previous segments
        params.single_segment = false;  // Allow multiple segments
        params.no_timestamps = true;  // Don't generate timestamps
        params.language = "en";  // Force English

        // Process audio using state-based API
        const result = c.whisper_full_with_state(
            self.whisper_ctx,
            self.whisper_state.?,
            params,
            self.audio_buffer.items.ptr,
            @intCast(self.audio_buffer.items.len),
        );

        if (result == 0) {
            const n_segments = c.whisper_full_n_segments_from_state(self.whisper_state.?);

            self.current_text.clearRetainingCapacity();

            var i: i32 = 0;
            while (i < n_segments) : (i += 1) {
                const text = c.whisper_full_get_segment_text_from_state(self.whisper_state.?, i);
                if (text != null) {
                    const text_slice = std.mem.span(text);
                    // Skip blank audio markers
                    if (!std.mem.containsAtLeast(u8, text_slice, 1, "[BLANK_AUDIO]")) {
                        self.current_text.appendSlice(text_slice) catch {};
                    }
                }
            }

            std.debug.print("[STT] Whisper transcribed: {s}\n", .{self.current_text.items});
        } else {
            std.debug.print("[STT] Whisper processing failed\n", .{});
        }
    }

    pub fn getCurrentText(self: *STTBackend) []const u8 {
        // Debug: show buffer status
        std.debug.print("[STT] getCurrentText: buffer_len={}, current_text_len={}, text='{s}'\n", .{self.audio_buffer.items.len, self.current_text.items.len, self.current_text.items});
        return self.current_text.items;
    }

    pub fn clearText(self: *STTBackend) void {
        self.current_text.clearRetainingCapacity();
        std.debug.print("[STT] Text buffer cleared\n", .{});
    }

    pub fn processBufferedAudio(self: *STTBackend) void {
        // Need at least 1.1 seconds to avoid whisper errors
        const MIN_SAMPLES = 17600; // 1.1 seconds at 16kHz

        std.debug.print("[STT] processBufferedAudio: recording={}, buffer_len={}\n", .{self.is_recording, self.audio_buffer.items.len});
        if (self.is_recording and self.audio_buffer.items.len >= MIN_SAMPLES) {
            std.debug.print("[STT] Processing with buffer of {} samples\n", .{self.audio_buffer.items.len});
            self.processWithWhisperFast();
            // Clear buffer after processing
            self.audio_buffer.clearRetainingCapacity();
        }
    }

    fn processWithWhisperFast(self: *STTBackend) void {
        // Process the actual audio buffer directly
        const audio_len = self.audio_buffer.items.len;
        std.debug.print("[STT] Processing {} samples with whisper\n", .{audio_len});

        // Process only the small chunk
        if (self.whisper_state == null) {
            self.whisper_state = c.whisper_init_state(self.whisper_ctx);
            if (self.whisper_state == null) return;
        }

        var params = c.whisper_full_default_params(c.WHISPER_SAMPLING_GREEDY);
        params.n_threads = 1;  // Single thread only
        params.translate = false;
        params.suppress_blank = false;  // Keep blank segments
        params.suppress_non_speech_tokens = false;  // Keep all tokens
        params.no_context = true;  // Don't use context from previous segments
        params.no_timestamps = true;  // Don't generate timestamps
        params.language = "en";  // Force English

        // Process only the limited chunk
        const result = c.whisper_full_with_state(
            self.whisper_ctx,
            self.whisper_state.?,
            params,
            self.audio_buffer.items.ptr,
            @intCast(audio_len),
        );

        if (result == 0) {
            const n_segments = c.whisper_full_n_segments_from_state(self.whisper_state.?);

            // Clear and get all text
            self.current_text.clearRetainingCapacity();

            var i: i32 = 0;
            while (i < n_segments) : (i += 1) {
                const text = c.whisper_full_get_segment_text_from_state(self.whisper_state.?, i);
                if (text != null) {
                    const text_slice = std.mem.span(text);
                    // Skip blank audio markers
                    if (!std.mem.containsAtLeast(u8, text_slice, 1, "[BLANK_AUDIO]")) {
                        self.current_text.appendSlice(text_slice) catch {};
                    }
                }
            }

            std.debug.print("[STT] Whisper recognized: {s}\n", .{self.current_text.items});
        } else {
            std.debug.print("[STT] Whisper processing failed with code {}\n", .{result});
        }
    }

    pub fn isAvailable(self: *STTBackend) bool {
        _ = self;
        return true;
    }

    pub fn requestAuthorization() void {
        // No authorization needed for Whisper
    }
};
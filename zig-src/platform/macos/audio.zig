const std = @import("std");
const c = @cImport({
    @cInclude("AudioToolbox/AudioToolbox.h");
    @cInclude("CoreAudio/CoreAudio.h");
});

pub const AudioBackend = struct {
    allocator: std.mem.Allocator,
    audio_queue: ?c.AudioQueueRef,
    buffers: [3]c.AudioQueueBufferRef,
    is_recording: bool,
    sample_rate: f64,
    channels: u32,
    audio_buffer: std.ArrayList(f32),
    mutex: std.Thread.Mutex,

    const buffer_size: u32 = 16384; // 16KB buffers
    const num_buffers: u32 = 3;

    pub fn init(allocator: std.mem.Allocator) !AudioBackend {
        return AudioBackend{
            .allocator = allocator,
            .audio_queue = null,
            .buffers = undefined,
            .is_recording = false,
            .sample_rate = 16000.0, // 16kHz for speech
            .channels = 1, // Mono
            .audio_buffer = std.ArrayList(f32).init(allocator),
            .mutex = .{},
        };
    }

    pub fn deinit(self: *AudioBackend) void {
        if (self.is_recording) {
            self.stopRecording() catch {};
        }
        self.audio_buffer.deinit();
    }

    pub fn startRecording(self: *AudioBackend) !void {
        if (self.is_recording) {
            return error.AlreadyRecording;
        }

        var audio_format = std.mem.zeroes(c.AudioStreamBasicDescription);
        audio_format.mSampleRate = self.sample_rate;
        audio_format.mFormatID = c.kAudioFormatLinearPCM;
        audio_format.mFormatFlags = c.kAudioFormatFlagIsFloat | c.kAudioFormatFlagIsPacked;
        audio_format.mFramesPerPacket = 1;
        audio_format.mChannelsPerFrame = @intCast(self.channels);
        audio_format.mBitsPerChannel = 32;
        audio_format.mBytesPerPacket = 4 * self.channels;
        audio_format.mBytesPerFrame = 4 * self.channels;

        // Create audio queue
        var queue: c.AudioQueueRef = undefined;
        const status = c.AudioQueueNewInput(
            &audio_format,
            audioQueueInputCallback,
            @ptrCast(self),
            null,
            null,
            0,
            &queue,
        );

        if (status != 0) {
            return error.AudioQueueCreationFailed;
        }

        self.audio_queue = queue;

        // Allocate buffers
        for (0..num_buffers) |i| {
            var buffer: c.AudioQueueBufferRef = undefined;
            const alloc_status = c.AudioQueueAllocateBuffer(
                queue,
                buffer_size,
                &buffer,
            );
            if (alloc_status != 0) {
                return error.BufferAllocationFailed;
            }
            self.buffers[i] = buffer;

            // Enqueue buffer for recording
            const enqueue_status = c.AudioQueueEnqueueBuffer(
                queue,
                buffer,
                0,
                null,
            );
            if (enqueue_status != 0) {
                return error.BufferEnqueueFailed;
            }
        }

        // Start recording
        const start_status = c.AudioQueueStart(queue, null);
        if (start_status != 0) {
            return error.AudioQueueStartFailed;
        }

        self.is_recording = true;
    }

    pub fn stopRecording(self: *AudioBackend) ![]const f32 {
        if (!self.is_recording) {
            return &[_]f32{};
        }

        if (self.audio_queue) |queue| {
            // Stop the queue
            _ = c.AudioQueueStop(queue, 1);

            // Dispose of the queue
            _ = c.AudioQueueDispose(queue, 1);
            self.audio_queue = null;
        }

        self.is_recording = false;

        // Return a copy of the audio buffer
        return try self.audio_buffer.toOwnedSlice();
    }

    pub fn getAudioBuffer(self: *AudioBackend) []const f32 {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.audio_buffer.items;
    }

    fn audioQueueInputCallback(
        inUserData: ?*anyopaque,
        inAQ: c.AudioQueueRef,
        inBuffer: c.AudioQueueBufferRef,
        inStartTime: [*c]const c.AudioTimeStamp,
        inNumPackets: c.UInt32,
        inPacketDesc: [*c]const c.AudioStreamPacketDescription,
    ) callconv(.C) void {
        _ = inStartTime;
        _ = inPacketDesc;

        const self = @as(*AudioBackend, @ptrCast(@alignCast(inUserData.?)));

        if (inNumPackets > 0 and inBuffer != null) {
            const float_data = @as([*]f32, @ptrCast(@alignCast(inBuffer.*.mAudioData)));
            const num_samples = inBuffer.*.mAudioDataByteSize / @sizeOf(f32);

            self.mutex.lock();
            defer self.mutex.unlock();

            // Append audio data to buffer
            self.audio_buffer.appendSlice(float_data[0..num_samples]) catch {
                std.debug.print("Failed to append audio data\n", .{});
            };
        }

        // Re-enqueue the buffer for continuous recording
        if (self.is_recording) {
            _ = c.AudioQueueEnqueueBuffer(inAQ, inBuffer, 0, null);
        }
    }

    pub fn requestPermission() !void {
        // On macOS, we need to request microphone permission
        // This would typically be done through AVAudioSession or similar
        // For now, we'll assume permission is granted
        // Real implementation would use AVCaptureDevice.requestAccess
    }

    pub fn checkPermission() bool {
        // Check if we have microphone permission
        // Real implementation would check AVAuthorizationStatus
        return true;
    }
};
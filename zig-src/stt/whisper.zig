const std = @import("std");

const c = @cImport({
    @cInclude("whisper.h");
});

pub const WhisperContext = struct {
    ctx: *c.whisper_context,
    params: c.whisper_full_params,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, model_path: []const u8) !WhisperContext {
        const model_path_c = try allocator.dupeZ(u8, model_path);
        defer allocator.free(model_path_c);

        const ctx = c.whisper_init_from_file(model_path_c.ptr);
        if (ctx == null) {
            return error.ModelLoadFailed;
        }

        // Initialize params with default settings for speech
        var params = c.whisper_full_default_params(c.WHISPER_SAMPLING_GREEDY);
        params.print_special = false;
        params.print_realtime = false;
        params.print_progress = false;
        params.translate = false;
        params.language = "en"; // Default to English
        params.n_threads = 4; // Use 4 threads
        params.n_max_text_ctx = 16384;
        params.offset_ms = 0;
        params.duration_ms = 0;
        params.single_segment = false;
        params.max_tokens = 0;

        // Speech-specific parameters
        params.thold_pt = 0.01;
        params.thold_ptsum = 0.01;
        params.max_len = 0;
        params.split_on_word = false;
        params.max_tokens = 0;

        // Suppress blanks and non-speech tokens
        params.suppress_blank = true;
        params.suppress_non_speech_tokens = true;

        return WhisperContext{
            .ctx = ctx,
            .params = params,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *WhisperContext) void {
        c.whisper_free(self.ctx);
    }

    pub fn transcribe(self: *WhisperContext, audio_data: []const f32) ![]const u8 {
        if (audio_data.len == 0) {
            return "";
        }

        // Process audio through whisper
        const result = c.whisper_full(
            self.ctx,
            self.params,
            audio_data.ptr,
            @intCast(audio_data.len),
        );

        if (result != 0) {
            return error.TranscriptionFailed;
        }

        // Get number of segments
        const n_segments = c.whisper_full_n_segments(self.ctx);
        if (n_segments == 0) {
            return "";
        }

        // Collect all text segments
        var text_buffer = std.ArrayList(u8).init(self.allocator);
        defer text_buffer.deinit();

        var i: i32 = 0;
        while (i < n_segments) : (i += 1) {
            const segment_text = c.whisper_full_get_segment_text(self.ctx, i);
            if (segment_text != null) {
                const text = std.mem.span(segment_text);
                try text_buffer.appendSlice(text);
                if (i < n_segments - 1) {
                    try text_buffer.append(' ');
                }
            }
        }

        return try text_buffer.toOwnedSlice();
    }

    pub fn setLanguage(self: *WhisperContext, language: []const u8) !void {
        const lang_c = try self.allocator.dupeZ(u8, language);
        defer self.allocator.free(lang_c);
        self.params.language = lang_c.ptr;
    }

    pub fn setTranslate(self: *WhisperContext, translate: bool) void {
        self.params.translate = translate;
    }

    pub fn detectLanguage(self: *WhisperContext, audio_data: []const f32) ![]const u8 {
        if (audio_data.len < 1000) { // Need at least some audio
            return "unknown";
        }

        // Auto-detect language
        self.params.language = "auto";

        const result = c.whisper_full(
            self.ctx,
            self.params,
            audio_data.ptr,
            @intCast(audio_data.len),
        );

        if (result != 0) {
            return "unknown";
        }

        const lang_id = c.whisper_full_lang_id(self.ctx);
        if (lang_id < 0) {
            return "unknown";
        }

        const lang_str = c.whisper_lang_str(lang_id);
        if (lang_str == null) {
            return "unknown";
        }

        return std.mem.span(lang_str);
    }
};

// Helper function to resample audio if needed
pub fn resampleAudio(allocator: std.mem.Allocator, input: []const f32, input_sample_rate: f32, output_sample_rate: f32) ![]f32 {
    if (input_sample_rate == output_sample_rate) {
        // No resampling needed
        const output = try allocator.alloc(f32, input.len);
        @memcpy(output, input);
        return output;
    }

    const ratio = output_sample_rate / input_sample_rate;
    const output_len = @as(usize, @intFromFloat(@as(f32, @floatFromInt(input.len)) * ratio));
    const output = try allocator.alloc(f32, output_len);

    // Simple linear interpolation resampling
    var i: usize = 0;
    while (i < output_len) : (i += 1) {
        const src_idx = @as(f32, @floatFromInt(i)) / ratio;
        const idx = @as(usize, @intFromFloat(@floor(src_idx)));
        const frac = src_idx - @floor(src_idx);

        if (idx + 1 < input.len) {
            output[i] = input[idx] * (1.0 - frac) + input[idx + 1] * frac;
        } else if (idx < input.len) {
            output[i] = input[idx];
        } else {
            output[i] = 0.0;
        }
    }

    return output;
}

// Check if whisper model file exists
pub fn modelExists(model_path: []const u8) bool {
    std.fs.cwd().access(model_path, .{}) catch {
        return false;
    };
    return true;
}

// Download a whisper model
pub fn downloadModel(allocator: std.mem.Allocator, model_name: []const u8, dest_path: []const u8) !void {
    _ = allocator;

    // Model URLs
    const base_url = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/";

    const model_files = std.ComptimeStringMap([]const u8, .{
        .{ "tiny", "ggml-tiny.bin" },
        .{ "base", "ggml-base.bin" },
        .{ "small", "ggml-small.bin" },
        .{ "medium", "ggml-medium.bin" },
        .{ "large", "ggml-large-v3.bin" },
    });

    const model_file = model_files.get(model_name) orelse return error.UnknownModel;

    // Create command to download
    var cmd_buf: [512]u8 = undefined;
    const cmd = try std.fmt.bufPrint(&cmd_buf, "curl -L -o {s} {s}{s}", .{
        dest_path,
        base_url,
        model_file,
    });

    // Execute download
    var child = std.process.Child.init(&.{ "sh", "-c", cmd }, std.heap.page_allocator);
    const result = try child.spawnAndWait();

    if (result != .Exited or result.Exited != 0) {
        return error.DownloadFailed;
    }
}
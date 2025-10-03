const std = @import("std");

// Linux audio backend stub - will be implemented with PulseAudio/ALSA
pub const AudioBackend = struct {
    allocator: std.mem.Allocator,
    is_recording: bool,
    audio_buffer: std.ArrayList(f32),

    pub fn init(allocator: std.mem.Allocator) !AudioBackend {
        return AudioBackend{
            .allocator = allocator,
            .is_recording = false,
            .audio_buffer = std.ArrayList(f32).init(allocator),
        };
    }

    pub fn deinit(self: *AudioBackend) void {
        self.audio_buffer.deinit();
    }

    pub fn startRecording(self: *AudioBackend) !void {
        if (self.is_recording) {
            return error.AlreadyRecording;
        }
        // TODO: Implement with PulseAudio or ALSA
        self.is_recording = true;
        std.debug.print("[Linux Audio] Recording started (stub)\n", .{});
    }

    pub fn stopRecording(self: *AudioBackend) ![]const f32 {
        if (!self.is_recording) {
            return &[_]f32{};
        }
        self.is_recording = false;
        std.debug.print("[Linux Audio] Recording stopped (stub)\n", .{});
        return try self.audio_buffer.toOwnedSlice();
    }

    pub fn getAudioBuffer(self: *AudioBackend) []const f32 {
        return self.audio_buffer.items;
    }

    pub fn requestPermission() !void {
        // Linux typically doesn't require explicit permission
    }

    pub fn checkPermission() bool {
        // Linux typically doesn't require explicit permission
        return true;
    }
};
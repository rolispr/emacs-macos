const std = @import("std");

// Linux TTS backend stub - will be implemented with espeak-ng or piper
pub const TTSBackend = struct {
    allocator: std.mem.Allocator,
    is_speaking: bool,

    pub fn init(allocator: std.mem.Allocator) !TTSBackend {
        return TTSBackend{
            .allocator = allocator,
            .is_speaking = false,
        };
    }

    pub fn deinit(self: *TTSBackend) void {
        if (self.is_speaking) {
            self.stop();
        }
    }

    pub fn speak(self: *TTSBackend, text: []const u8, voice: ?[]const u8) !void {
        _ = voice;
        self.is_speaking = true;
        std.debug.print("[Linux TTS] Speaking: {s} (stub)\n", .{text});
        // TODO: Implement with espeak-ng or piper
    }

    pub fn stop(self: *TTSBackend) void {
        self.is_speaking = false;
        std.debug.print("[Linux TTS] Stopped (stub)\n", .{});
    }

    pub fn pause(self: *TTSBackend) void {
        _ = self;
        std.debug.print("[Linux TTS] Paused (stub)\n", .{});
    }

    pub fn continueSpeaking(self: *TTSBackend) void {
        _ = self;
        std.debug.print("[Linux TTS] Continued (stub)\n", .{});
    }

    pub fn listVoices() ![]const VoiceInfo {
        // TODO: Return actual voice list from espeak-ng
        return &[_]VoiceInfo{};
    }

    pub fn setRate(self: *TTSBackend, rate: f32) void {
        _ = self;
        _ = rate;
    }

    pub fn setPitch(self: *TTSBackend, pitch: f32) void {
        _ = self;
        _ = pitch;
    }

    pub fn setVolume(self: *TTSBackend, volume: f32) void {
        _ = self;
        _ = volume;
    }

    pub const VoiceInfo = struct {
        id: []const u8,
        name: []const u8,
        language: []const u8,
    };
};
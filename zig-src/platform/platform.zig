const std = @import("std");
const builtin = @import("builtin");

pub const Platform = enum {
    macos,
    linux,
    windows,
    unsupported,

    pub fn current() Platform {
        return switch (builtin.os.tag) {
            .macos => .macos,
            .linux => .linux,
            .windows => .windows,
            else => .unsupported,
        };
    }

    pub fn isSupported(self: Platform) bool {
        return self != .unsupported;
    }
};

pub const AudioBackend = union(enum) {
    macos: MacOSAudioBackend,
    linux: LinuxAudioBackend,
    unsupported: void,

    pub fn init(allocator: std.mem.Allocator) !AudioBackend {
        return switch (Platform.current()) {
            .macos => .{ .macos = try MacOSAudioBackend.init(allocator) },
            .linux => .{ .linux = try LinuxAudioBackend.init(allocator) },
            else => .{ .unsupported = {} },
        };
    }

    pub fn deinit(self: *AudioBackend) void {
        switch (self.*) {
            .macos => |*backend| backend.deinit(),
            .linux => |*backend| backend.deinit(),
            .unsupported => {},
        }
    }

    pub fn startRecording(self: *AudioBackend) !void {
        switch (self.*) {
            .macos => |*backend| try backend.startRecording(),
            .linux => |*backend| try backend.startRecording(),
            .unsupported => return error.UnsupportedPlatform,
        }
    }

    pub fn stopRecording(self: *AudioBackend) ![]const f32 {
        switch (self.*) {
            .macos => |*backend| return try backend.stopRecording(),
            .linux => |*backend| return try backend.stopRecording(),
            .unsupported => return &[_]f32{},
        }
    }

    pub fn getAudioBuffer(self: *AudioBackend) []const f32 {
        switch (self.*) {
            .macos => |*backend| return backend.getAudioBuffer(),
            .linux => |*backend| return backend.getAudioBuffer(),
            .unsupported => return &[_]f32{},
        }
    }
};

pub const TTSBackend = union(enum) {
    macos: MacOSTTSBackend,
    linux: LinuxTTSBackend,
    unsupported: void,

    pub fn init(allocator: std.mem.Allocator) !TTSBackend {
        return switch (Platform.current()) {
            .macos => .{ .macos = try MacOSTTSBackend.init(allocator) },
            .linux => .{ .linux = try LinuxTTSBackend.init(allocator) },
            else => .{ .unsupported = {} },
        };
    }

    pub fn deinit(self: *TTSBackend) void {
        switch (self.*) {
            .macos => |*backend| backend.deinit(),
            .linux => |*backend| backend.deinit(),
            .unsupported => {},
        }
    }

    pub fn speak(self: *TTSBackend, text: []const u8, voice: ?[]const u8) !void {
        switch (self.*) {
            .macos => |*backend| try backend.speak(text, voice),
            .linux => |*backend| try backend.speak(text, voice),
            .unsupported => return error.UnsupportedPlatform,
        }
    }

    pub fn stop(self: *TTSBackend) void {
        switch (self.*) {
            .macos => |*backend| backend.stop(),
            .linux => |*backend| backend.stop(),
            .unsupported => {},
        }
    }

    pub fn setRate(self: *TTSBackend, rate: f32) void {
        switch (self.*) {
            .macos => |*backend| backend.setRate(rate),
            .linux => |*backend| backend.setRate(rate),
            .unsupported => {},
        }
    }

    pub fn setPitch(self: *TTSBackend, pitch: f32) void {
        switch (self.*) {
            .macos => |*backend| backend.setPitch(pitch),
            .linux => |*backend| backend.setPitch(pitch),
            .unsupported => {},
        }
    }

    pub fn pause(self: *TTSBackend) void {
        switch (self.*) {
            .macos => |*backend| backend.pause(),
            .linux => |*backend| backend.pause(),
            .unsupported => {},
        }
    }

    pub fn continueSpeaking(self: *TTSBackend) void {
        switch (self.*) {
            .macos => |*backend| backend.continueSpeaking(),
            .linux => |*backend| backend.continueSpeaking(),
            .unsupported => {},
        }
    }
};

pub const MacOSAudioBackend = if (builtin.os.tag == .macos)
    @import("macos/audio.zig").AudioBackend
else
    struct {
        pub fn init(allocator: std.mem.Allocator) !@This() {
            _ = allocator;
            return @This(){};
        }
        pub fn deinit(self: *@This()) void {
            _ = self;
        }
    };

pub const LinuxAudioBackend = if (builtin.os.tag == .linux)
    @import("linux/audio.zig").AudioBackend
else
    struct {
        allocator: std.mem.Allocator,
        is_recording: bool,
        audio_buffer: std.ArrayList(f32),

        pub fn init(allocator: std.mem.Allocator) !@This() {
            return @This(){
                .allocator = allocator,
                .is_recording = false,
                .audio_buffer = std.ArrayList(f32).init(allocator),
            };
        }
        pub fn deinit(self: *@This()) void {
            self.audio_buffer.deinit();
        }
        pub fn startRecording(self: *@This()) !void {
            _ = self;
        }
        pub fn stopRecording(self: *@This()) ![]const f32 {
            _ = self;
            return &[_]f32{};
        }
        pub fn getAudioBuffer(self: *@This()) []const f32 {
            _ = self;
            return &[_]f32{};
        }
    };

pub const MacOSTTSBackend = if (builtin.os.tag == .macos)
    @import("macos/tts.zig").TTSBackend
else
    struct {
        pub fn init(allocator: std.mem.Allocator) !@This() {
            _ = allocator;
            return @This(){};
        }
        pub fn deinit(self: *@This()) void {
            _ = self;
        }
        pub fn speak(self: *@This(), text: []const u8, voice: ?[]const u8) !void {
            _ = self;
            _ = text;
            _ = voice;
        }
        pub fn stop(self: *@This()) void {
            _ = self;
        }
        pub fn setRate(self: *@This(), rate: f32) void {
            _ = self;
            _ = rate;
        }
        pub fn setPitch(self: *@This(), pitch: f32) void {
            _ = self;
            _ = pitch;
        }
        pub fn pause(self: *@This()) void {
            _ = self;
        }
        pub fn continueSpeaking(self: *@This()) void {
            _ = self;
        }
    };

pub const LinuxTTSBackend = if (builtin.os.tag == .linux)
    @import("linux/tts.zig").TTSBackend
else
    struct {
        pub fn init(allocator: std.mem.Allocator) !@This() {
            _ = allocator;
            return @This(){};
        }
        pub fn deinit(self: *@This()) void {
            _ = self;
        }
        pub fn speak(self: *@This(), text: []const u8, voice: ?[]const u8) !void {
            _ = self;
            _ = text;
            _ = voice;
        }
        pub fn stop(self: *@This()) void {
            _ = self;
        }
        pub fn setRate(self: *@This(), rate: f32) void {
            _ = self;
            _ = rate;
        }
        pub fn setPitch(self: *@This(), pitch: f32) void {
            _ = self;
            _ = pitch;
        }
        pub fn pause(self: *@This()) void {
            _ = self;
        }
        pub fn continueSpeaking(self: *@This()) void {
            _ = self;
        }
    };

// STT Backend
pub const STTBackend = union(enum) {
    macos: MacOSSTTBackend,
    linux: LinuxSTTBackend,
    unsupported: void,

    pub fn init(allocator: std.mem.Allocator) !STTBackend {
        return switch (Platform.current()) {
            .macos => .{ .macos = try MacOSSTTBackend.init(allocator) },
            .linux => .{ .linux = try LinuxSTTBackend.init(allocator) },
            else => .{ .unsupported = {} },
        };
    }

    pub fn deinit(self: *STTBackend) void {
        switch (self.*) {
            .macos => |*backend| backend.deinit(),
            .linux => |*backend| backend.deinit(),
            .unsupported => {},
        }
    }

    pub fn startRecording(self: *STTBackend) !void {
        switch (self.*) {
            .macos => |*backend| try backend.startRecording(),
            .linux => |*backend| try backend.startRecording(),
            .unsupported => return error.UnsupportedPlatform,
        }
    }

    pub fn stopRecording(self: *STTBackend) !void {
        switch (self.*) {
            .macos => |*backend| try backend.stopRecording(),
            .linux => |*backend| try backend.stopRecording(),
            .unsupported => return error.UnsupportedPlatform,
        }
    }

    pub fn getCurrentText(self: *STTBackend) []const u8 {
        switch (self.*) {
            .macos => |*backend| return backend.getCurrentText(),
            .linux => |*backend| return backend.getCurrentText(),
            .unsupported => return "",
        }
    }

    pub fn processBufferedAudio(self: *STTBackend) void {
        switch (self.*) {
            .macos => |*backend| backend.processBufferedAudio(),
            .linux => |*backend| backend.processBufferedAudio(),
            .unsupported => {},
        }
    }

    pub fn clearText(self: *STTBackend) void {
        switch (self.*) {
            .macos => |*backend| backend.clearText(),
            .linux => |*backend| backend.clearText(),
            .unsupported => {},
        }
    }
};

pub const MacOSSTTBackend = if (builtin.os.tag == .macos)
    @import("macos/stt.zig").STTBackend
else
    struct {
        pub fn init(allocator: std.mem.Allocator) !@This() {
            _ = allocator;
            return @This(){};
        }
        pub fn deinit(self: *@This()) void {
            _ = self;
        }
        pub fn startRecording(self: *@This()) !void {
            _ = self;
        }
        pub fn stopRecording(self: *@This()) !void {
            _ = self;
        }
        pub fn getCurrentText(self: *@This()) []const u8 {
            _ = self;
            return "";
        }
    };

pub const LinuxSTTBackend = struct {
    pub fn init(allocator: std.mem.Allocator) !@This() {
        _ = allocator;
        return @This(){};
    }
    pub fn deinit(self: *@This()) void {
        _ = self;
    }
    pub fn startRecording(self: *@This()) !void {
        _ = self;
    }
    pub fn stopRecording(self: *@This()) !void {
        _ = self;
    }
    pub fn getCurrentText(self: *@This()) []const u8 {
        _ = self;
        return "";
    }
    pub fn processBufferedAudio(self: *@This()) void {
        _ = self;
    }
    pub fn clearText(self: *@This()) void {
        _ = self;
    }
};
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const voice_module = b.addSharedLibrary(.{
        .name = "zig-voice",
        .root_source_file = b.path("zig-src/main.zig"),
        .target = target,
        .optimize = optimize,
        .version = .{ .major = 0, .minor = 1, .patch = 0 },
    });

    // Platform-specific configuration
    switch (target.result.os.tag) {
        .macos => {
            voice_module.linkFramework("Foundation");
            voice_module.linkFramework("AVFoundation");
            voice_module.linkFramework("CoreAudio");
            voice_module.linkFramework("AudioToolbox");
            voice_module.linkFramework("Accelerate");
        },
        .linux => {
            voice_module.linkSystemLibrary("pthread");
        },
        else => {},
    }

    voice_module.linkLibC();

    b.installArtifact(voice_module);
}
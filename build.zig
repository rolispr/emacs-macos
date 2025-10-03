const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Download whisper model if it doesn't exist
    const download_model = b.addSystemCommand(&.{
        "sh", "-c",
        \\if [ ! -f models/ggml-base.en.bin ]; then
        \\  echo "[Vox] Downloading Whisper model..."
        \\  mkdir -p models
        \\  curl -L --progress-bar https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin -o models/ggml-base.en.bin
        \\  echo "[Vox] Model downloaded successfully"
        \\fi
    });

    // Get dependencies
    const zig_emacs = b.dependency("zig-emacs", .{
        .target = target,
        .optimize = optimize,
    });

    const whisper = b.dependency("whisper_cpp", .{
        .target = target,
        .optimize = optimize,
    });

    // Build whisper.cpp with CMake - CPU only, no problematic backends
    const build_whisper = b.addSystemCommand(&.{
        "sh", "-c", b.fmt(
            \\cd {s} && \
            \\rm -rf build && \
            \\mkdir -p build && \
            \\cd build && \
            \\cmake .. \
            \\  -DCMAKE_BUILD_TYPE=Release \
            \\  -DBUILD_SHARED_LIBS=OFF \
            \\  -DGGML_METAL=OFF \
            \\  -DGGML_CUDA=OFF \
            \\  -DGGML_BLAS=OFF \
            \\  -DGGML_ACCELERATE=OFF \
            \\  -DGGML_OPENMP=OFF \
            \\  -DWHISPER_BUILD_TESTS=OFF \
            \\  -DWHISPER_BUILD_EXAMPLES=OFF && \
            \\make -j4 whisper ggml
        , .{whisper.path("").getPath(b)})
    });

    const voice_module = b.addSharedLibrary(.{
        .name = "vox",
        .root_source_file = b.path("zig-src/main.zig"),
        .target = target,
        .optimize = optimize,
        .version = .{ .major = 0, .minor = 1, .patch = 0 },
    });

    // Add modules
    voice_module.root_module.addImport("emacs", zig_emacs.module("emacs"));

    // Add whisper include paths
    voice_module.addIncludePath(whisper.path("include"));
    voice_module.addIncludePath(whisper.path("ggml/include"));

    // Link to the CMake-built whisper library
    voice_module.addObjectFile(whisper.path("build/src/libwhisper.a"));
    voice_module.addObjectFile(whisper.path("build/ggml/src/libggml.a"));
    voice_module.addObjectFile(whisper.path("build/ggml/src/libggml-base.a"));
    voice_module.addObjectFile(whisper.path("build/ggml/src/libggml-cpu.a"));

    // Platform-specific configuration
    switch (target.result.os.tag) {
        .macos => {
            voice_module.linkFramework("Foundation");
            voice_module.linkFramework("AVFoundation");
            voice_module.linkFramework("CoreAudio");
            voice_module.linkFramework("AudioToolbox");
        },
        .linux => {
            voice_module.linkSystemLibrary("pthread");
        },
        else => {},
    }

    voice_module.linkLibC();
    voice_module.linkLibCpp();

    // Make voice_module depend on model download and whisper build
    voice_module.step.dependOn(&download_model.step);
    voice_module.step.dependOn(&build_whisper.step);

    b.installArtifact(voice_module);
}
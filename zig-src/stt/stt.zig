const std = @import("std");
const emacs = @import("emacs");
const platform = @import("../platform/platform.zig");

// Global state for recording
var is_recording = false;
var current_transcription: []const u8 = "";
var audio_backend: ?platform.AudioBackend = null;
var stt_mutex = std.Thread.Mutex{};

fn ensureAudioBackend() !*platform.AudioBackend {
    stt_mutex.lock();
    defer stt_mutex.unlock();

    if (audio_backend == null) {
        audio_backend = try platform.AudioBackend.init(std.heap.page_allocator);
    }
    return &audio_backend.?;
}

// Permission functions
pub fn requestPermission(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    // TODO: Request actual OS permission
    std.debug.print("[STT] Requesting speech recognition permission\n", .{});

    return try env.makeString("Requesting speech recognition permission... Check for system dialog");
}

pub fn checkPermission(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    // TODO: Check actual permission status
    // For now, return "not-determined"
    return try env.makeString("not-determined");
}

// Recording functions
pub fn startRecording(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    if (is_recording) {
        return env.signalError("already-recording", "Recording is already in progress");
    }

    const backend = ensureAudioBackend() catch {
        return env.signalError("audio-error", "Failed to initialize audio backend");
    };

    backend.startRecording() catch {
        return env.signalError("audio-error", "Failed to start recording");
    };

    is_recording = true;
    current_transcription = "";

    return try env.makeString("Recording started");
}

pub fn stopRecording(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    if (!is_recording) {
        return try env.makeString("");
    }

    const backend = ensureAudioBackend() catch {
        is_recording = false;
        return try env.makeString("");
    };

    const audio_data = backend.stopRecording() catch {
        is_recording = false;
        return try env.makeString("");
    };
    defer std.heap.page_allocator.free(audio_data);

    is_recording = false;

    // TODO: Process audio through speech recognition
    // For now, return a placeholder
    return try env.makeString("Test transcription from Zig audio backend");
}

// Status functions
pub fn getStatus(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    if (is_recording) {
        return try env.makeString("recording");
    }

    return try env.makeString("idle");
}

pub fn isAvailable(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    // TODO: Check actual availability
    return env.makeT();
}

pub fn getCurrentTranscription(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    if (!is_recording) {
        return try env.makeString("");
    }

    // TODO: Return actual partial transcription
    return try env.makeString(current_transcription);
}

// Whisper functions
var whisper_recording = false;
var whisper_audio_buffer: std.ArrayList(f32) = undefined;

pub fn whisperStart(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    if (whisper_recording) {
        return env.signalError("already-recording", "Whisper recording already in progress");
    }

    whisper_recording = true;
    whisper_audio_buffer = std.ArrayList(f32).init(std.heap.page_allocator);

    // TODO: Start actual audio recording
    std.debug.print("[Whisper] Starting recording\n", .{});

    return try env.makeString("Recording started");
}

pub fn whisperTranscribe(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    if (args.len != 1) {
        return env.wrongTypeArgument();
    }

    if (!whisper_recording) {
        return env.signalError("not-recording", "No recording in progress");
    }

    const api_key = try env.getString(args[0]);
    defer env.allocator.free(api_key);

    whisper_recording = false;
    defer whisper_audio_buffer.deinit();

    // TODO: Process audio through Whisper
    std.debug.print("[Whisper] Transcribing with API key\n", .{});

    return try env.makeString("Whisper transcription placeholder");
}
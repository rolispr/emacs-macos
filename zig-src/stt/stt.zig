const std = @import("std");
const emacs_api = @import("../emacs_api.zig");

// Global state for recording
var is_recording = false;
var current_transcription: []const u8 = "";

// Permission functions
pub fn requestPermission(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = nargs;
    _ = args;
    _ = data;

    // TODO: Request actual OS permission
    std.debug.print("[STT] Requesting speech recognition permission\n", .{});

    return emacs_api.makeString(env, "Requesting speech recognition permission... Check for system dialog");
}

pub fn checkPermission(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = nargs;
    _ = args;
    _ = data;

    // TODO: Check actual permission status
    // For now, return "not-determined"
    return emacs_api.makeString(env, "not-determined");
}

// Recording functions
pub fn startRecording(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = nargs;
    _ = args;
    _ = data;

    if (is_recording) {
        return emacs_api.signalError(env, "already-recording", "Recording is already in progress");
    }

    is_recording = true;
    current_transcription = "";

    // TODO: Start actual recording
    std.debug.print("[STT] Starting recording\n", .{});

    return emacs_api.makeString(env, "Recording started");
}

pub fn stopRecording(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = nargs;
    _ = args;
    _ = data;

    if (!is_recording) {
        return emacs_api.makeString(env, "");
    }

    is_recording = false;

    // TODO: Stop recording and return actual transcription
    std.debug.print("[STT] Stopping recording\n", .{});

    return emacs_api.makeString(env, "Test transcription from Zig");
}

// Status functions
pub fn getStatus(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = nargs;
    _ = args;
    _ = data;

    if (is_recording) {
        return emacs_api.makeString(env, "recording");
    }

    return emacs_api.makeString(env, "idle");
}

pub fn isAvailable(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = nargs;
    _ = args;
    _ = data;

    // TODO: Check actual availability
    return emacs_api.makeT(env);
}

pub fn getCurrentTranscription(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = nargs;
    _ = args;
    _ = data;

    if (!is_recording) {
        return emacs_api.makeString(env, "");
    }

    // TODO: Return actual partial transcription
    return emacs_api.makeString(env, current_transcription);
}

// Whisper functions
var whisper_recording = false;
var whisper_audio_buffer: std.ArrayList(f32) = undefined;

pub fn whisperStart(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = nargs;
    _ = args;
    _ = data;

    if (whisper_recording) {
        return emacs_api.signalError(env, "already-recording", "Whisper recording already in progress");
    }

    whisper_recording = true;
    whisper_audio_buffer = std.ArrayList(f32).init(std.heap.page_allocator);

    // TODO: Start actual audio recording
    std.debug.print("[Whisper] Starting recording\n", .{});

    return emacs_api.makeString(env, "Recording started");
}

pub fn whisperTranscribe(env: *emacs_api.EmacsEnv, nargs: isize, args: [*c]emacs_api.EmacsValue, data: ?*anyopaque) callconv(.C) emacs_api.EmacsValue {
    _ = data;
    if (nargs != 1) {
        return emacs_api.signalError(env, "wrong-number-of-arguments", "whisper-transcribe requires 1 argument");
    }

    if (!whisper_recording) {
        return emacs_api.signalError(env, "not-recording", "No recording in progress");
    }

    const allocator = std.heap.page_allocator;
    const api_key = emacs_api.extractString(env, args[0], allocator) catch {
        return emacs_api.signalError(env, "invalid-argument", "Invalid API key argument");
    };
    defer allocator.free(api_key);

    whisper_recording = false;
    defer whisper_audio_buffer.deinit();

    // TODO: Process audio through Whisper
    std.debug.print("[Whisper] Transcribing with API key\n", .{});

    return emacs_api.makeString(env, "Whisper transcription placeholder");
}
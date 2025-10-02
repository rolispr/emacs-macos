const std = @import("std");
const emacs_api = @import("emacs_api.zig");
const tts = @import("tts/tts.zig");
const stt = @import("stt/stt.zig");

// Module initialization - required by Emacs
export fn plugin_is_GPL_compatible() c_int {
    return 0;
}

export fn emacs_module_init(runtime: *emacs_api.EmacsRuntime) c_int {
    const env = runtime.get_environment.?(runtime) orelse return 1;

    // Register all functions to match Swift module API
    registerTTSFunctions(env) catch return 2;
    registerSTTFunctions(env) catch return 2;

    return 0;
}

fn registerTTSFunctions(env: *emacs_api.EmacsEnv) !void {
    // Basic TTS
    try emacs_api.registerFunction(env, "macmod/speak", 1, 1, tts.speak,
        "Speak text using system voice");

    try emacs_api.registerFunction(env, "macmod/speak-with-voice", 2, 2, tts.speakWithVoice,
        "Speak text using specific system voice");

    try emacs_api.registerFunction(env, "macmod/speak-advanced", 4, 4, tts.speakAdvanced,
        "Speak text with voice, rate, and pitch control");

    // TTS Control
    try emacs_api.registerFunction(env, "macmod/stop-speech", 0, 0, tts.stopSpeech,
        "Stop speech synthesis");

    try emacs_api.registerFunction(env, "macmod/pause-speech", 0, 0, tts.pauseSpeech,
        "Pause speech synthesis");

    try emacs_api.registerFunction(env, "macmod/continue-speech", 0, 0, tts.continueSpeech,
        "Continue paused speech");

    // Voice Management
    try emacs_api.registerFunction(env, "macmod/list-voices", 0, 0, tts.listVoices,
        "List available system voices");

    try emacs_api.registerFunction(env, "macmod/get-suggested-voices", 0, 0, tts.getSuggestedVoices,
        "Get suggested voice identifiers");

    // OpenAI TTS
    try emacs_api.registerFunction(env, "macmod/speak-openai", 3, 3, tts.speakOpenAI,
        "Synthesize and speak text using OpenAI TTS");

    try emacs_api.registerFunction(env, "macmod/stop-openai", 0, 0, tts.stopOpenAI,
        "Stop any ongoing OpenAI speech synthesis");
}

fn registerSTTFunctions(env: *emacs_api.EmacsEnv) !void {
    // Permissions
    try emacs_api.registerFunction(env, "macmod/request-speech-permission", 0, 0, stt.requestPermission,
        "Request speech recognition permission");

    try emacs_api.registerFunction(env, "macmod/check-speech-permission", 0, 0, stt.checkPermission,
        "Check current speech recognition permission status");

    // Local STT
    try emacs_api.registerFunction(env, "macmod/start-recording", 0, 0, stt.startRecording,
        "Start recording with speech recognition");

    try emacs_api.registerFunction(env, "macmod/stop-recording", 0, 0, stt.stopRecording,
        "Stop recording and get transcription");

    try emacs_api.registerFunction(env, "macmod/get-recognition-status", 0, 0, stt.getStatus,
        "Get speech recognition status");

    try emacs_api.registerFunction(env, "macmod/is-speech-available", 0, 0, stt.isAvailable,
        "Check if speech recognition is available");

    try emacs_api.registerFunction(env, "macmod/get-current-transcription", 0, 0, stt.getCurrentTranscription,
        "Get current partial transcription while recording");

    // Whisper STT
    try emacs_api.registerFunction(env, "macmod/whisper-start", 0, 0, stt.whisperStart,
        "Start recording audio for Whisper transcription");

    try emacs_api.registerFunction(env, "macmod/whisper-transcribe", 1, 1, stt.whisperTranscribe,
        "Stop recording and transcribe via Whisper API");
}
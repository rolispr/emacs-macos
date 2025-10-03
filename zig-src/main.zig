const std = @import("std");
const emacs = @import("emacs");
const platform = @import("platform/platform.zig");

var tts_backend: ?platform.TTSBackend = null;
var tts_mutex = std.Thread.Mutex{};
var stt_backend: ?platform.STTBackend = null;
var stt_mutex = std.Thread.Mutex{};
var stt_initialized = false;

const Module = struct {
    pub fn init(env: emacs.Env) c_int {
        // Initialize TTS backend
        tts_backend = platform.TTSBackend.init(std.heap.c_allocator) catch {
            std.debug.print("[TTS] Failed to initialize TTS backend\n", .{});
            return 1;
        };

        // STT backend will be initialized on first use

        // Register TTS functions
        env.makeFunction("vox-speak", speak, .{ .doc_string = "Speak text using system voice" });
        env.makeFunction("vox-speak-with-voice", speakWithVoice, .{ .doc_string = "Speak text using specific system voice" });
        env.makeFunction("vox-stop", stopSpeech, .{ .doc_string = "Stop speech synthesis" });
        env.makeFunction("vox-list-voices", listVoices, .{ .doc_string = "List available system voices" });

        // Register STT functions
        env.makeFunction("vox-start-recording", startRecording, .{ .doc_string = "Start speech recognition" });
        env.makeFunction("vox-stop-recording", stopRecording, .{ .doc_string = "Stop speech recognition" });
        env.makeFunction("vox-get-recognition-text", getRecognitionText, .{ .doc_string = "Get current recognized text" });
        env.makeFunction("vox-clear-recognition-text", clearRecognitionText, .{ .doc_string = "Clear recognized text buffer" });
        env.makeFunction("vox-process-audio", processAudio, .{ .doc_string = "Process buffered audio with whisper" });
        env.makeFunction("vox-request-permission", requestPermission, .{ .doc_string = "Request speech recognition permission" });

        return 0;
    }

    fn speak(env: emacs.Env, text_value: emacs.Value) emacs.Value {
        const allocator = std.heap.c_allocator;
        const text = env.extractString(allocator, text_value) catch return env.nil;
        defer allocator.free(text);

        tts_mutex.lock();
        defer tts_mutex.unlock();

        if (tts_backend) |*backend| {
            backend.speak(text, null) catch {
                std.debug.print("[TTS] Failed to speak text\n", .{});
                return env.nil;
            };
        }

        return env.t;
    }

    fn speakWithVoice(env: emacs.Env, text_value: emacs.Value, voice_value: emacs.Value) emacs.Value {
        const allocator = std.heap.c_allocator;
        const text = env.extractString(allocator, text_value) catch return env.nil;
        defer allocator.free(text);
        const voice = env.extractString(allocator, voice_value) catch return env.nil;
        defer allocator.free(voice);

        tts_mutex.lock();
        defer tts_mutex.unlock();

        if (tts_backend) |*backend| {
            backend.speak(text, voice) catch {
                std.debug.print("[TTS] Failed to speak text with voice\n", .{});
                return env.nil;
            };
        }

        return env.t;
    }

    fn stopSpeech(env: emacs.Env) emacs.Value {
        tts_mutex.lock();
        defer tts_mutex.unlock();

        if (tts_backend) |*backend| {
            backend.stop();
        }

        return env.t;
    }

    fn listVoices(env: emacs.Env) emacs.Value {
        // For now, return a simple list of voice IDs
        // TODO: Implement full voice listing once platform layer supports it
        return env.nil;
    }

    // STT Functions
    fn startRecording(env: emacs.Env) emacs.Value {
        std.debug.print("[STT] startRecording called\n", .{});
        stt_mutex.lock();
        defer stt_mutex.unlock();

        // Lazy initialize STT backend on first use
        if (!stt_initialized) {
            std.debug.print("[STT] Initializing STT backend on first use\n", .{});
            if (platform.STTBackend.init(std.heap.c_allocator)) |backend| {
                stt_backend = backend;
                std.debug.print("[STT] Backend initialized successfully\n", .{});
            } else |err| {
                std.debug.print("[STT] Failed to initialize STT backend: {}\n", .{err});
                stt_backend = null;
            }
            stt_initialized = true;
        }

        if (stt_backend) |*backend| {
            std.debug.print("[STT] Backend exists, calling startRecording\n", .{});
            backend.startRecording() catch |err| {
                std.debug.print("[STT] Failed to start recording: {}\n", .{err});
                return env.nil;
            };
            std.debug.print("[STT] Recording started successfully\n", .{});
            return env.t;
        }

        std.debug.print("[STT] No backend available\n", .{});
        return env.nil;
    }

    fn stopRecording(env: emacs.Env) emacs.Value {
        stt_mutex.lock();
        defer stt_mutex.unlock();

        if (stt_backend) |*backend| {
            backend.stopRecording() catch {
                std.debug.print("[STT] Failed to stop recording\n", .{});
                return env.nil;
            };
            return env.t;
        }

        return env.nil;
    }

    fn getRecognitionText(env: emacs.Env) emacs.Value {
        stt_mutex.lock();
        defer stt_mutex.unlock();

        if (stt_backend) |*backend| {
            const text = backend.getCurrentText();
            if (text.len > 0) {
                const str = std.heap.page_allocator.dupeZ(u8, text) catch return env.nil;
                defer std.heap.page_allocator.free(str);
                return env.makeString(str);
            }
        }

        return env.makeString("");
    }

    fn clearRecognitionText(env: emacs.Env) emacs.Value {
        stt_mutex.lock();
        defer stt_mutex.unlock();

        if (stt_backend) |*backend| {
            backend.clearText();
            return env.t;
        }

        return env.nil;
    }

    fn processAudio(env: emacs.Env) emacs.Value {
        stt_mutex.lock();
        defer stt_mutex.unlock();

        if (stt_backend) |*backend| {
            backend.processBufferedAudio();
            return env.t;
        }

        return env.nil;
    }

    fn requestPermission(env: emacs.Env) emacs.Value {
        if (@hasDecl(platform.MacOSSTTBackend, "requestAuthorization")) {
            platform.MacOSSTTBackend.requestAuthorization();
            return env.t;
        }
        return env.nil;
    }
};

comptime {
    emacs.module_init(Module);
}
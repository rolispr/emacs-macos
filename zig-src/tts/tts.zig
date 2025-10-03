const std = @import("std");
const emacs = @import("emacs");
const platform = @import("../platform/platform.zig");

// Global TTS backend instance
var tts_backend: ?platform.TTSBackend = null;
var tts_mutex = std.Thread.Mutex{};

fn ensureTTSBackend() !*platform.TTSBackend {
    tts_mutex.lock();
    defer tts_mutex.unlock();

    if (tts_backend == null) {
        tts_backend = try platform.TTSBackend.init(std.heap.page_allocator);
    }
    return &tts_backend.?;
}

pub fn speak(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    if (args.len != 1) {
        return env.wrongTypeArgument();
    }

    const text = try env.getString(args[0]);
    defer env.allocator.free(text);

    const backend = ensureTTSBackend() catch {
        return env.signalError("tts-error", "Failed to initialize TTS backend");
    };

    backend.speak(text, null) catch {
        return env.signalError("tts-error", "Failed to speak text");
    };

    return env.makeT();
}

pub fn speakWithVoice(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    if (args.len != 2) {
        return env.wrongTypeArgument();
    }

    const text = try env.getString(args[0]);
    defer env.allocator.free(text);

    const voice = try env.getString(args[1]);
    defer env.allocator.free(voice);

    const backend = ensureTTSBackend() catch {
        return env.signalError("tts-error", "Failed to initialize TTS backend");
    };

    backend.speak(text, voice) catch {
        return env.signalError("tts-error", "Failed to speak text");
    };

    return env.makeT();
}

pub fn speakAdvanced(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    if (args.len != 4) {
        return env.wrongTypeArgument();
    }

    const text = try env.getString(args[0]);
    defer env.allocator.free(text);

    const voice = try env.getString(args[1]);
    defer env.allocator.free(voice);

    const rate = try env.getFloat(args[2]);
    const pitch = try env.getFloat(args[3]);

    const backend = ensureTTSBackend() catch {
        return env.signalError("tts-error", "Failed to initialize TTS backend");
    };

    // Set parameters
    backend.setRate(@floatCast(rate));
    backend.setPitch(@floatCast(pitch));

    backend.speak(text, voice) catch {
        return env.signalError("tts-error", "Failed to speak text");
    };

    return env.makeT();
}

pub fn stopSpeech(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    const backend = ensureTTSBackend() catch {
        return env.makeNil();
    };

    backend.stop();

    return env.makeNil();
}

pub fn pauseSpeech(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    const backend = ensureTTSBackend() catch {
        return env.makeNil();
    };

    backend.pause();

    return env.makeNil();
}

pub fn continueSpeech(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    const backend = ensureTTSBackend() catch {
        return env.makeNil();
    };

    backend.continueSpeaking();

    return env.makeNil();
}

pub fn listVoices(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    const backend = ensureTTSBackend() catch {
        return env.makeNil();
    };

    const voices = backend.listVoices() catch {
        return env.makeNil();
    };
    defer std.heap.page_allocator.free(voices);

    // Convert to Emacs list
    var list = env.makeNil();
    var i = voices.len;
    while (i > 0) {
        i -= 1;
        const voice_info = voices[i];

        // Create association list for voice info
        const id_str = try env.makeString(voice_info.id);
        const name_str = try env.makeString(voice_info.name);
        const lang_str = try env.makeString(voice_info.language);

        const id_cons = try env.cons(try env.intern("id"), id_str);
        const name_cons = try env.cons(try env.intern("name"), name_str);
        const lang_cons = try env.cons(try env.intern("language"), lang_str);

        var voice_alist = try env.cons(lang_cons, env.makeNil());
        voice_alist = try env.cons(name_cons, voice_alist);
        voice_alist = try env.cons(id_cons, voice_alist);

        list = try env.cons(voice_alist, list);
    }

    return list;
}

pub fn getSuggestedVoices(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    // Return a dictionary of suggested voices
    const en_us = try env.makeString("com.apple.voice.enhanced.en-US.Samantha");
    const en_gb = try env.makeString("com.apple.voice.enhanced.en-GB.Daniel");

    const en_us_cons = try env.cons(try env.intern("en-US"), en_us);
    const en_gb_cons = try env.cons(try env.intern("en-GB"), en_gb);

    var dict = env.makeNil();
    dict = try env.cons(en_gb_cons, dict);
    dict = try env.cons(en_us_cons, dict);

    return dict;
}

pub fn speakOpenAI(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    if (args.len != 3) {
        return env.wrongTypeArgument();
    }

    const text = try env.getString(args[0]);
    defer env.allocator.free(text);

    const api_key = try env.getString(args[1]);
    defer env.allocator.free(api_key);

    const voice = try env.getString(args[2]);
    defer env.allocator.free(voice);

    // TODO: Implement OpenAI TTS
    std.debug.print("[TTS] OpenAI speak with voice {s}\n", .{voice});

    return env.makeNil();
}

pub fn stopOpenAI(env: *emacs.Env, args: []const emacs.Value) emacs.Error!emacs.Value {
    _ = args;

    // TODO: Stop OpenAI synthesis
    std.debug.print("[TTS] Stopping OpenAI speech\n", .{});

    return env.makeNil();
}
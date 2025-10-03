const std = @import("std");
const objc = @cImport({
    @cInclude("objc/objc.h");
    @cInclude("objc/runtime.h");
    @cInclude("objc/message.h");
});

// Objective-C runtime helpers
fn getClass(name: [*c]const u8) objc.Class {
    return objc.objc_getClass(name).?;
}

fn sel(name: [*c]const u8) objc.SEL {
    return objc.sel_registerName(name).?;
}

fn msgSend(comptime ReturnType: type, receiver: anytype, selector: objc.SEL, args: anytype) ReturnType {
    const msg_send = @as(*const fn (@TypeOf(receiver), objc.SEL) callconv(.C) ReturnType, @ptrCast(&objc.objc_msgSend));
    _ = args;
    return msg_send(receiver, selector);
}

fn msgSendWithArg(comptime ReturnType: type, receiver: anytype, selector: objc.SEL, arg: anytype) ReturnType {
    const msg_send = @as(*const fn (@TypeOf(receiver), objc.SEL, @TypeOf(arg)) callconv(.C) ReturnType, @ptrCast(&objc.objc_msgSend));
    return msg_send(receiver, selector, arg);
}

fn msgSendWithArgs(comptime ReturnType: type, receiver: anytype, selector: objc.SEL, arg1: anytype, arg2: anytype) ReturnType {
    const msg_send = @as(*const fn (@TypeOf(receiver), objc.SEL, @TypeOf(arg1), @TypeOf(arg2)) callconv(.C) ReturnType, @ptrCast(&objc.objc_msgSend));
    return msg_send(receiver, selector, arg1, arg2);
}

pub const TTSBackend = struct {
    allocator: std.mem.Allocator,
    synthesizer: objc.id,
    current_utterance: ?objc.id,
    is_speaking: bool,
    rate: f32,
    pitch: f32,
    volume: f32,

    pub fn init(allocator: std.mem.Allocator) !TTSBackend {
        // Create AVSpeechSynthesizer
        const AVSpeechSynthesizer = getClass("AVSpeechSynthesizer");
        const alloc_sel = sel("alloc");
        const init_sel = sel("init");

        const synthesizer_alloc = msgSend(objc.id, AVSpeechSynthesizer, alloc_sel, {});
        const synthesizer = msgSend(objc.id, synthesizer_alloc, init_sel, {});

        if (synthesizer == null) {
            return error.SynthesizerCreationFailed;
        }

        return TTSBackend{
            .allocator = allocator,
            .synthesizer = synthesizer,
            .current_utterance = null,
            .is_speaking = false,
            .rate = 0.5, // Default rate
            .pitch = 1.0, // Default pitch
            .volume = 1.0, // Default volume
        };
    }

    pub fn deinit(self: *TTSBackend) void {
        if (self.is_speaking) {
            self.stop();
        }

        // Release the synthesizer
        if (self.synthesizer != null) {
            _ = msgSend(void, self.synthesizer, sel("release"), {});
        }
    }

    pub fn speak(self: *TTSBackend, text: []const u8, voice: ?[]const u8) !void {
        // Create NSString from text
        const NSString = getClass("NSString");
        const text_cstr = try self.allocator.dupeZ(u8, text);
        defer self.allocator.free(text_cstr);

        const ns_text = msgSendWithArg(
            objc.id,
            NSString,
            sel("stringWithUTF8String:"),
            text_cstr.ptr,
        );

        if (ns_text == null) {
            return error.StringCreationFailed;
        }

        // Create AVSpeechUtterance
        const AVSpeechUtterance = getClass("AVSpeechUtterance");
        const utterance = msgSendWithArg(
            objc.id,
            AVSpeechUtterance,
            sel("speechUtteranceWithString:"),
            ns_text,
        );

        if (utterance == null) {
            return error.UtteranceCreationFailed;
        }

        // Set rate, pitch, and volume
        msgSendWithArg(void, utterance, sel("setRate:"), self.rate);
        msgSendWithArg(void, utterance, sel("setPitchMultiplier:"), self.pitch);
        msgSendWithArg(void, utterance, sel("setVolume:"), self.volume);

        // Set voice if specified
        if (voice) |voice_id| {
            const voice_cstr = try self.allocator.dupeZ(u8, voice_id);
            defer self.allocator.free(voice_cstr);

            const ns_voice_id = msgSendWithArg(
                objc.id,
                NSString,
                sel("stringWithUTF8String:"),
                voice_cstr.ptr,
            );

            const AVSpeechSynthesisVoice = getClass("AVSpeechSynthesisVoice");
            const voice_obj = msgSendWithArg(
                objc.id,
                AVSpeechSynthesisVoice,
                sel("voiceWithIdentifier:"),
                ns_voice_id,
            );

            if (voice_obj != null) {
                msgSendWithArg(void, utterance, sel("setVoice:"), voice_obj);
            }
        }

        // Store current utterance
        self.current_utterance = utterance;
        self.is_speaking = true;

        // Speak the utterance
        msgSendWithArg(void, self.synthesizer, sel("speakUtterance:"), utterance);
    }

    pub fn stop(self: *TTSBackend) void {
        if (self.is_speaking) {
            // Stop at immediate boundary
            const AVSpeechBoundary_Immediate: c_long = 0;
            _ = msgSendWithArg(
                bool,
                self.synthesizer,
                sel("stopSpeakingAtBoundary:"),
                AVSpeechBoundary_Immediate,
            );
            self.is_speaking = false;
            self.current_utterance = null;
        }
    }

    pub fn pause(self: *TTSBackend) void {
        if (self.is_speaking) {
            const AVSpeechBoundary_Immediate: c_long = 0;
            _ = msgSendWithArg(
                bool,
                self.synthesizer,
                sel("pauseSpeakingAtBoundary:"),
                AVSpeechBoundary_Immediate,
            );
        }
    }

    pub fn continueSpeaking(self: *TTSBackend) void {
        _ = msgSend(bool, self.synthesizer, sel("continueSpeaking"), {});
    }

    pub fn listVoices() ![]const VoiceInfo {
        const AVSpeechSynthesisVoice = getClass("AVSpeechSynthesisVoice");
        const voices_array = msgSend(
            objc.id,
            AVSpeechSynthesisVoice,
            sel("speechVoices"),
            {},
        );

        if (voices_array == null) {
            return &[_]VoiceInfo{};
        }

        const count = msgSend(c_ulong, voices_array, sel("count"), {});

        var voice_list = std.ArrayList(VoiceInfo).init(std.heap.page_allocator);
        defer voice_list.deinit();

        var i: c_ulong = 0;
        while (i < count) : (i += 1) {
            const voice = msgSendWithArg(objc.id, voices_array, sel("objectAtIndex:"), i);
            if (voice != null) {
                // Get voice properties
                const voice_id = msgSend(objc.id, voice, sel("identifier"), {});
                const voice_name = msgSend(objc.id, voice, sel("name"), {});
                const voice_lang = msgSend(objc.id, voice, sel("language"), {});

                // Convert to C strings
                const id_cstr = msgSend([*c]const u8, voice_id, sel("UTF8String"), {});
                const name_cstr = msgSend([*c]const u8, voice_name, sel("UTF8String"), {});
                const lang_cstr = msgSend([*c]const u8, voice_lang, sel("UTF8String"), {});

                try voice_list.append(VoiceInfo{
                    .id = std.mem.span(id_cstr),
                    .name = std.mem.span(name_cstr),
                    .language = std.mem.span(lang_cstr),
                });
            }
        }

        return try voice_list.toOwnedSlice();
    }

    pub fn setRate(self: *TTSBackend, rate: f32) void {
        self.rate = std.math.clamp(rate, 0.0, 1.0);
    }

    pub fn setPitch(self: *TTSBackend, pitch: f32) void {
        self.pitch = std.math.clamp(pitch, 0.5, 2.0);
    }

    pub fn setVolume(self: *TTSBackend, volume: f32) void {
        self.volume = std.math.clamp(volume, 0.0, 1.0);
    }

    pub const VoiceInfo = struct {
        id: []const u8,
        name: []const u8,
        language: []const u8,
    };
};
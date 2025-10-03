import EmacsSwiftModule
import AVFoundation
import Speech

public func createModule() -> Module {
    return Vox()
}

class Vox: EmacsSwiftModule.Module {
    var isGPLCompatible: Bool { true }

    func Init(_ env: Environment) throws {
        try env.defun("vox/speak-openai", with: "Synthesize and speak text using OpenAI TTS.") {
            (text: String, apiKey: String, voice: String) in
            Task { @MainActor in
                VoxTTS.shared.speakOpenAI(text: text, apiKey: apiKey, voice: voice)
            }
        }

        try env.defun("vox/stop-openai", with: "Stop any ongoing OpenAI speech synthesis.") {
            Task { @MainActor in
                VoxTTS.shared.stopOpenAI()
            }
        }

        try env.defun("vox/whisper-start", with: "Start recording audio for Whisper transcription.") {
            () throws -> String in
            try VoxSTT.shared.startWhisperRecording()
            return "Recording started"
        }

        try env.defun("vox/whisper-transcribe", with: "Stop recording and transcribe via Whisper API.") {
            (apiKey: String) throws -> String in
            return try VoxSTT.shared.stopAndTranscribeWhisper(apiKey: apiKey)
        }

        try env.defun("vox/speak", with: "Speak text using system voice.") {
            (text: String) in
            VoxTTS.shared.speakLocal(text: text)
        }

        try env.defun("vox/speak-with-voice", with: "Speak text using specific system voice.") {
            (text: String, voice: String) in
            VoxTTS.shared.speakLocal(text: text, voice: voice)
        }

        try env.defun("vox/speak-advanced", with: "Speak text with voice, rate, and pitch control.") {
            (text: String, voice: String, rate: Double, pitch: Double) in
            VoxTTS.shared.speakLocal(text: text, voice: voice, rate: Float(rate), pitch: Float(pitch))
        }

        try env.defun("vox/stop-speech", with: "Stop speech synthesis.") {
            VoxTTS.shared.stopLocal()
        }

        try env.defun("vox/pause-speech", with: "Pause speech synthesis.") {
            VoxTTS.shared.pauseLocal()
        }

        try env.defun("vox/continue-speech", with: "Continue paused speech.") {
            VoxTTS.shared.continueLocal()
        }

        try env.defun("vox/list-voices", with: "List available system voices.") {
            () -> [[String: String]] in
            return VoxTTS.shared.listVoices()
        }

        try env.defun("vox/get-suggested-voices", with: "Get suggested voice identifiers.") {
            () -> [String: String] in
            return VoxTTS.shared.getSuggestedVoices()
        }

        try env.defun("vox/request-speech-permission", with: "Request speech recognition permission.") {
            () -> String in
            SFSpeechRecognizer.requestAuthorization { authStatus in
                print("[Speech] Authorization status: \(authStatus)")
            }
            return "Requesting speech recognition permission... Check for system dialog"
        }

        try env.defun("vox/check-speech-permission", with: "Check current speech recognition permission status.") {
            () -> String in
            let status = SFSpeechRecognizer.authorizationStatus()
            switch status {
            case .authorized:
                return "authorized"
            case .denied:
                return "denied"
            case .restricted:
                return "restricted"
            case .notDetermined:
                return "not-determined"
            @unknown default:
                return "unknown"
            }
        }

        try env.defun("vox/start-recording", with: "Start recording with speech recognition.") {
            () throws -> String in
            try VoxSTT.shared.startLocalRecording()
            return "Recording started"
        }

        try env.defun("vox/stop-recording", with: "Stop recording and get transcription.") {
            () -> String in
            return VoxSTT.shared.stopLocalRecording()
        }

        try env.defun("vox/get-recognition-status", with: "Get speech recognition status.") {
            () -> String in
            return VoxSTT.shared.getStatus()
        }

        try env.defun("vox/is-speech-available", with: "Check if speech recognition is available.") {
            () -> Bool in
            return VoxSTT.shared.isAvailable()
        }

        try env.defun("vox/get-current-transcription", with: "Get current partial transcription while recording.") {
            () -> String in
            return VoxSTT.shared.getCurrentTranscription()
        }
    }
}
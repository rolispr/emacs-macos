import EmacsSwiftModule
import AVFoundation
import Speech

public func createModule() -> Module {
    return EmacsMacOSModule()
}

class EmacsMacOSModule: EmacsSwiftModule.Module {
    var isGPLCompatible: Bool { true }

    func Init(_ env: Environment) throws {
        // Skip channel entirely - we don't need async callbacks

        // --- OpenAI TTS Functions ---
        try env.defun("macmod/speak-openai", with: "Synthesize and speak text using OpenAI TTS.") {
            (text: String, apiKey: String, voice: String) in
            Task { @MainActor in
                AudioService.shared.speak(text: text, apiKey: apiKey, voice: voice)
            }
        }

        try env.defun("macmod/stop-openai", with: "Stop any ongoing OpenAI speech synthesis.") {
            Task { @MainActor in
                AudioService.shared.stop()
            }
        }

        // --- Whisper STT Functions ---
        try env.defun("macmod/whisper-start", with: "Start recording audio for Whisper transcription.") {
            () throws -> String in
            try WhisperService.shared.startRecordingSync()
            return "Recording started"
        }

        try env.defun("macmod/whisper-transcribe", with: "Stop recording and transcribe via Whisper API.") {
            (apiKey: String) throws -> String in
            return try WhisperService.shared.stopAndTranscribeSync(apiKey: apiKey)
        }

        // --- Local Voice Functions (System TTS) ---
        try env.defun("macmod/speak", with: "Speak text using system voice.") {
            (text: String) in
            LocalVoiceService.shared.speak(text: text)
        }

        try env.defun("macmod/speak-with-voice", with: "Speak text using specific system voice.") {
            (text: String, voice: String) in
            LocalVoiceService.shared.speak(text: text, voice: voice)
        }

        try env.defun("macmod/speak-advanced", with: "Speak text with voice, rate, and pitch control.") {
            (text: String, voice: String, rate: Double, pitch: Double) in
            LocalVoiceService.shared.speak(text: text, voice: voice, rate: Float(rate), pitch: Float(pitch))
        }

        try env.defun("macmod/stop-speech", with: "Stop speech synthesis.") {
            LocalVoiceService.shared.stop()
        }

        try env.defun("macmod/pause-speech", with: "Pause speech synthesis.") {
            LocalVoiceService.shared.pause()
        }

        try env.defun("macmod/continue-speech", with: "Continue paused speech.") {
            LocalVoiceService.shared.`continue`()
        }

        try env.defun("macmod/list-voices", with: "List available system voices.") {
            () -> [[String: String]] in
            return LocalVoiceService.shared.listVoices()
        }

        try env.defun("macmod/get-suggested-voices", with: "Get suggested voice identifiers.") {
            () -> [String: String] in
            return LocalVoiceService.shared.getSuggestedVoices()
        }


        // --- Local Speech Recognition ---

        // Request permission explicitly
        try env.defun("macmod/request-speech-permission", with: "Request speech recognition permission.") {
            () -> String in
            // Actually request authorization
            SFSpeechRecognizer.requestAuthorization { authStatus in
                print("[Speech] Authorization status: \(authStatus)")
            }
            return "Requesting speech recognition permission... Check for system dialog"
        }

        // Check current permission status (synchronous)
        try env.defun("macmod/check-speech-permission", with: "Check current speech recognition permission status.") {
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

        // Start recording with local speech recognition
        try env.defun("macmod/start-recording", with: "Start recording with speech recognition.") {
            () throws -> String in
            try LocalSpeechRecognizer.shared.startRecording()
            return "Recording started"
        }

        try env.defun("macmod/stop-recording", with: "Stop recording and get transcription.") {
            () -> String in
            return LocalSpeechRecognizer.shared.stopRecording()
        }

        try env.defun("macmod/get-recognition-status", with: "Get speech recognition status.") {
            () -> String in
            return LocalSpeechRecognizer.shared.getStatus()
        }

        try env.defun("macmod/is-speech-available", with: "Check if speech recognition is available.") {
            () -> Bool in
            return LocalSpeechRecognizer.shared.isAvailable()
        }

        try env.defun("macmod/get-current-transcription", with: "Get current partial transcription while recording.") {
            () -> String in
            return LocalSpeechRecognizer.shared.getCurrentTranscription()
        }
    }
}
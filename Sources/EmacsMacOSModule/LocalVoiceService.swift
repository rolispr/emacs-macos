import AVFoundation
import Foundation

/// Service for local text-to-speech using system voices (no API required)
class LocalVoiceService: NSObject {
    static let shared = LocalVoiceService()
    private let synthesizer = AVSpeechSynthesizer()

    override init() {
        super.init()
        synthesizer.delegate = self
    }


    /// Speak text using local system voice
    func speak(text: String, voice: String? = nil, rate: Float = 0.5, pitch: Float = 1.0) {
        let utterance = AVSpeechUtterance(string: text)

        // Set voice - use identifier if provided, otherwise default to system
        if let voiceIdentifier = voice {
            utterance.voice = AVSpeechSynthesisVoice(identifier: voiceIdentifier)
        } else {
            // Default to en-US voice
            utterance.voice = AVSpeechSynthesisVoice(language: "en-US")
        }

        // Configure speech parameters
        utterance.rate = rate  // 0.0 - 1.0, default is 0.5
        utterance.pitchMultiplier = pitch  // 0.5 - 2.0, default is 1.0
        utterance.volume = 0.9  // 0.0 - 1.0

        // Speak
        synthesizer.speak(utterance)
    }

    /// Stop any ongoing speech
    func stop() {
        synthesizer.stopSpeaking(at: .immediate)
    }

    /// Pause speech
    func pause() {
        synthesizer.pauseSpeaking(at: .immediate)
    }

    /// Continue paused speech
    func `continue`() {
        synthesizer.continueSpeaking()
    }

    /// List available system voices
    func listVoices() -> [[String: String]] {
        return AVSpeechSynthesisVoice.speechVoices().map { voice in
            var voiceInfo: [String: String] = [
                "identifier": voice.identifier,
                "name": voice.name,
                "language": voice.language
            ]

            // Check for premium quality (only on macOS 13+)
            if #available(macOS 13.0, *) {
                voiceInfo["quality"] = voice.quality == .premium ? "premium" : "default"
            } else {
                voiceInfo["quality"] = "default"
            }

            return voiceInfo
        }
    }

    /// Get suggested voices for common use cases
    func getSuggestedVoices() -> [String: String] {
        return [
            "samantha": "com.apple.voice.compact.en-US.Samantha",  // Default female
            "alex": "com.apple.voice.compact.en-US.Alex",          // Default male
            "daniel": "com.apple.voice.compact.en-GB.Daniel",      // British male
            "karen": "com.apple.voice.compact.en-AU.Karen",        // Australian female
            "moira": "com.apple.voice.compact.en-IE.Moira",        // Irish female
            "tessa": "com.apple.voice.compact.en-ZA.Tessa",        // South African
            "zoe": "com.apple.voice.premium.en-US.Zoe",            // Premium female
            "oliver": "com.apple.voice.premium.en-US.Oliver",      // Premium male
        ]
    }
}

// MARK: - AVSpeechSynthesizerDelegate
extension LocalVoiceService: AVSpeechSynthesizerDelegate {
    func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didStart utterance: AVSpeechUtterance) {
        print("[LocalVoice] Started speaking")
    }

    func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didFinish utterance: AVSpeechUtterance) {
        print("[LocalVoice] Finished speaking")
    }

    func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didCancel utterance: AVSpeechUtterance) {
        print("[LocalVoice] Cancelled speaking")
    }
}
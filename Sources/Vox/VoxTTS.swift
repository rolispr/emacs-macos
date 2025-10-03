import AVFoundation
import Foundation
import NaturalLanguage

final class VoxTTS: NSObject, @unchecked Sendable {
    static let shared = VoxTTS()
    private let synthesizer = AVSpeechSynthesizer()

    private var audioPlayer: AVAudioPlayer?
    private var audioDataQueue: [Data] = []
    private var isPlaying = false
    private var speechTask: Task<Void, Error>?

    override init() {
        super.init()
        synthesizer.delegate = self
    }

    func speakLocal(text: String, voice: String? = nil, rate: Float = 0.5, pitch: Float = 1.0) {
        let utterance = AVSpeechUtterance(string: text)

        if let voiceIdentifier = voice {
            utterance.voice = AVSpeechSynthesisVoice(identifier: voiceIdentifier)
        } else {
            utterance.voice = AVSpeechSynthesisVoice(language: "en-US")
        }

        utterance.rate = rate
        utterance.pitchMultiplier = pitch
        utterance.volume = 0.9

        synthesizer.speak(utterance)
    }

    func stopLocal() {
        synthesizer.stopSpeaking(at: .immediate)
    }

    func pauseLocal() {
        synthesizer.pauseSpeaking(at: .immediate)
    }

    func continueLocal() {
        synthesizer.continueSpeaking()
    }

    func listVoices() -> [[String: String]] {
        return AVSpeechSynthesisVoice.speechVoices().map { voice in
            var voiceInfo: [String: String] = [
                "identifier": voice.identifier,
                "name": voice.name,
                "language": voice.language
            ]

            if #available(macOS 13.0, *) {
                voiceInfo["quality"] = voice.quality == .premium ? "premium" : "default"
            } else {
                voiceInfo["quality"] = "default"
            }

            return voiceInfo
        }
    }

    func getSuggestedVoices() -> [String: String] {
        return [
            "samantha": "com.apple.voice.compact.en-US.Samantha",
            "alex": "com.apple.voice.compact.en-US.Alex",
            "daniel": "com.apple.voice.compact.en-GB.Daniel",
            "karen": "com.apple.voice.compact.en-AU.Karen",
            "moira": "com.apple.voice.compact.en-IE.Moira",
            "tessa": "com.apple.voice.compact.en-ZA.Tessa",
            "zoe": "com.apple.voice.premium.en-US.Zoe",
            "oliver": "com.apple.voice.premium.en-US.Oliver"
        ]
    }

    @MainActor
    func speakOpenAI(text: String, apiKey: String, voice: String) {
        stopOpenAI()
        speechTask = Task {
            let chunks = self.splitText(text)
            for chunk in chunks {
                try Task.checkCancellation()
                let data = try await VoxOpenAI.generateSpeech(from: chunk, voice: voice, apiKey: apiKey)
                self.audioDataQueue.append(data)
                if !self.isPlaying {
                    self.playNextInQueue()
                }
            }
        }
    }

    @MainActor
    func stopOpenAI() {
        speechTask?.cancel()
        speechTask = nil

        audioPlayer?.stop()
        audioPlayer?.delegate = nil
        audioPlayer = nil
        audioDataQueue.removeAll()
        isPlaying = false
    }

    @MainActor
    private func playNextInQueue() {
        guard !audioDataQueue.isEmpty, !isPlaying else { return }

        let nextData = audioDataQueue.removeFirst()
        isPlaying = true

        do {
            audioPlayer = try AVAudioPlayer(data: nextData)
            audioPlayer?.delegate = self
            audioPlayer?.play()
        } catch {
            isPlaying = false
        }
    }

    private func splitText(_ text: String, maxLength: Int = 4096) -> [String] {
        var chunks: [String] = []
        var currentChunk = ""
        let tokenizer = NLTokenizer(unit: .sentence)
        tokenizer.string = text

        tokenizer.enumerateTokens(in: text.startIndex..<text.endIndex) { tokenRange, _ in
            let sentence = String(text[tokenRange])
            if currentChunk.count + sentence.count > maxLength {
                if !currentChunk.isEmpty { chunks.append(currentChunk) }
                currentChunk = ""
            }
            currentChunk.append(contentsOf: sentence)
            return true
        }

        if !currentChunk.isEmpty {
            chunks.append(currentChunk)
        }

        return chunks
    }
}

extension VoxTTS: AVSpeechSynthesizerDelegate {
    func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didStart utterance: AVSpeechUtterance) {
    }

    func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didFinish utterance: AVSpeechUtterance) {
    }

    func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didCancel utterance: AVSpeechUtterance) {
    }
}

extension VoxTTS: @preconcurrency AVAudioPlayerDelegate {
    @MainActor
    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        isPlaying = false
        player.delegate = nil
        if !audioDataQueue.isEmpty {
            playNextInQueue()
        }
    }
}

struct VoxOpenAI {
    static func generateSpeech(from text: String, voice: String = "alloy", apiKey: String) async throws -> Data {
        let url = URL(string: "https://api.openai.com/v1/audio/speech")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("Bearer \(apiKey)", forHTTPHeaderField: "Authorization")
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")

        let body = [
            "model": "tts-1",
            "input": text,
            "voice": voice
        ]

        request.httpBody = try JSONSerialization.data(withJSONObject: body)

        let (data, response) = try await URLSession.shared.data(for: request)

        if let httpResponse = response as? HTTPURLResponse,
           httpResponse.statusCode != 200 {
            throw NSError(domain: "VoxOpenAI", code: httpResponse.statusCode, userInfo: [NSLocalizedDescriptionKey: "HTTP \(httpResponse.statusCode)"])
        }

        return data
    }
}
import Foundation
import AVFoundation
import NaturalLanguage

// Handles playing back audio data received from OpenAI
class AudioService: NSObject, @preconcurrency AVAudioPlayerDelegate {
    static let shared = AudioService()
    
    // FIXED: Add strong reference to prevent deallocation during playback
    private static var keepAlive: AudioService? = shared
    
    private var audioPlayer: AVAudioPlayer?
    private var audioDataQueue: [Data] = []
    private var isPlaying = false
    private var speechTask: Task<Void, Error>?
    
    override private init() {}
    
    @MainActor
    func speak(text: String, apiKey: String, voice: String) {
        stop() // Clear any existing audio and cancel previous tasks
        speechTask = Task {
            let chunks = self.splitText(text)
            for chunk in chunks {
                try Task.checkCancellation()
                let data = try await OpenAIService.generateSpeech(from: chunk, voice: voice, apiKey: apiKey)
                self.audioDataQueue.append(data)
                if !self.isPlaying {
                    self.playNextInQueue()
                }
            }
        }
    }
    
    @MainActor
    func stop() {
        speechTask?.cancel()
        speechTask = nil
        
        audioPlayer?.stop()
        audioPlayer?.delegate = nil  // FIXED: Clear delegate reference
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
            print("Error playing audio: \(error)")
            isPlaying = false
        }
    }
    
    @MainActor
    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        isPlaying = false
        player.delegate = nil  // FIXED: Clear delegate when done
        if !audioDataQueue.isEmpty {
            playNextInQueue()
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

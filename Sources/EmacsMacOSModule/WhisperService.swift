import Foundation
import AVFoundation

class WhisperService: NSObject, AVAudioRecorderDelegate {
    static let shared = WhisperService()
    private static var keepAlive: WhisperService? = shared
    
    private var audioRecorder: AVAudioRecorder?
    private var tempFileURL: URL {
        FileManager.default.temporaryDirectory.appendingPathComponent("emacs-whisper-recording.m4a")
    }
    
    override private init() {
        super.init()
    }
    
    func startRecordingSync() throws {
        let settings = [
            AVFormatIDKey: Int(kAudioFormatMPEG4AAC),
            AVSampleRateKey: 12000,
            AVNumberOfChannelsKey: 1,
            AVEncoderAudioQualityKey: AVAudioQuality.high.rawValue
        ]
        
        if FileManager.default.fileExists(atPath: tempFileURL.path) {
            try FileManager.default.removeItem(at: tempFileURL)
        }
        
        audioRecorder = try AVAudioRecorder(url: tempFileURL, settings: settings)
        audioRecorder?.delegate = self
        audioRecorder?.record()
    }
    
    func stopAndTranscribeSync(apiKey: String) throws -> String {
        guard let recorder = audioRecorder else {
            throw NSError(domain: "WhisperService", code: 2, userInfo: [NSLocalizedDescriptionKey: "Recording was not started."])
        }
        
        recorder.stop()
        
        // Wait a moment for the file to be written
        Thread.sleep(forTimeInterval: 0.5)
        
        // Check if file exists and has content
        guard FileManager.default.fileExists(atPath: tempFileURL.path) else {
            throw NSError(domain: "WhisperService", code: 4, userInfo: [NSLocalizedDescriptionKey: "Recording file not found"])
        }
        
        let fileSize = try FileManager.default.attributesOfItem(atPath: tempFileURL.path)[.size] as? Int ?? 0
        guard fileSize > 1000 else { // At least 1KB
            throw NSError(domain: "WhisperService", code: 5, userInfo: [NSLocalizedDescriptionKey: "Recording file too small: \(fileSize) bytes"])
        }
        
        // Transcribe
        let transcription = try OpenAIService.transcribeAudioSync(from: tempFileURL, apiKey: apiKey)
        
        // Cleanup
        audioRecorder = nil
        try? FileManager.default.removeItem(at: tempFileURL)
        
        return transcription
    }
    
    // Delegate method
    func audioRecorderDidFinishRecording(_ recorder: AVAudioRecorder, successfully flag: Bool) {
        // Recording finished
    }
}

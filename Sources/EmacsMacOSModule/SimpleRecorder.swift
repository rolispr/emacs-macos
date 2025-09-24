import AVFoundation
import Foundation

/// Simple audio recorder that just captures audio to a file
/// This is more stable than trying to use SFSpeechRecognizer directly
class SimpleRecorder: NSObject {
    static let shared = SimpleRecorder()

    private var audioRecorder: AVAudioRecorder?
    private var recordingURL: URL?

    override init() {
        super.init()
    }

    /// Get URL for temporary recording
    private func getRecordingURL() -> URL {
        let tempDir = FileManager.default.temporaryDirectory
        return tempDir.appendingPathComponent("emacs_recording_\(Date().timeIntervalSince1970).m4a")
    }

    /// Start recording audio
    func startRecording() throws -> String {
        // Stop any existing recording
        if audioRecorder?.isRecording == true {
            audioRecorder?.stop()
        }

        recordingURL = getRecordingURL()

        // Configure recording settings
        let settings: [String: Any] = [
            AVFormatIDKey: Int(kAudioFormatMPEG4AAC),
            AVSampleRateKey: 44100,
            AVNumberOfChannelsKey: 1,
            AVEncoderAudioQualityKey: AVAudioQuality.high.rawValue
        ]

        // Create recorder
        audioRecorder = try AVAudioRecorder(url: recordingURL!, settings: settings)
        audioRecorder?.delegate = self
        audioRecorder?.prepareToRecord()

        // Start recording
        if audioRecorder?.record() == true {
            return "Recording started"
        } else {
            throw NSError(domain: "SimpleRecorder", code: 1,
                         userInfo: [NSLocalizedDescriptionKey: "Failed to start recording"])
        }
    }

    /// Stop recording
    func stopRecording() -> String {
        guard let recorder = audioRecorder, recorder.isRecording else {
            return ""
        }

        recorder.stop()

        // For now, return the file path
        // In production, this would transcribe the audio
        if let url = recordingURL {
            return "Recording saved to: \(url.path)"
        }
        return "Recording stopped"
    }

    /// Check if currently recording
    func isRecording() -> Bool {
        return audioRecorder?.isRecording ?? false
    }
}

// MARK: - AVAudioRecorderDelegate
extension SimpleRecorder: AVAudioRecorderDelegate {
    func audioRecorderDidFinishRecording(_ recorder: AVAudioRecorder, successfully flag: Bool) {
        print("[SimpleRecorder] Recording finished: \(flag)")
    }

    func audioRecorderEncodeErrorDidOccur(_ recorder: AVAudioRecorder, error: Error?) {
        print("[SimpleRecorder] Recording error: \(error?.localizedDescription ?? "Unknown")")
    }
}
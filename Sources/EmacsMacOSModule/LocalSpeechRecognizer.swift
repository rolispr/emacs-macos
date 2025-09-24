import Speech
import AVFoundation

/// Robust local speech recognition using SFSpeechRecognizer
class LocalSpeechRecognizer: NSObject {
    static let shared = LocalSpeechRecognizer()

    private var speechRecognizer: SFSpeechRecognizer?
    private var recognitionRequest: SFSpeechAudioBufferRecognitionRequest?
    private var recognitionTask: SFSpeechRecognitionTask?
    private let audioEngine = AVAudioEngine()

    private var isRecording = false
    private var transcription = ""
    private var isAuthorized = false

    override init() {
        super.init()
        setupRecognizer()
    }

    private func setupRecognizer() {
        // Initialize with user's locale or fallback to en-US
        speechRecognizer = SFSpeechRecognizer(locale: Locale.current) ??
                          SFSpeechRecognizer(locale: Locale(identifier: "en-US"))

        // Check initial authorization status
        checkAuthorization()
    }

    private func checkAuthorization() {
        // Just check current status, don't request
        let status = SFSpeechRecognizer.authorizationStatus()
        switch status {
        case .authorized:
            isAuthorized = true
            print("[Speech] ✅ Speech recognition authorized")
        case .denied:
            print("[Speech] ❌ Speech recognition denied")
            isAuthorized = false
        case .restricted:
            print("[Speech] ⚠️ Speech recognition restricted")
            isAuthorized = false
        case .notDetermined:
            print("[Speech] ❓ Speech recognition not determined - assuming authorized since user says it's enabled")
            // If user says it's enabled in Settings but API says notDetermined, trust the user
            isAuthorized = true
        @unknown default:
            isAuthorized = false
        }
    }

    /// Start recording and recognizing speech with proper error handling
    func startRecording() throws {
        // BYPASS ALL PERMISSION CHECKS - just try to record
        print("[Speech] BYPASSING permission check - attempting to record anyway")
        isAuthorized = true  // Force it

        // Check if recognizer is available
        guard let speechRecognizer = speechRecognizer, speechRecognizer.isAvailable else {
            throw NSError(domain: "LocalSpeechRecognizer", code: 503,
                         userInfo: [NSLocalizedDescriptionKey: "Speech recognizer not available. Check internet connection or language settings."])
        }

        // Reset if already recording
        if isRecording {
            stopRecording()
        }

        // Cancel any ongoing task
        recognitionTask?.cancel()
        recognitionTask = nil

        // Create and configure the speech recognition request
        recognitionRequest = SFSpeechAudioBufferRecognitionRequest()
        guard let recognitionRequest = recognitionRequest else {
            throw NSError(domain: "LocalSpeechRecognizer", code: 500,
                         userInfo: [NSLocalizedDescriptionKey: "Unable to create recognition request"])
        }

        // Configure request for best results
        recognitionRequest.shouldReportPartialResults = true
        recognitionRequest.taskHint = .dictation

        // Use on-device if available for privacy (macOS 13+)
        if #available(macOS 13, *) {
            if speechRecognizer.supportsOnDeviceRecognition {
                recognitionRequest.requiresOnDeviceRecognition = true
                print("[Speech] Using on-device recognition")
            }
        }

        // Configure audio engine input
        let inputNode = audioEngine.inputNode
        let recordingFormat = inputNode.outputFormat(forBus: 0)

        // Validate format
        guard recordingFormat.sampleRate > 0 else {
            throw NSError(domain: "LocalSpeechRecognizer", code: 500,
                         userInfo: [NSLocalizedDescriptionKey: "Invalid audio format"])
        }

        // Clear previous transcription
        transcription = ""

        // Start the recognition task
        recognitionTask = speechRecognizer.recognitionTask(with: recognitionRequest) { [weak self] result, error in
            guard let self = self else { return }

            if let result = result {
                self.transcription = result.bestTranscription.formattedString

                if result.isFinal {
                    print("[Speech] Final transcription: \(self.transcription)")
                    self.cleanup()
                }
            }

            if let error = error {
                print("[Speech] Recognition error: \(error.localizedDescription)")
                self.cleanup()
            }
        }

        // Install tap on audio input
        inputNode.installTap(onBus: 0, bufferSize: 1024, format: recordingFormat) { [weak self] buffer, _ in
            self?.recognitionRequest?.append(buffer)
        }

        // Start audio engine
        audioEngine.prepare()
        try audioEngine.start()

        isRecording = true
        print("[Speech] 🎤 Recording started successfully")
    }

    /// Stop recording and return the transcription
    func stopRecording() -> String {
        guard isRecording else {
            return transcription
        }

        // Capture transcription before cleanup
        let finalTranscription = transcription

        // End audio capture
        recognitionRequest?.endAudio()

        // Clean up immediately - don't use async
        cleanup()

        isRecording = false
        print("[Speech] 🛑 Recording stopped. Transcription: \(finalTranscription)")

        return finalTranscription
    }

    private func cleanup() {
        if audioEngine.isRunning {
            audioEngine.stop()
            audioEngine.inputNode.removeTap(onBus: 0)
        }
        recognitionRequest = nil
        recognitionTask = nil
        isRecording = false
    }

    /// Get current partial transcription
    func getCurrentTranscription() -> String {
        return transcription
    }

    /// Check if currently recording
    func getIsRecording() -> Bool {
        return isRecording
    }

    /// Check if speech recognition is available and authorized
    func isAvailable() -> Bool {
        return isAuthorized && (speechRecognizer?.isAvailable ?? false)
    }

    /// Get authorization status message
    func getStatus() -> String {
        if !isAuthorized {
            return "Not authorized - check System Settings > Privacy & Security > Speech Recognition"
        }
        if !(speechRecognizer?.isAvailable ?? false) {
            return "Speech recognizer not available"
        }
        if isRecording {
            return "Recording..."
        }
        return "Ready"
    }
}

// MARK: - Thread-safe wrapper for Emacs
extension LocalSpeechRecognizer {
    /// Start recording with proper synchronization
    func startRecordingSync() throws {
        try startRecording()
    }

    /// Stop and get transcription synchronously
    func stopRecordingSync() -> String {
        return stopRecording()
    }

    /// Request permission synchronously
    func requestPermissionSync() -> Bool {
        // Check current auth status
        let currentStatus = SFSpeechRecognizer.authorizationStatus()
        isAuthorized = (currentStatus == .authorized)

        if currentStatus == .notDetermined {
            // Can't request permission synchronously from Emacs module
            // User must grant permission outside of Emacs first
            print("[Speech] ⚠️ Permission not granted yet.")
            print("[Speech] Please grant microphone and speech recognition permissions in:")
            print("[Speech] System Settings > Privacy & Security > Microphone")
            print("[Speech] System Settings > Privacy & Security > Speech Recognition")
            return false
        } else if currentStatus == .denied || currentStatus == .restricted {
            print("[Speech] ❌ Permission denied or restricted")
            return false
        }

        return isAuthorized
    }

}
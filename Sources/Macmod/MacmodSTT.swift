import Speech
import AVFoundation
import Foundation

class MacmodSTT: NSObject {
    static let shared = MacmodSTT()

    private var speechRecognizer: SFSpeechRecognizer?
    private var recognitionRequest: SFSpeechAudioBufferRecognitionRequest?
    private var recognitionTask: SFSpeechRecognitionTask?
    private let audioEngine = AVAudioEngine()

    private var audioRecorder: AVAudioRecorder?
    private var tempFileURL: URL {
        FileManager.default.temporaryDirectory.appendingPathComponent("macmod-recording.m4a")
    }

    private var isRecording = false
    private var transcription = ""
    private var isAuthorized = false

    override init() {
        super.init()
        setupRecognizer()
    }

    private func setupRecognizer() {
        speechRecognizer = SFSpeechRecognizer(locale: Locale.current) ??
                          SFSpeechRecognizer(locale: Locale(identifier: "en-US"))
        checkAuthorization()
    }

    private func checkAuthorization() {
        let status = SFSpeechRecognizer.authorizationStatus()
        switch status {
        case .authorized:
            isAuthorized = true
            print("[MacmodSTT] ✅ Speech recognition authorized")
        case .denied:
            print("[MacmodSTT] ❌ Speech recognition denied")
            isAuthorized = false
        case .restricted:
            print("[MacmodSTT] ⚠️ Speech recognition restricted")
            isAuthorized = false
        case .notDetermined:
            print("[MacmodSTT] ❓ Speech recognition not determined - assuming authorized since user says it's enabled")
            isAuthorized = true
        @unknown default:
            isAuthorized = false
        }
    }

    func startLocalRecording() throws {
        print("[MacmodSTT] BYPASSING permission check - attempting to record anyway")
        isAuthorized = true

        guard let speechRecognizer = speechRecognizer, speechRecognizer.isAvailable else {
            throw NSError(domain: "MacmodSTT", code: 503,
                         userInfo: [NSLocalizedDescriptionKey: "Speech recognizer not available. Check internet connection or language settings."])
        }

        if isRecording {
            stopLocalRecording()
        }

        recognitionTask?.cancel()
        recognitionTask = nil

        recognitionRequest = SFSpeechAudioBufferRecognitionRequest()
        guard let recognitionRequest = recognitionRequest else {
            throw NSError(domain: "MacmodSTT", code: 500,
                         userInfo: [NSLocalizedDescriptionKey: "Unable to create recognition request"])
        }

        recognitionRequest.shouldReportPartialResults = true
        recognitionRequest.taskHint = .dictation

        if #available(macOS 13, *) {
            if speechRecognizer.supportsOnDeviceRecognition {
                recognitionRequest.requiresOnDeviceRecognition = true
                print("[MacmodSTT] Using on-device recognition")
            }
        }

        let inputNode = audioEngine.inputNode
        let recordingFormat = inputNode.outputFormat(forBus: 0)

        guard recordingFormat.sampleRate > 0 else {
            throw NSError(domain: "MacmodSTT", code: 500,
                         userInfo: [NSLocalizedDescriptionKey: "Invalid audio format"])
        }

        transcription = ""

        recognitionTask = speechRecognizer.recognitionTask(with: recognitionRequest) { [weak self] result, error in
            guard let self = self else { return }

            if let result = result {
                self.transcription = result.bestTranscription.formattedString

                if result.isFinal {
                    print("[MacmodSTT] Final transcription: \(self.transcription)")
                    self.cleanup()
                }
            }

            if let error = error {
                print("[MacmodSTT] Recognition error: \(error.localizedDescription)")
                self.cleanup()
            }
        }

        inputNode.installTap(onBus: 0, bufferSize: 1024, format: recordingFormat) { [weak self] buffer, _ in
            self?.recognitionRequest?.append(buffer)
        }

        audioEngine.prepare()
        try audioEngine.start()

        isRecording = true
        print("[MacmodSTT] 🎤 Recording started successfully")
    }

    func stopLocalRecording() -> String {
        guard isRecording else {
            return transcription
        }

        let finalTranscription = transcription

        recognitionRequest?.endAudio()
        cleanup()

        isRecording = false
        print("[MacmodSTT] 🛑 Recording stopped. Transcription: \(finalTranscription)")

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

    func getCurrentTranscription() -> String {
        return transcription
    }

    func getIsRecording() -> Bool {
        return isRecording
    }

    func isAvailable() -> Bool {
        return isAuthorized && (speechRecognizer?.isAvailable ?? false)
    }

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

    func startWhisperRecording() throws {
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

    func stopAndTranscribeWhisper(apiKey: String) throws -> String {
        guard let recorder = audioRecorder else {
            throw NSError(domain: "MacmodSTT", code: 2, userInfo: [NSLocalizedDescriptionKey: "Recording was not started."])
        }

        recorder.stop()

        Thread.sleep(forTimeInterval: 0.5)

        guard FileManager.default.fileExists(atPath: tempFileURL.path) else {
            throw NSError(domain: "MacmodSTT", code: 4, userInfo: [NSLocalizedDescriptionKey: "Recording file not found"])
        }

        let fileSize = try FileManager.default.attributesOfItem(atPath: tempFileURL.path)[.size] as? Int ?? 0
        guard fileSize > 1000 else {
            throw NSError(domain: "MacmodSTT", code: 5, userInfo: [NSLocalizedDescriptionKey: "Recording file too small: \(fileSize) bytes"])
        }

        let transcription = try MacmodWhisper.transcribeSync(from: tempFileURL, apiKey: apiKey)

        audioRecorder = nil
        try? FileManager.default.removeItem(at: tempFileURL)

        return transcription
    }
}

extension MacmodSTT: AVAudioRecorderDelegate {
    func audioRecorderDidFinishRecording(_ recorder: AVAudioRecorder, successfully flag: Bool) {
    }
}

struct MacmodWhisper {
    static func transcribeSync(from fileURL: URL, apiKey: String) throws -> String {
        let url = URL(string: "https://api.openai.com/v1/audio/transcriptions")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("Bearer \(apiKey)", forHTTPHeaderField: "Authorization")

        let boundary = UUID().uuidString
        request.setValue("multipart/form-data; boundary=\(boundary)", forHTTPHeaderField: "Content-Type")

        var body = Data()

        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"model\"\r\n\r\n".data(using: .utf8)!)
        body.append("whisper-1\r\n".data(using: .utf8)!)

        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"file\"; filename=\"recording.m4a\"\r\n".data(using: .utf8)!)
        body.append("Content-Type: audio/mp4\r\n\r\n".data(using: .utf8)!)

        let audioData = try Data(contentsOf: fileURL)
        body.append(audioData)
        body.append("\r\n".data(using: .utf8)!)

        body.append("--\(boundary)--\r\n".data(using: .utf8)!)

        request.httpBody = body

        let semaphore = DispatchSemaphore(value: 0)
        var responseData: Data?
        var responseError: Error?
        var httpStatusCode: Int = 0

        URLSession.shared.dataTask(with: request) { data, response, error in
            responseData = data
            responseError = error
            if let httpResponse = response as? HTTPURLResponse {
                httpStatusCode = httpResponse.statusCode
            }
            semaphore.signal()
        }.resume()

        let timeoutResult = semaphore.wait(timeout: .now() + 30)

        if timeoutResult == .timedOut {
            throw NSError(domain: "MacmodWhisper", code: -1, userInfo: [NSLocalizedDescriptionKey: "Request timed out"])
        }

        if let error = responseError {
            throw error
        }

        if httpStatusCode != 200 {
            throw NSError(domain: "MacmodWhisper", code: httpStatusCode, userInfo: [NSLocalizedDescriptionKey: "HTTP \(httpStatusCode)"])
        }

        guard let data = responseData else {
            throw NSError(domain: "MacmodWhisper", code: -2, userInfo: [NSLocalizedDescriptionKey: "No response data"])
        }

        let json = try JSONSerialization.jsonObject(with: data) as? [String: Any]
        return json?["text"] as? String ?? ""
    }
}
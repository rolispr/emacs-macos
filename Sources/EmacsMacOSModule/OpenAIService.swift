import Foundation

struct OpenAIService {
    
    // EXISTING: Async version for TTS (keep unchanged)
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
            throw NSError(domain: "OpenAIService", code: httpResponse.statusCode, userInfo: [NSLocalizedDescriptionKey: "HTTP \(httpResponse.statusCode)"])
        }
        
        return data
    }
    
    // EXISTING: Async version for Whisper transcription (keep for compatibility)
    static func transcribeAudio(from fileURL: URL, apiKey: String) async throws -> String {
        let url = URL(string: "https://api.openai.com/v1/audio/transcriptions")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("Bearer \(apiKey)", forHTTPHeaderField: "Authorization")
        
        let boundary = UUID().uuidString
        request.setValue("multipart/form-data; boundary=\(boundary)", forHTTPHeaderField: "Content-Type")
        
        var body = Data()
        
        // Add model parameter
        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"model\"\r\n\r\n".data(using: .utf8)!)
        body.append("whisper-1\r\n".data(using: .utf8)!)
        
        // Add file parameter
        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"file\"; filename=\"recording.m4a\"\r\n".data(using: .utf8)!)
        body.append("Content-Type: audio/mp4\r\n\r\n".data(using: .utf8)!)
        
        let audioData = try Data(contentsOf: fileURL)
        body.append(audioData)
        body.append("\r\n".data(using: .utf8)!)
        
        body.append("--\(boundary)--\r\n".data(using: .utf8)!)
        
        request.httpBody = body
        
        let (data, response) = try await URLSession.shared.data(for: request)
        
        if let httpResponse = response as? HTTPURLResponse,
           httpResponse.statusCode != 200 {
            throw NSError(domain: "OpenAIService", code: httpResponse.statusCode, userInfo: [NSLocalizedDescriptionKey: "HTTP \(httpResponse.statusCode)"])
        }
        
        let json = try JSONSerialization.jsonObject(with: data) as? [String: Any]
        return json?["text"] as? String ?? ""
    }
    
    // NEW: Synchronous version for Whisper transcription (for Emacs)
    static func transcribeAudioSync(from fileURL: URL, apiKey: String) throws -> String {
        let url = URL(string: "https://api.openai.com/v1/audio/transcriptions")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("Bearer \(apiKey)", forHTTPHeaderField: "Authorization")
        
        let boundary = UUID().uuidString
        request.setValue("multipart/form-data; boundary=\(boundary)", forHTTPHeaderField: "Content-Type")
        
        var body = Data()
        
        // Add model parameter
        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"model\"\r\n\r\n".data(using: .utf8)!)
        body.append("whisper-1\r\n".data(using: .utf8)!)
        
        // Add file parameter
        body.append("--\(boundary)\r\n".data(using: .utf8)!)
        body.append("Content-Disposition: form-data; name=\"file\"; filename=\"recording.m4a\"\r\n".data(using: .utf8)!)
        body.append("Content-Type: audio/mp4\r\n\r\n".data(using: .utf8)!)
        
        let audioData = try Data(contentsOf: fileURL)
        body.append(audioData)
        body.append("\r\n".data(using: .utf8)!)
        
        body.append("--\(boundary)--\r\n".data(using: .utf8)!)
        
        request.httpBody = body
        
        // Perform synchronous request using semaphore
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
        
        // Wait for completion (timeout after 30 seconds)
        let timeoutResult = semaphore.wait(timeout: .now() + 30)
        
        if timeoutResult == .timedOut {
            throw NSError(domain: "OpenAIService", code: -1, userInfo: [NSLocalizedDescriptionKey: "Request timed out"])
        }
        
        if let error = responseError {
            throw error
        }
        
        if httpStatusCode != 200 {
            throw NSError(domain: "OpenAIService", code: httpStatusCode, userInfo: [NSLocalizedDescriptionKey: "HTTP \(httpStatusCode)"])
        }
        
        guard let data = responseData else {
            throw NSError(domain: "OpenAIService", code: -2, userInfo: [NSLocalizedDescriptionKey: "No response data"])
        }
        
        let json = try JSONSerialization.jsonObject(with: data) as? [String: Any]
        return json?["text"] as? String ?? ""
    }
}

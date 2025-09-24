;;; macmod-stt.el --- Speech-to-text functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; STT interface for the macOS module.
;; Provides local speech recognition and Whisper API integration.

;;; Code:

(require 'macmod-core)

(defgroup macmod-stt nil
  "Speech-to-text settings for macOS module."
  :group 'macmod)

(defvar macmod-stt-recording-p nil
  "Whether currently recording.")

(defvar macmod-stt-last-transcription nil
  "Store the last transcription result.")

(defvar macmod-stt-callback nil
  "Callback function for transcription results.")

;; Local Speech Recognition

(defun macmod-stt-check-permission ()
  "Check speech recognition permission status."
  (interactive)
  (if (fboundp 'macmod/check-speech-permission)
      (let ((status (macmod/check-speech-permission)))
        (message "Speech Recognition Permission: %s" status)
        status)
    (error "Permission check function not available")))

(defun macmod-stt-request-permission ()
  "Request speech recognition permission."
  (interactive)
  (if (fboundp 'macmod/request-speech-permission)
      (progn
        (macmod/request-speech-permission)
        (message "Requesting speech permission... Check system dialog"))
    (error "Permission request function not available")))

(defun macmod-stt-start-recording ()
  "Start local speech recognition."
  (interactive)
  (if (fboundp 'macmod/start-recording)
      (progn
        (macmod/start-recording)
        (setq macmod-stt-recording-p t)
        (message "🎤 Recording..."))
    (error "Start recording function not available")))

(defun macmod-stt-stop-recording ()
  "Stop recording and get transcription."
  (interactive)
  (if (fboundp 'macmod/stop-recording)
      (let ((result (macmod/stop-recording)))
        (setq macmod-stt-recording-p nil
              macmod-stt-last-transcription result)
        (when macmod-stt-callback
          (funcall macmod-stt-callback result))
        (message "Transcription: %s" result)
        result)
    (error "Stop recording function not available")))

(defun macmod-stt-get-partial ()
  "Get partial transcription during recording."
  (if (and macmod-stt-recording-p
           (fboundp 'macmod/get-current-transcription))
      (macmod/get-current-transcription)
    ""))

;; Whisper API Integration

(defun macmod-stt-whisper-start ()
  "Start recording for Whisper transcription."
  (interactive)
  (if (fboundp 'macmod/whisper-start)
      (progn
        (macmod/whisper-start)
        (setq macmod-stt-recording-p t)
        (message "🎤 Recording for Whisper..."))
    (error "Whisper start function not available")))

(defun macmod-stt-whisper-transcribe (api-key)
  "Stop recording and transcribe via Whisper API."
  (interactive "sOpenAI API Key: ")
  (if (fboundp 'macmod/whisper-transcribe)
      (let ((result (macmod/whisper-transcribe api-key)))
        (setq macmod-stt-recording-p nil
              macmod-stt-last-transcription result)
        (when macmod-stt-callback
          (funcall macmod-stt-callback result))
        result)
    (error "Whisper transcribe function not available")))

;; Voice Loop Helper

(defun macmod-stt-voice-loop (callback)
  "Start a voice input loop with CALLBACK for results."
  (setq macmod-stt-callback callback)
  (macmod-stt-start-recording))

(provide 'macmod-stt)
;;; macmod-stt.el ends here
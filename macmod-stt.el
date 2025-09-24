;;; macmod-stt.el --- Speech-to-text functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2"))
;; Keywords: multimedia, macos, speech, stt
;; URL: https://github.com/yourusername/macmod

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defun macmod-stt-check-permission ()
  "Check speech recognition permission status."
  (interactive)
  (if (fboundp 'macmod/check-speech-permission)
      (let ((status (macmod/check-speech-permission)))
        (message "Speech Recognition Permission: %s" status)
        status)
    (error "Permission check function not available")))

(defun macmod-stt-request-permission ()
  "Request speech recognition permission from the system."
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
  "Stop recording and transcribe via Whisper API using API-KEY."
  (interactive "sOpenAI API Key: ")
  (if (fboundp 'macmod/whisper-transcribe)
      (let ((result (macmod/whisper-transcribe api-key)))
        (setq macmod-stt-recording-p nil
              macmod-stt-last-transcription result)
        (when macmod-stt-callback
          (funcall macmod-stt-callback result))
        result)
    (error "Whisper transcribe function not available")))

(defun macmod-stt-voice-loop (callback)
  "Start a voice input loop with CALLBACK for results."
  (setq macmod-stt-callback callback)
  (macmod-stt-start-recording))

(provide 'macmod-stt)
;;; macmod-stt.el ends here
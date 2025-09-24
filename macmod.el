;;; macmod.el --- macOS Swift module for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Main entry point for the macOS module package.
;; Provides a unified interface to all module functionality.

;;; Code:

(require 'macmod-core)
(require 'macmod-tts)
(require 'macmod-stt)

;; Optional: Voice commands
(with-eval-after-load 'macmod-stt
  (require 'macmod-voice-commands nil t))

;; Re-export main functions with shorter names
(defalias 'macmod-speak 'macmod-tts-speak)
(defalias 'macmod-stop 'macmod-tts-stop)
(defalias 'macmod-voices 'macmod-tts-list-voices)

(defun macmod-status ()
  "Show macOS module status."
  (interactive)
  (let ((buffer (get-buffer-create "*macOS Module Status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "macOS Module Status\n")
      (insert "===================\n\n")

      (insert (format "Module loaded: %s\n"
                      (if macmod-loaded-p "✓" "✗")))

      (when macmod-loaded-p
        (insert "\nAvailable functions:\n")
        (dolist (func (macmod-loaded-functions))
          (insert (format "  • %s\n" func))))

      (insert "\nTTS Configuration:\n")
      (insert (format "  Default rate: %.2f\n" macmod-tts-default-rate))
      (insert (format "  Default pitch: %.2f\n" macmod-tts-default-pitch))
      (insert (format "  Pronunciations: %d rules\n"
                      (length macmod-tts-pronunciations)))

      (insert "\nSTT Status:\n")
      (insert (format "  Recording: %s\n"
                      (if macmod-stt-recording-p "yes" "no")))
      (when macmod-stt-last-transcription
        (insert (format "  Last transcription: %s\n"
                        macmod-stt-last-transcription))))
    (display-buffer buffer)))

(provide 'macmod)
;;; macmod.el ends here
;;; macmod-tts.el --- Text-to-speech functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; TTS interface for the macOS module.
;; Provides speaking with system voices, rate/pitch control, and pronunciation customization.

;;; Code:

(require 'macmod-core)

(defgroup macmod-tts nil
  "Text-to-speech settings for macOS module."
  :group 'macmod)

(defcustom macmod-tts-default-voice nil
  "Default voice identifier or nickname.
If nil, uses system default."
  :type '(choice (const :tag "System default" nil)
                 string)
  :group 'macmod-tts)

(defcustom macmod-tts-default-rate 0.5
  "Default speaking rate (0.0 to 1.0)."
  :type 'number
  :group 'macmod-tts)

(defcustom macmod-tts-default-pitch 1.0
  "Default pitch multiplier (0.5 to 2.0)."
  :type 'number
  :group 'macmod-tts)

(defcustom macmod-tts-pronunciations nil
  "Alist of (TERM . PRONUNCIATION) for text preprocessing.
Example: ((\"Elisp\" . \"E Lisp\") (\"Emacs\" . \"E-macs\"))"
  :type '(alist :key-type string :value-type string)
  :group 'macmod-tts)

(defun macmod-tts-preprocess (text)
  "Apply pronunciation rules to TEXT."
  (let ((processed text))
    (dolist (rule macmod-tts-pronunciations processed)
      (setq processed
            (replace-regexp-in-string
             (regexp-quote (car rule))
             (cdr rule)
             processed t t)))))

(defun macmod-tts-speak (text &optional voice rate pitch)
  "Speak TEXT with optional VOICE, RATE, and PITCH.
Uses preprocessing if `macmod-tts-pronunciations' is configured."
  (interactive "sText to speak: ")
  (unless macmod-loaded-p
    (error "macOS module not loaded. Run M-x macmod-load first"))

  (let* ((processed-text (if macmod-tts-pronunciations
                             (macmod-tts-preprocess text)
                           text))
         (voice (or voice macmod-tts-default-voice))
         (rate (or rate macmod-tts-default-rate))
         (pitch (or pitch macmod-tts-default-pitch)))

    (cond
     ;; Use advanced function if available
     ((fboundp 'macmod/speak-advanced)
      (macmod/speak-advanced processed-text
                            (or voice "com.apple.voice.compact.en-US.Samantha")
                            rate pitch))
     ;; Fallback to basic speak
     ((and voice (fboundp 'macmod/speak-with-voice))
      (macmod/speak-with-voice processed-text voice))
     ((fboundp 'macmod/speak)
      (macmod/speak processed-text))
     (t
      (error "No speak functions available. Module loaded correctly?")))))

(defun macmod-tts-stop ()
  "Stop any ongoing speech."
  (interactive)
  (if (fboundp 'macmod/stop-speech)
      (macmod/stop-speech)
    (error "Stop function not available")))

(defun macmod-tts-pause ()
  "Pause speech."
  (interactive)
  (if (fboundp 'macmod/pause-speech)
      (macmod/pause-speech)
    (error "Pause function not available")))

(defun macmod-tts-continue ()
  "Continue paused speech."
  (interactive)
  (if (fboundp 'macmod/continue-speech)
      (macmod/continue-speech)
    (error "Continue function not available")))

(defun macmod-tts-list-voices ()
  "List available system voices."
  (interactive)
  (if (fboundp 'macmod/list-voices)
      (let ((voices (macmod/list-voices)))
        (with-current-buffer (get-buffer-create "*macOS Voices*")
          (erase-buffer)
          (insert "Available macOS Voices:\n\n")
          (dolist (voice voices)
            (insert (format "%-40s %s (%s)\n"
                           (alist-get "name" voice nil nil #'string=)
                           (alist-get "language" voice nil nil #'string=)
                           (alist-get "quality" voice nil nil #'string=))))
          (display-buffer (current-buffer))))
    (error "List voices function not available")))

(provide 'macmod-tts)
;;; macmod-tts.el ends here
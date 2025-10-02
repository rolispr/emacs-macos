;;; zig-voice.el --- Voice module built with Zig -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2"))
;; Keywords: multimedia, speech, voice, tts, stt

;;; Commentary:

;; Voice module for Emacs built with Zig
;; Provides TTS and STT functionality cross-platform

;;; Code:

(defgroup zig-voice nil
  "Voice module settings."
  :group 'multimedia)

(defcustom zig-voice-module-path
  (expand-file-name "zig-out/lib/libzig-voice.dylib"
                    (file-name-directory load-file-name))
  "Path to the Zig voice module dynamic library."
  :type 'file
  :group 'zig-voice)

(defvar zig-voice--loaded nil
  "Whether the Zig voice module is loaded.")

;;;###autoload
(defun zig-voice-load ()
  "Load the Zig voice module."
  (interactive)
  (unless zig-voice--loaded
    (if (file-exists-p zig-voice-module-path)
        (progn
          (module-load zig-voice-module-path)
          (setq zig-voice--loaded t)
          (message "Zig voice module loaded successfully"))
      (error "Zig voice module not found at %s" zig-voice-module-path))))

;;;###autoload
(defun zig-voice-test ()
  "Test that the voice module is loaded and working."
  (interactive)
  (zig-voice-load)
  (message "Module functions available: %s"
           (delq nil (mapcar (lambda (sym)
                              (when (string-prefix-p "macmod/" (symbol-name sym))
                                sym))
                            (apropos-internal "macmod/.*")))))

;; Auto-load on require
(when (featurep 'modules)
  (zig-voice-load))

(provide 'zig-voice)
;;; zig-voice.el ends here
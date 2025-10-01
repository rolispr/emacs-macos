;;; macmod-voice.el --- Voice commands and control -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (cl-lib "0.5"))
;; Keywords: multimedia, macos, speech, voice-commands
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

;; Voice command system for macOS module.
;; Provides continuous voice recognition and command execution.

;;; Code:

(require 'cl-lib)
(require 'macmod-stt)
(require 'macmod-tts)

(defgroup macmod-voice nil
  "Voice command settings for macOS module."
  :group 'macmod)

(defvar macmod-voice--active nil
  "Whether voice mode is currently active (internal use).")

(defvar macmod-voice--last-text ""
  "Last recognized text (internal use).")

(defvar macmod-voice--processed-text ""
  "Already processed text to avoid duplicates (internal use).")

(defvar macmod-voice--last-change-time 0
  "Time of last text change (internal use).")

(defvar macmod-voice--process-timer nil
  "Timer for processing voice input (internal use).")

(defvar macmod-voice--insert-mode nil
  "Whether in insert mode for dictation (internal use).")

(defun macmod-voice-start ()
  "Start voice command mode."
  (interactive)
  (when macmod-voice--active
    (user-error "Voice mode already active"))
  (setq macmod-voice--active t
        macmod-voice--last-text ""
        macmod-voice--processed-text ""
        macmod-voice--last-change-time 0
        macmod-voice--insert-mode nil)
  (macmod-stt-start-recording)
  (setq macmod-voice--process-timer
        (run-with-timer 0.1 0.1 #'macmod-voice--process))
  (message "Voice active. Say 'done' to stop."))

(defun macmod-voice-stop ()
  "Stop voice command mode."
  (interactive)
  (setq macmod-voice--active nil)
  (when macmod-voice--process-timer
    (cancel-timer macmod-voice--process-timer)
    (setq macmod-voice--process-timer nil))
  (macmod-stt-stop-recording)
  (message "Voice stopped."))

(defun macmod-voice--process ()
  "Process voice input continuously (internal use)."
  (when macmod-voice--active
    (when-let* ((text (macmod-stt-get-partial))
                ((not (string-empty-p text))))
      (when (not (string= text macmod-voice--last-text))
        (message "[Debug] Text changed: '%s'" text)
        (setq macmod-voice--last-text text
              macmod-voice--last-change-time (float-time)))

      (when (and (> macmod-voice--last-change-time 0)
                 (> (- (float-time) macmod-voice--last-change-time) 1.0)
                 (not (string= text macmod-voice--processed-text)))
        (message "[Debug] Processing after pause: '%s'" text)
        (setq macmod-voice--processed-text text)
        (macmod-voice--handle-phrase text)
        (setq macmod-voice--last-text ""
              macmod-voice--last-change-time 0)))))

(defun macmod-voice--handle-phrase (phrase)
  "Process PHRASE as command or dictation (internal use)."
  (let ((cleaned (string-trim (downcase phrase))))
    (message "[Voice] Processing: '%s'" cleaned)

    (if (string-match "\\b\\(done\\|stop\\)\\b" cleaned)
        (progn
          (setq macmod-voice--insert-mode nil)
          (macmod-voice-stop))

      (if macmod-voice--insert-mode
          (if (string= cleaned "command mode")
              (progn
                (setq macmod-voice--insert-mode nil)
                (message "[Command mode] Ready for commands"))
            (if (not buffer-read-only)
                (progn
                  (insert phrase)
                  (message "[Inserted] %s" phrase))
              (message "[Cannot insert] Buffer is read-only")))

        (cond
         ((string= cleaned "other window")
          (other-window 1)
          (message "[Done] Switched to other window"))

         ((string= cleaned "split window")
          (split-window-right)
          (message "[Done] Split window"))

         ((string= cleaned "delete window")
          (delete-window)
          (message "[Done] Deleted window"))

         ((string= cleaned "kill buffer")
          (kill-buffer)
          (message "[Done] Killed buffer"))

         ((string= cleaned "switch buffer")
          (call-interactively 'switch-to-buffer)
          (message "[Done] Switching buffer..."))

         ((string= cleaned "find file")
          (call-interactively 'find-file)
          (message "[Done] Finding file..."))

         ((string= cleaned "save")
          (save-buffer)
          (message "[Done] Buffer saved"))

         ((string= cleaned "insert")
          (setq macmod-voice--insert-mode t)
          (message "[Insert mode] Say text to insert, 'done' to stop"))

         ((string= cleaned "command mode")
          (setq macmod-voice--insert-mode nil)
          (message "[Command mode] Ready for commands"))

         (t
          (let ((cmd (intern (replace-regexp-in-string " " "-" cleaned))))
            (if (commandp cmd)
                (progn
                  (message "[Voice] Executing: %s" cmd)
                  (condition-case err
                      (call-interactively cmd)
                    (error (message "Error: %s" err))))
              (message "[Voice] No command for: '%s'" cleaned)))))))))

(defun macmod-voice-toggle ()
  "Toggle voice command mode."
  (interactive)
  (if macmod-voice--active
      (macmod-voice-stop)
    (macmod-voice-start)))

(defun macmod-voice-command ()
  "One-shot voice command recognition."
  (interactive)
  (macmod-stt-start-recording)
  (message "🎤 Listening...")
  (run-with-timer 3 nil
                  (lambda ()
                    (when-let* ((text (macmod-stt-stop-recording))
                                ((not (string-empty-p text))))
                      (macmod-voice--handle-phrase text)))))

(defun macmod-voice-active-p ()
  "Return t if voice mode is active."
  macmod-voice--active)

(global-set-key (kbd "C-c v") 'macmod-voice-toggle)
(global-set-key (kbd "C-c v c") 'macmod-voice-command)

(provide 'macmod-voice)
;;; macmod-voice.el ends here
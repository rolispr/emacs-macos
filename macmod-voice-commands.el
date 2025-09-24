;;; macmod-voice-commands.el --- Voice command system for Emacs -*- lexical-binding: t; -*-

;;; Code:

(require 'macmod-stt)
(require 'cl-lib)

(defvar macmod-voice-active nil)
(defvar macmod-voice-last-text "")
(defvar macmod-voice-processed-text "")
(defvar macmod-voice-last-change-time 0)
(defvar macmod-voice-process-timer nil)
(defvar macmod-voice-insert-mode nil)

(defun macmod-voice-start ()
  "Start voice mode."
  (interactive)
  (when macmod-voice-active
    (user-error "Already active"))
  (setq macmod-voice-active t
        macmod-voice-last-text ""
        macmod-voice-processed-text ""
        macmod-voice-last-change-time 0
        macmod-voice-insert-mode nil)
  (macmod-stt-start-recording)
  (setq macmod-voice-process-timer
        (run-with-timer 0.1 0.1 #'macmod-voice-process))
  (message "Voice active. Say 'done' to stop."))

(defun macmod-voice-stop ()
  "Stop voice mode."
  (interactive)
  (setq macmod-voice-active nil)
  (when macmod-voice-process-timer
    (cancel-timer macmod-voice-process-timer)
    (setq macmod-voice-process-timer nil))
  (macmod-stt-stop-recording)
  (message "Voice stopped."))

(defun macmod-voice-process ()
  "Process voice input."
  (when macmod-voice-active
    (let ((text (macmod-stt-get-partial)))
      (when (and text (not (string-empty-p text)))
        ;; Text changed - update timestamp and last text
        (when (not (string= text macmod-voice-last-text))
          (message "[Debug] Text changed: '%s'" text)
          (setq macmod-voice-last-text text
                macmod-voice-last-change-time (float-time)))

        ;; Wait for pause (1 second of no changes) before processing
        (when (and (> macmod-voice-last-change-time 0)
                   (> (- (float-time) macmod-voice-last-change-time) 1.0)
                   (not (string= text macmod-voice-processed-text)))
          (message "[Debug] Processing after pause: '%s'" text)
          ;; Mark as processed BEFORE handling to avoid loops
          (setq macmod-voice-processed-text text)
          (macmod-voice-handle-phrase text)
          ;; Reset for next phrase
          (setq macmod-voice-last-text ""
                macmod-voice-last-change-time 0))))))

(defun macmod-voice-handle-phrase (phrase)
  "Process complete PHRASE as command."
  (let ((cleaned (string-trim (downcase phrase))))
    (message "[Voice] Processing: '%s'" cleaned)

    ;; Check for stop commands
    (if (string-match "\\b\\(done\\|stop\\)\\b" cleaned)
        (progn
          (setq macmod-voice-insert-mode nil)
          (macmod-voice-stop))

      ;; In insert mode - check for command mode first
      (if macmod-voice-insert-mode
          (if (string= cleaned "command mode")
              (progn
                (setq macmod-voice-insert-mode nil)
                (message "[Command mode] Ready for commands"))
            ;; Insert text in current buffer
            (if (not buffer-read-only)
                (progn
                  (insert phrase)  ; Insert original phrase, not cleaned
                  (message "[Inserted] %s" phrase))
              (message "[Cannot insert] Buffer is read-only")))

        ;; Try exact command matches first
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

     ((string= cleaned "save buffer")
      (save-buffer)
      (message "[Done] Saved buffer"))

     ((string= cleaned "save")
      (save-buffer)
      (message "[Done] Saved"))

     ((string= cleaned "undo")
      (undo)
      (message "[Done] Undone"))

     ((string= cleaned "kill buffer")
      (kill-buffer)
      (message "[Done] Killed buffer"))

     ((string= cleaned "switch buffer")
      (call-interactively 'switch-to-buffer)
      (message "[Done] Switching buffer..."))

     ((string= cleaned "find file")
      (call-interactively 'find-file)
      (message "[Done] Finding file..."))

     ((string= cleaned "insert")
      (setq macmod-voice-insert-mode t)
      (message "[Insert mode] Say text to insert, 'done' to stop"))

     ((string= cleaned "command mode")
      (setq macmod-voice-insert-mode nil)
      (message "[Command mode] Ready for commands"))

     ;; Try space-to-hyphen conversion
     (t
      (let ((cmd (intern (replace-regexp-in-string " " "-" cleaned))))
        (if (commandp cmd)
            (progn
              (message "[Voice] Executing: %s" cmd)
              (condition-case err
                  (call-interactively cmd)
                (error (message "Error: %s" err))))
          (message "[Voice] No command for: '%s'" cleaned)))))))))

(defun macmod-voice-command-toggle ()
  "Toggle voice command mode."
  (interactive)
  (if macmod-voice-active
      (macmod-voice-stop)
    (macmod-voice-start)))

(global-set-key (kbd "C-c v") 'macmod-voice-command-toggle)

(provide 'macmod-voice-commands)
;;; macmod-voice-commands.el ends here
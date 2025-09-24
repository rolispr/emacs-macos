;;; macos-voice.el --- Full voice interface with local transcription -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete voice loop: local TTS and STT using system APIs
;; No external APIs needed!

;;; Code:

;; DO NOT reset this variable - it tracks module loading across reloads!
(defvar macos-module-loaded-p nil
  "Track whether the macOS module has been loaded. DO NOT RESET!")

(defvar macos-module-path
  "/Users/bret.horne/.emacs.d/macos-module/.build/arm64-apple-macosx/debug/libEmacsMacOSModule.dylib"
  "Path to the compiled Swift module.")

(defun macos-module-ensure-loaded ()
  "Safely load the macOS module only once."
  ;; First check if functions already exist (most reliable check!)
  (if (fboundp 'macmod/speak)
      (progn
        (setq macos-module-loaded-p t)
        (message "✅ Module already loaded (functions exist)"))
    ;; Not loaded, so load it
    (if (file-exists-p macos-module-path)
        (progn
          (module-load macos-module-path)
          (setq macos-module-loaded-p t)
          (message "✅ macOS module loaded for first time"))
      (error "Module not found at %s. Run 'swift build' first" macos-module-path))))

;; Load the module (safe - checks if already loaded)
(macos-module-ensure-loaded)

;;; Text-to-Speech (Local System Voices)

;; Pronunciation configuration
(defvar macos-voice-pronunciations nil
  "Alist of (TERM . PRONUNCIATION) for text preprocessing.
Set this to customize pronunciations.")

(defvar macos-voice-default-rate 0.5
  "Default speaking rate (0.0-1.0).")

(defvar macos-voice-default-pitch 1.0
  "Default pitch multiplier (0.5-2.0).")

(defun macos-voice-preprocess (text)
  "Preprocess TEXT according to `macos-voice-pronunciations'."
  (let ((processed text))
    ;; Apply custom pronunciations
    (dolist (rule macos-voice-pronunciations)
      (setq processed
            (replace-regexp-in-string
             (regexp-quote (car rule))
             (cdr rule)
             processed t t)))
    processed))

(defun macos-speak (text &optional voice rate pitch)
  "Speak TEXT using system voice.
VOICE can be nickname or identifier.
RATE controls speed (0.0-1.0, default 0.5).
PITCH controls tone (0.5-2.0, default 1.0)."
  (interactive "sText to speak: ")
  (macos-module-ensure-loaded)
  (condition-case err
      (let* ((processed-text (if macos-voice-pronunciations
                                 (macos-voice-preprocess text)
                               text))
             (voice-id (when voice
                        (let ((suggested (append (macmod/get-suggested-voices) nil)))
                          (or (cdr (assoc voice suggested)) voice))))
             (rate (or rate macos-voice-default-rate))
             (pitch (or pitch macos-voice-default-pitch)))
        (if (fboundp 'macmod/speak-advanced)
            (macmod/speak-advanced processed-text
                                  (or voice-id "com.apple.voice.compact.en-US.Samantha")
                                  rate pitch)
          ;; Fallback to basic speak
          (if voice-id
              (macmod/speak-with-voice processed-text voice-id)
            (macmod/speak processed-text)))
        (message "🔊 Speaking: %s" (substring processed-text 0 (min 50 (length processed-text)))))
    (error (message "Speech error: %s" err))))

(defun macos-stop-speech ()
  "Stop any ongoing speech."
  (interactive)
  (macmod/stop-speech)
  (message "🔇 Speech stopped"))

;;; Permission Callback

(defun macos-voice-permission-callback (status)
  "Handle async permission callback from Swift module."
  (message "Speech recognition permission: %s" status)
  (when (string= status "authorized")
    (message "✅ You can now use speech recognition!")))

;;; Speech-to-Text (Local Speech Recognition)

(defvar macos-recording-p nil
  "Whether currently recording.")

(defun macos-check-speech-status ()
  "Check speech recognition status."
  (interactive)
  (macos-module-ensure-loaded)
  (let ((status (macmod/check-speech-permission)))
    (message "Speech Recognition: %s" status)
    status))

(defun macos-request-speech-permission ()
  "Request speech recognition permission (async)."
  (interactive)
  (macos-module-ensure-loaded)
  (macmod/request-speech-permission)
  (message "Requesting speech permission... Check system dialog"))

(defvar macos-voice-timer nil
  "Timer for checking real-time transcription.")

(defun macos-show-partial-transcription ()
  "Display partial transcription in minibuffer."
  (when macos-recording-p
    (let ((partial (macmod/get-current-transcription)))
      (when (and partial (not (string-empty-p partial)))
        ;; Show last 80 chars if too long (to avoid cutoff)
        (let ((display-text
               (if (> (length partial) 80)
                   (concat "..." (substring partial (- (length partial) 80)))
                 partial)))
          (message "🎤 %s" display-text))))))

(defun macos-start-recording ()
  "Start recording with local speech recognition."
  (macos-module-ensure-loaded)
  ;; Just try to start - it will request permission if needed
  (condition-case err
      (progn
        (macmod/start-recording)
        (setq macos-recording-p t)
        ;; Start timer for real-time display
        (when macos-voice-timer
          (cancel-timer macos-voice-timer))
        (setq macos-voice-timer
              (run-at-time nil 0.5 #'macos-show-partial-transcription))
        (message "🎤 Recording... (press C-c v v to stop)"))
    (error
     (message "Recording failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

(defun macos-cancel-recording ()
  "Cancel recording without returning transcription."
  (interactive)
  (when macos-recording-p
    (setq macos-recording-p nil)
    (when macos-voice-timer
      (cancel-timer macos-voice-timer)
      (setq macos-voice-timer nil))
    (macmod/stop-recording)
    (setq macos-voice-claude-state nil)
    (message "❌ Recording cancelled")))

(defun macos-stop-recording ()
  "Stop recording and return transcription."
  (macos-module-ensure-loaded)
  (setq macos-recording-p nil)
  ;; Stop the real-time display timer
  (when macos-voice-timer
    (cancel-timer macos-voice-timer)
    (setq macos-voice-timer nil))
  (let ((transcription (macmod/stop-recording)))
    (if (and transcription (not (string-empty-p transcription)))
        (progn
          (message "📝 Transcription: %s" transcription)
          transcription)
      (message "No transcription received")
      "")))

(defun macos-record-toggle ()
  "Toggle recording on/off. Insert text when stopping."
  (interactive)
  (if macos-recording-p
      (progn
        (let ((text (macos-stop-recording)))
          (when (and text (not (string-empty-p text)))
            (if buffer-read-only
                ;; In read-only buffer, copy to kill ring and show message
                (progn
                  (kill-new text)
                  (message "📋 Copied to clipboard: %s" text))
              ;; Normal buffer, insert
              (insert text)
              (insert " ")))))
    (macos-start-recording)))

(defun macos-record-and-insert ()
  "Record speech and insert transcription at point."
  (interactive)
  (macos-record-toggle))

;;; The Complete Voice Loop

(defvar macos-voice-claude-state nil
  "State of voice-claude interaction: nil, 'recording, or 'recorded")

(defvar macos-voice-last-transcription nil
  "Store the last transcription for speaking")

(defun macos-voice-to-claude ()
  "Smart toggle for voice conversation with Claude.
First press: Start recording
Second press: Stop recording and send to Claude
Third press: Speak Claude's response (if available)"
  (interactive)
  (cond
   ;; Currently recording - stop and send
   (macos-recording-p
    (let ((text (macos-stop-recording)))
      (when (and text (not (string-empty-p text)))
        ;; Store for display
        (setq macos-voice-last-transcription text)
        ;; Copy to kill ring
        (kill-new text)
        ;; Send to Claude
        (if (get-buffer "*claude-code[bret.horne]*")
            (progn
              ;; Send without switching
              (with-current-buffer "*claude-code[bret.horne]*"
                (goto-char (point-max))
                (setq macos-voice-last-send-position (point))
                (vterm-send-string text)
                (vterm-send-return))
              ;; Show first part in echo area (avoid cutoff)
              (let ((display-text (if (> (length text) 70)
                                      (concat (substring text 0 70) "...")
                                    text)))
                (message "📤 Sent: %s" display-text))
              (setq macos-voice-claude-state 'recorded))
          (message "❌ Claude buffer not found. Start claude-code first."))))

    ;; Just sent - speak response
    ((eq macos-voice-claude-state 'recorded)
     (macos-speak-last-claude-response)
     (setq macos-voice-claude-state nil))

    ;; Not recording - start
    (t
     (macos-start-recording)
     (setq macos-voice-claude-state 'recording)
     (message "🎤 Listening... Press %s to stop & send"
              (key-description (this-command-keys)))))))

(defun macos-voice-to-text ()
  "Toggle voice recording (simple transcription)."
  (interactive)
  (macos-record-toggle))

(defun macos-voice-chat ()
  "Complete voice interaction loop with local transcription."
  (interactive)
  ;; Greet
  (macos-speak "Ready for voice input.")

  ;; Record
  (macos-start-recording)
  (read-key "🎤 Speak your command... (press any key when done)")

  ;; Get transcription
  (let ((input (macos-stop-recording)))
    (if (and input (not (string-empty-p input)))
        (progn
          ;; Echo what we heard
          (macos-speak (format "You said: %s" input))

          ;; Process command (examples)
          (cond
           ((string-match-p "\\(hello\\|hi\\)" (downcase input))
            (macos-speak "Hello! How can I help you?"))

           ((string-match-p "time" (downcase input))
            (macos-speak (format "The time is %s" (format-time-string "%l:%M %p"))))

           ((string-match-p "buffer" (downcase input))
            (macos-speak (format "Current buffer is %s" (buffer-name))))

           (t
            (macos-speak "I heard you. Ready to integrate with your LLM!")))

          ;; Return the transcription
          input)
      (macos-speak "Sorry, I didn't catch that.")
      nil)))

(defun macos-voice-command ()
  "Execute voice command using local transcription."
  (interactive)
  (let ((command (macos-voice-to-text)))
    (when command
      (message "Command: %s" command)
      ;; Here you can hook into claude-code-ide or any command processor
      ;; For now, just try to execute if it's a known command
      (let ((cmd (intern-soft command)))
        (if (commandp cmd)
            (progn
              (call-interactively cmd)
              (macos-speak "Command executed"))
          (macos-speak (format "Unknown command: %s" command)))))))

;;; Demo and Testing

(defun macos-test-full-loop ()
  "Test the complete voice loop with local everything."
  (interactive)
  ;; Test TTS
  (macos-speak "Testing text to speech. This is using your Mac's system voice.")
  (sleep-for 2)

  ;; Test STT
  (macos-speak "Now testing speech recognition. Please speak after the beep.")
  (sleep-for 1)

  ;; Record
  (macos-start-recording)
  (let ((key (read-key "🎤 Speak now... (press any key when done)")))
    (let ((text (macos-stop-recording)))
      (if text
          (progn
            (macos-speak (format "I heard: %s" text))
            (message "Transcription: %s" text))
        (macos-speak "No transcription received")))))

;;; Integration with LLM (ready to connect)

(defun macos-voice-to-llm ()
  "Record voice, transcribe locally, send to LLM."
  (interactive)
  (let ((input (macos-voice-to-text)))
    (when input
      ;; This is where you'd send to claude-code-ide
      ;; For example:
      ;; (claude-code-ide-query input)
      (message "Ready to send to LLM: %s" input)
      input)))

;; Voice conversation mode (minor mode for continuous conversation)
(define-minor-mode macos-voice-conversation-mode
  "Minor mode for voice conversation with Claude.
When enabled, RET sends voice to Claude."
  :lighter " 🎤"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Need to use a different key since vterm intercepts RET
            (define-key map (kbd "C-<return>") #'macos-voice-or-return)
            (define-key map (kbd "C-c RET") #'macos-voice-or-return)
            (define-key map (kbd "C-c C-c") #'macos-voice-to-claude)
            map))

(defun macos-voice-or-return ()
  "If recording, send to Claude. Otherwise, start recording."
  (interactive)
  (if macos-recording-p
      ;; Stop and send
      (macos-voice-to-claude)
    ;; Start recording
    (macos-start-recording)
    (message "🎤 Speak, then press RET to send...")))

(defvar macos-voice-last-position nil
  "Last position in buffer before Claude responds.")

(defvar macos-voice-speak-responses t
  "Whether to speak Claude's responses aloud.")

(defvar macos-claude-voice "Samantha"
  "Voice to use for Claude's responses. Try: Samantha, Alex, Victoria, Zoe, or Karen.")

(defun macos-watch-for-claude-response (beg end len)
  "Watch for Claude's response and speak it.
BEG, END, and LEN are the after-change-functions arguments."
  (when (and macos-voice-conversation-mode
             macos-voice-speak-responses
             macos-voice-last-position)
    ;; Only check if we've added significant text
    (when (> (- end beg) 5)
      (let ((new-text (buffer-substring-no-properties
                       macos-voice-last-position
                       (point-max))))
        ;; Look for Claude's response (not user input)
        (when (and (> (length new-text) 10)
                   ;; Simple heuristic - if it's a long block of text, it's probably Claude
                   (string-match-p "\n\n" new-text))
          ;; Clean up the text for speaking
          (let ((speak-text (replace-regexp-in-string
                             "\\*\\*\\|```[^`]*```\\|\\[.*?\\]" "" new-text)))
            (when (> (length speak-text) 20)
              (macos-speak speak-text)
              (setq macos-voice-last-position nil))))))))

(defun macos-start-voice-conversation ()
  "Start voice conversation mode with Claude."
  (interactive)
  ;; Find or switch to Claude buffer
  (if (get-buffer "*claude-code[bret.horne]*")
      (progn
        (switch-to-buffer "*claude-code[bret.horne]*")
        ;; Enable voice mode
        (macos-voice-conversation-mode 1)
        ;; Skip the auto-speak for now - vterm makes it complex
        (message "🎤 Voice conversation ON. Press C-RET to toggle recording."))
    (message "Claude Code buffer not found. Start claude-code first.")))

;;; Dedicated Voice Conversation Buffer

(defvar macos-voice-prompt-buffer "*Voice Prompt*"
  "Buffer for voice conversation with Claude.")

(defvar macos-voice-prompt-marker nil
  "Marker for the start of current prompt.")

(defvar macos-voice-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'macos-voice-prompt-record)
    (define-key map (kbd "C") #'macos-voice-prompt-record)
    (define-key map (kbd "RET") #'macos-voice-prompt-send)
    (define-key map (kbd "s") #'macos-speak-last-claude-response)
    (define-key map (kbd "C-u") #'macos-voice-prompt-clear-current)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "h") #'macos-voice-prompt-help)
    map)
  "Keymap for voice prompt buffer.")

(define-derived-mode macos-voice-prompt-mode fundamental-mode "Voice"
  "Major mode for voice conversation with Claude.
\\{macos-voice-prompt-mode-map}"
  (setq-local cursor-type 'box)
  (read-only-mode -1)
  ;; Force the keymap
  (set-keymap-parent macos-voice-prompt-mode-map nil)
  (use-local-map macos-voice-prompt-mode-map))

(defun macos-voice-prompt-insert-prompt ()
  "Insert a new prompt marker."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (let ((inhibit-read-only t))
    (insert "\n> ")
    (setq macos-voice-prompt-marker (point-marker))))

(defun macos-voice-prompt-record ()
  "Toggle recording in voice prompt buffer."
  (interactive)
  (if macos-recording-p
      (progn
        (let ((text (macos-stop-recording)))
          (when (and text (not (string-empty-p text)))
            ;; Insert at end of buffer
            (goto-char (point-max))
            (insert text)
            (insert " "))))
    (macos-start-recording)
    (message "🎤 Recording... Press 'c' again to stop")))

(defun macos-voice-prompt-send ()
  "Send only the current prompt text to Claude."
  (interactive)
  (when macos-voice-prompt-marker
    (let ((text (string-trim (buffer-substring-no-properties
                              macos-voice-prompt-marker
                              (point-max)))))
      (when (not (string-empty-p text))
        ;; Add to history
        (goto-char (point-max))
        (insert "\n[Sent: " (format-time-string "%H:%M") "]\n")
        ;; Send to Claude
        (if (get-buffer "*claude-code[bret.horne]*")
            (progn
              (with-current-buffer "*claude-code[bret.horne]*"
                (goto-char (point-max))
                ;; Mark where we're sending from
                (setq macos-voice-last-send-position (point))
                (vterm-send-string text)
                (vterm-send-return))
              ;; Create new prompt
              (macos-voice-prompt-insert-prompt)
              (message "✅ Sent! Press 'c' to record again.")
              ;; Show Claude buffer
              (display-buffer "*claude-code[bret.horne]*"))
          (message "❌ Claude buffer not found. Start claude-code first."))))))

(defun macos-voice-prompt-clear-current ()
  "Clear the current prompt."
  (interactive)
  (when macos-voice-prompt-marker
    (delete-region macos-voice-prompt-marker (point-max))))

(defun macos-voice-prompt-help ()
  "Show help for voice prompt buffer."
  (interactive)
  (message "Voice Prompt: 'c' = record/stop, RET = send, C-u = clear current, 'q' = quit"))

(defun macos-open-voice-prompt ()
  "Open the voice prompt buffer for conversation with Claude."
  (interactive)
  (let ((buf (get-buffer-create macos-voice-prompt-buffer)))
    (switch-to-buffer buf)
    (unless (eq major-mode 'macos-voice-prompt-mode)
      (macos-voice-prompt-mode)
      ;; Only insert header on first open
      (when (= (buffer-size) 0)
        (insert "🎤 Voice Prompt - History\n")
        (insert "════════════════════════\n")))
    ;; Always create a new prompt at the end
    (macos-voice-prompt-insert-prompt)
    (message "Ready! Press 'c' to start recording")))

(defvar macos-voice-last-send-position nil
  "Position in Claude buffer when we last sent a message.")

(defun macos-speak-last-claude-response ()
  "Speak Claude's response to the last message sent."
  (interactive)
  (if (get-buffer "*claude-code[bret.horne]*")
      (with-current-buffer "*claude-code[bret.horne]*"
        ;; Get text from where we sent to current position
        (let* ((start (or macos-voice-last-send-position
                          (- (point-max) 1000))) ; fallback to last 1000 chars
               (end (point-max))
               (text (buffer-substring-no-properties start end))
               ;; Clean up - remove ANSI codes, the prompt we sent, etc
               (clean-text (replace-regexp-in-string
                            "\\[[0-9;]*m\\|\\[[A-Z]\\|\\[\\?.*?h\\|^>.*$\\|^bret\\.horne.*$"
                            "" text))
               ;; Further cleanup - remove extra whitespace
               (clean-text (string-trim clean-text)))
          (if (> (length clean-text) 20)
              (progn
                (macos-speak clean-text macos-claude-voice)
                (message "🔊 Speaking Claude's response..."))
            (message "No response to speak yet"))))
    (message "Claude buffer not found")))

(defun macos-toggle-claude-voice ()
  "Toggle speaking Claude's responses."
  (interactive)
  (setq macos-voice-speak-responses (not macos-voice-speak-responses))
  (message "Claude voice responses: %s" (if macos-voice-speak-responses "ON" "OFF")))

;;; Keybindings

;; Main toggle binding
(global-set-key (kbd "C-c v v") #'macos-record-toggle)

;; Other bindings
(global-set-key (kbd "C-c v s") #'macos-speak)
(global-set-key (kbd "C-c v S") #'macos-speak-last-claude-response) ;; Speak Claude's response
(global-set-key (kbd "C-c v r") #'macos-record-toggle)  ;; Same as v v
(global-set-key (kbd "C-c v c") #'macos-voice-to-claude)
(global-set-key (kbd "C-c v p") #'macos-open-voice-prompt) ;; Open prompt buffer
(global-set-key (kbd "C-c v C") #'macos-start-voice-conversation) ;; Start conversation mode
(global-set-key (kbd "C-c v t") #'macos-voice-to-text)
(global-set-key (kbd "C-c v x") #'macos-voice-command)
(global-set-key (kbd "C-c v q") #'macos-stop-speech)
(global-set-key (kbd "C-c v ?") #'macos-check-speech-status)
(global-set-key (kbd "C-c v d") #'macos-test-full-loop)

(message "🎙️ Voice module ready!
  C-c v s - Speak text
  C-c v r - Record & insert
  C-c v c - Voice chat
  C-c v t - Voice to text
  C-c v d - Demo
  C-c v ? - Check status")

(provide 'macos-voice)

;;; macos-voice.el ends here

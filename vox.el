;;; vox.el --- Voice control for Emacs -*- lexical-binding: t; -*-

;; Author: Bret Horne
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.2") (cl-lib "0.5"))
;; Keywords: multimedia, macos, speech, voice

;;; Commentary:


;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'vox-cmd)

;;; Customization

(defgroup vox nil
  "Voice control system for Emacs."
  :group 'multimedia
  :prefix "vox-")


(defcustom vox-auto-stop-words '("done" "stop" "finish" "end")
  "Words that automatically stop voice recording."
  :type '(repeat string)
  :group 'vox)

(defcustom vox-process-delay 1.0
  "Delay in seconds before processing completed phrases."
  :type 'float
  :group 'vox)

(defcustom vox-polling-interval 0.1
  "Interval in seconds for polling voice recognition."
  :type 'float
  :group 'vox)

(defcustom vox-show-partial-transcription t
  "Whether to show partial transcription in echo area while listening."
  :type 'boolean
  :group 'vox)

(defcustom vox-transcription-prefix "[Listening] "
  "Prefix to show before partial transcription."
  :type 'string
  :group 'vox)

(defcustom vox-backend 'swift
  "Voice backend to use."
  :type '(choice (const :tag "Swift backend" swift)
                 (const :tag "Zig backend" zig))
  :group 'vox)

(defcustom vox-module-path "~/git/elisp/macos-module/.build/debug/libVox.dylib"
  "Path to the compiled vox module."
  :type 'string
  :group 'vox)

(defcustom vox-silence-timeout 5.0
  "Seconds of silence before automatically stopping voice recognition."
  :type 'float
  :group 'vox)

(defcustom vox-default-voice nil
  "Default voice identifier or nickname."
  :type '(choice (const :tag "System default" nil)
                 string)
  :group 'vox)

(defcustom vox-default-rate 0.5
  "Default speaking rate (0.0 to 1.0)."
  :type 'float
  :group 'vox)

(defcustom vox-default-pitch 1.0
  "Default pitch multiplier (0.5 to 2.0)."
  :type 'float
  :group 'vox)


;;; Backend System

(defclass vox-backend ()
  ()
  "Base class for voice backends.")

(cl-defgeneric vox-backend-initialize (backend)
  "Initialize BACKEND.")
(cl-defgeneric vox-backend-start-recording (backend)
  "Start recording with BACKEND.")
(cl-defgeneric vox-backend-stop-recording (backend)
  "Stop recording with BACKEND.")
(cl-defgeneric vox-backend-get-partial (backend)
  "Get partial transcription from BACKEND.")
(cl-defgeneric vox-backend-speak (backend text)
  "Speak TEXT using BACKEND.")
(cl-defgeneric vox-backend-stop-speech (backend)
  "Stop speech synthesis on BACKEND.")
(cl-defgeneric vox-backend-available-p (backend)
  "Check if BACKEND is available.")
(cl-defgeneric vox-backend-request-permission (backend)
  "Request permissions for BACKEND.")
(cl-defgeneric vox-backend-list-voices (backend)
  "List available voices for BACKEND.")
(cl-defgeneric vox-backend-speak-advanced (backend text voice rate pitch)
  "Speak TEXT with advanced parameters using BACKEND.")
(cl-defgeneric vox-backend-pause-speech (backend)
  "Pause speech synthesis on BACKEND.")
(cl-defgeneric vox-backend-continue-speech (backend)
  "Continue paused speech on BACKEND.")

;; Swift Backend
(defclass vox-backend-swift (vox-backend) ())

(cl-defmethod vox-backend-initialize ((backend vox-backend-swift))
  (unless (fboundp 'vox/start-recording)
    (let ((module-path (expand-file-name vox-module-path)))
      (when (file-exists-p module-path)
        (module-load module-path)))))

(cl-defmethod vox-backend-start-recording ((backend vox-backend-swift))
  (unless (fboundp 'vox/start-recording)
    (vox-backend-initialize backend)
    (unless (fboundp 'vox/start-recording)
      (error "Swift module not loaded. Run M-x vox-build-and-load")))
  (vox/start-recording))

(cl-defmethod vox-backend-stop-recording ((backend vox-backend-swift))
  (vox/stop-recording))

(cl-defmethod vox-backend-get-partial ((backend vox-backend-swift))
  (vox/get-current-transcription))

(cl-defmethod vox-backend-speak ((backend vox-backend-swift) text)
  (vox/speak text))

(cl-defmethod vox-backend-stop-speech ((backend vox-backend-swift))
  (vox/stop-speech))

(cl-defmethod vox-backend-available-p ((backend vox-backend-swift))
  (or (fboundp 'vox/start-recording)
      (file-exists-p (expand-file-name vox-module-path))))

(cl-defmethod vox-backend-request-permission ((backend vox-backend-swift))
  (vox/request-speech-permission))

(cl-defmethod vox-backend-list-voices ((backend vox-backend-swift))
  (vox/list-voices))

(cl-defmethod vox-backend-speak-advanced ((backend vox-backend-swift) text voice rate pitch)
  (vox/speak-advanced text (or voice "com.apple.voice.compact.en-US.Samantha") rate pitch))

(cl-defmethod vox-backend-pause-speech ((backend vox-backend-swift))
  (vox/pause-speech))

(cl-defmethod vox-backend-continue-speech ((backend vox-backend-swift))
  (vox/continue-speech))

;; Zig Backend
(defclass vox-backend-zig (vox-backend) ())

(cl-defmethod vox-backend-initialize ((backend vox-backend-zig))
  (let ((module-path (expand-file-name "zig-out/lib/libvox.dylib"
                                       (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-exists-p module-path)
      (ignore-errors (module-load module-path)))))

(cl-defmethod vox-backend-start-recording ((backend vox-backend-zig))
  (vox-start-recording))

(cl-defmethod vox-backend-stop-recording ((backend vox-backend-zig))
  (vox-stop-recording))

(cl-defmethod vox-backend-get-partial ((backend vox-backend-zig))
  (vox-process-audio)
  (vox-get-recognition-text))

(cl-defmethod vox-backend-speak ((backend vox-backend-zig) text)
  (vox-speak text))

(cl-defmethod vox-backend-stop-speech ((backend vox-backend-zig))
  (vox-stop))

(cl-defmethod vox-backend-available-p ((backend vox-backend-zig))
  (file-exists-p (expand-file-name "zig-out/lib/libvox.dylib"
                                   (file-name-directory (or load-file-name buffer-file-name)))))

(cl-defmethod vox-backend-request-permission ((backend vox-backend-zig))
  (vox-request-authorization))

;;; Voice System

(defclass vox-engine ()
  ((backend :initarg :backend :type vox-backend)
   (active :initform nil :type boolean)
   (insert-mode :initform nil :type boolean)
   (last-text :initform "" :type string)
   (processed-text :initform "" :type string)
   (last-change-time :initform 0 :type number)
   (process-timer :initform nil)
   (tts-buffer :initform "" :type string)
   (tts-position :initform 0 :type number)
   (tts-paused :initform nil :type boolean)
   (last-silence-time :initform 0 :type number))
  "Unified voice engine.")

(cl-defmethod vox-engine-start ((engine vox-engine))
  (with-slots (backend active process-timer) engine
    (when active
      (error "Voice already active"))
    (setf active t
          (slot-value engine 'last-text) ""
          (slot-value engine 'processed-text) ""
          (slot-value engine 'last-change-time) 0
          (slot-value engine 'insert-mode) nil)
    (vox-backend-start-recording backend)
    (setf process-timer
          (run-with-timer vox-polling-interval vox-polling-interval
                          (lambda () (vox-engine--process engine))))
    (message "Voice active. Say %s to stop." (mapconcat #'identity vox-auto-stop-words " or "))))

(cl-defmethod vox-engine-stop ((engine vox-engine))
  (with-slots (backend active process-timer) engine
    (setf active nil)
    (when process-timer
      (cancel-timer process-timer)
      (setf process-timer nil))
    (vox-backend-stop-recording backend)
    (message "Voice stopped.")))

(cl-defmethod vox-engine--process ((engine vox-engine))
  (with-slots (backend active last-text last-change-time processed-text last-silence-time) engine
    (when active
      (let ((text (vox-backend-get-partial backend)))
        (if (and text (not (string-empty-p text)))
            (progn
              (setf last-silence-time 0)

              (when (and vox-show-partial-transcription
                         (not (string= text last-text)))
                (message "%s%s" vox-transcription-prefix text))

              (when (not (string= text last-text))
                (setf last-text text
                      last-change-time (float-time)))

              (when (and (> last-change-time 0)
                         (> (- (float-time) last-change-time) vox-process-delay)
                         (not (string= text processed-text)))
                (setf processed-text text)
                (when vox-show-partial-transcription
                  (message ""))
                (vox-engine--handle-phrase engine text)
                (setf last-text ""
                      last-change-time 0)))

          (progn
            (when (= last-silence-time 0)
              (setf last-silence-time (float-time)))
            (when (> (- (float-time) last-silence-time) vox-silence-timeout)
              (message "Voice stopped due to silence timeout")
              (vox-engine-stop engine))))))))

(cl-defmethod vox-engine--handle-phrase ((engine vox-engine) phrase)
  "Handle recognized phrase by delegating to command system."
  (with-slots (insert-mode) engine
    (let ((cleaned (string-trim (downcase phrase))))
      (cond
       ((cl-some (lambda (word) (string-match-p (regexp-quote word) cleaned)) vox-auto-stop-words)
        (setf insert-mode nil)
        (vox-engine-stop engine))

       (t
        ;; Delegate to command system if available
        (if (fboundp 'vox-cmd-process-phrase)
            (vox-cmd-process-phrase phrase engine)
          (message "[Voice] Command system not loaded: %s" phrase)))))))

(cl-defmethod vox-engine-speak ((engine vox-engine) text &optional voice rate pitch)
  "Speak TEXT using the engine's TTS buffer."
  (with-slots (backend tts-buffer tts-position tts-paused) engine
    (setf tts-buffer text
          tts-position 0
          tts-paused nil)
    (if (or voice rate pitch)
        (vox-backend-speak-advanced backend text voice rate pitch)
      (vox-backend-speak backend text))))

(cl-defmethod vox-engine-pause-speech ((engine vox-engine))
  "Pause speech and mark position in TTS buffer."
  (with-slots (backend tts-paused) engine
    (setf tts-paused t)
    (vox-backend-pause-speech backend)))

(cl-defmethod vox-engine-continue-speech ((engine vox-engine))
  "Continue speech from current position."
  (with-slots (backend tts-paused) engine
    (setf tts-paused nil)
    (vox-backend-continue-speech backend)))

(cl-defmethod vox-engine-stop-speech ((engine vox-engine))
  "Stop speech and clear TTS buffer."
  (with-slots (backend tts-buffer tts-position tts-paused) engine
    (setf tts-buffer ""
          tts-position 0
          tts-paused nil)
    (vox-backend-stop-speech backend)))

;;; Global State

(defvar vox--backend-registry
  '((swift . vox-backend-swift)
    (zig . vox-backend-zig)))

(defvar vox--engine nil)


;;;###autoload
(defun vox-initialize (&optional backend-sym)
  "Initialize the Vox voice system with BACKEND-SYM."
  (interactive)
  (let* ((backend-sym (or backend-sym
                          (if (boundp 'vox-backend) vox-backend 'swift)))
         (backend-class (alist-get backend-sym vox--backend-registry)))
    (unless backend-class
      (error "Unknown backend: %s" backend-sym))
    (let ((backend (make-instance backend-class)))
      (unless (vox-backend-available-p backend)
        (error "Backend %s not available" backend-sym))
      (vox-backend-initialize backend)
      (setq vox--engine (make-instance 'vox-engine :backend backend))
      (message "Vox initialized with %s backend" backend-sym))))

;;;###autoload
(defun vox-start ()
  "Start voice recognition."
  (interactive)
  (unless vox--engine
    (vox-initialize))
  (vox-engine-start vox--engine))

;;;###autoload
(defun vox-stop ()
  "Stop voice recognition."
  (interactive)
  (when vox--engine
    (vox-engine-stop vox--engine)))

;;;###autoload
(defun vox-toggle ()
  "Toggle voice recognition on/off."
  (interactive)
  (if (and vox--engine (slot-value vox--engine 'active))
      (vox-stop)
    (vox-start)))

;;;###autoload
(defun vox-say (text)
  "Speak TEXT using voice synthesis."
  (interactive "sSpeak: ")
  (unless vox--engine
    (vox-initialize))
  (vox-engine-speak vox--engine text))

;;;###autoload
(defun vox-request-permission ()
  "Request speech recognition permission."
  (interactive)
  (unless vox--engine
    (vox-initialize))
  (vox-backend-request-permission (slot-value vox--engine 'backend)))

;;;###autoload
(defun vox-build-swift ()
  "Build the Swift backend module."
  (interactive)
  (let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
         (default-directory module-dir))
    (message "Building Swift module in %s..." module-dir)
    (compile "swift build")))

;;;###autoload
(defun vox-build-zig ()
  "Build the Zig backend module."
  (interactive)
  (let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
         (default-directory module-dir))
    (message "Building Zig module in %s..." module-dir)
    (compile "zig build")))

;;;###autoload
(defun vox-build (&optional backend-sym)
  "Build the backend module for BACKEND-SYM (defaults to `vox-backend`)."
  (interactive)
  (let ((backend-sym (or backend-sym vox-backend)))
    (pcase backend-sym
      ('swift (vox-build-swift))
      ('zig (vox-build-zig))
      (_ (error "Unknown backend: %s" backend-sym)))))

;;;###autoload
(defun vox-build-and-load (&optional backend-sym)
  "Build and load the backend module for BACKEND-SYM."
  (interactive)
  (let* ((backend-sym (or backend-sym vox-backend))
         (module-dir (file-name-directory (or load-file-name buffer-file-name)))
         (default-directory module-dir))
    (message "Building %s module..." backend-sym)
    (let ((build-command (pcase backend-sym
                           ('swift "swift build")
                           ('zig "zig build")
                           (_ (error "Unknown backend: %s" backend-sym)))))
      (if (zerop (shell-command build-command))
          (progn
            (message "Build successful, loading module...")
            (vox-initialize backend-sym))
        (error "Build failed. Check *Shell Command Output* buffer")))))

;;;###autoload
(defun vox-speak-advanced (text &optional voice rate pitch)
  "Speak TEXT with advanced parameters: VOICE, RATE, and PITCH."
  (interactive "sText to speak: ")
  (unless vox--engine
    (vox-initialize))
  (let ((voice (or voice vox-default-voice))
        (rate (or rate vox-default-rate))
        (pitch (or pitch vox-default-pitch)))
    (vox-engine-speak vox--engine text voice rate pitch)))

;;;###autoload
(defun vox-list-voices ()
  "List available voices in a buffer."
  (interactive)
  (unless vox--engine
    (vox-initialize))
  (let ((voices (vox-backend-list-voices (slot-value vox--engine 'backend))))
    (with-current-buffer (get-buffer-create "*Vox Voices*")
      (erase-buffer)
      (insert "Available Voices:\n\n")
      (dolist (voice voices)
        (insert (format "%-40s %s (%s)\n"
                        (alist-get "name" voice nil nil #'string=)
                        (alist-get "language" voice nil nil #'string=)
                        (alist-get "quality" voice nil nil #'string=))))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun vox-pause ()
  "Pause speech synthesis."
  (interactive)
  (when vox--engine
    (vox-engine-pause-speech vox--engine)))

;;;###autoload
(defun vox-continue ()
  "Continue paused speech synthesis."
  (interactive)
  (when vox--engine
    (vox-engine-continue-speech vox--engine)))

;;;###autoload
(defun vox-stop-speech ()
  "Stop speech synthesis."
  (interactive)
  (when vox--engine
    (vox-engine-stop-speech vox--engine)))

(provide 'vox)
;;; vox.el ends here

;;; vox-cmd.el --- Voice command system for Vox -*- lexical-binding: t; -*-

;; Author: Bret Horne
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.2") (cl-lib "0.5") (vox "0.2.0"))
;; Keywords: multimedia, voice, commands

;;; Commentary:

;; Voice command processing system for Vox.

;;; Code:

(require 'cl-lib)

(defgroup vox-cmd nil
  "Voice command configuration for Vox."
  :group 'vox)

;;; Customization

(defcustom vox-cmd-commands
  '(("other window" . other-window)
    ("split window" . split-window-right)
    ("delete window" . delete-window)
    ("kill buffer" . kill-buffer)
    ("switch buffer" . switch-to-buffer)
    ("find file" . find-file)
    ("save" . save-buffer))
  "Voice command mappings."
  :type '(alist :key-type string :value-type function)
  :group 'vox-cmd)

;;; Command Processing

(defun vox-cmd-process-phrase (phrase engine)
  "Process PHRASE as a voice command for ENGINE."
  (with-slots (insert-mode) engine
    (let ((cleaned (string-trim (downcase phrase))))
      (message "[Voice] Processing: '%s'" cleaned)

      (cond
       (insert-mode
        (if (string= cleaned "command mode")
            (progn
              (setf insert-mode nil)
              (message "[Command mode] Ready for commands"))
          (unless buffer-read-only
            (insert phrase)
            (message "[Inserted] %s" phrase))))

       ((string= cleaned "insert")
        (setf insert-mode t)
        (message "[Insert mode] Say text to insert"))

       (t
        (let ((command-entry (assoc cleaned vox-cmd-commands)))
          (if command-entry
              (let ((cmd (cdr command-entry)))
                (message "[Voice] Executing: %s" cmd)
                (condition-case err
                    (if (commandp cmd)
                        (call-interactively cmd)
                      (funcall cmd))
                  (error (message "Error: %s" err))))
            (let ((cmd (intern (replace-regexp-in-string " " "-" cleaned))))
              (if (commandp cmd)
                  (progn
                    (message "[Voice] Executing: %s" cmd)
                    (condition-case err
                        (call-interactively cmd)
                      (error (message "Error: %s" err))))
                (message "[Voice] No command for: '%s'" cleaned))))))))))

(provide 'vox-cmd)
;;; vox-cmd.el ends here

;;; macmod-build.el --- Build helper for macOS module -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional build helper that can compile the Swift module from Emacs.

;;; Code:

(require 'macmod-core)

(defun macmod-build ()
  "Build the Swift module."
  (interactive)
  (let* ((module-dir (file-name-directory
                      (or load-file-name buffer-file-name)))
         (default-directory module-dir)
         (build-buffer "*macmod-build*"))
    (message "Building Swift module in %s..." module-dir)
    (compile "swift build" 'nil)
    (with-current-buffer "*compilation*"
      (rename-buffer build-buffer t))
    (message "Building... check %s buffer for progress" build-buffer)))

(defun macmod-build-and-load ()
  "Build the Swift module and load it when complete."
  (interactive)
  (let* ((module-dir (file-name-directory
                      (or load-file-name buffer-file-name)))
         (default-directory module-dir))
    (message "Building Swift module...")
    (if (zerop (shell-command "swift build"))
        (progn
          (message "Build successful, loading module...")
          (macmod-load))
      (error "Build failed. Check *Shell Command Output* buffer"))))

(defun macmod-clean-build ()
  "Clean and rebuild the Swift module."
  (interactive)
  (let* ((module-dir (file-name-directory
                      (or load-file-name buffer-file-name)))
         (default-directory module-dir))
    (message "Cleaning build directory...")
    (shell-command "rm -rf .build")
    (macmod-build)))

(provide 'macmod-build)
;;; macmod-build.el ends here
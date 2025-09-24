;;; macmod-core.el --- Core functionality for macOS module -*- lexical-binding: t; -*-

;;; Commentary:
;; Core loading and initialization for the macOS Swift module.
;; This provides the foundation for TTS, STT, and other macOS integrations.

;;; Code:

(defgroup macmod nil
  "macOS Swift module integration."
  :group 'multimedia
  :prefix "macmod-")

(defcustom macmod-module-path
  (expand-file-name ".build/arm64-apple-macosx/debug/libEmacsMacOSModule.dylib"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the compiled Swift module dylib."
  :type 'file
  :group 'macmod)

(defvar macmod-loaded-p nil
  "Whether the macOS module has been loaded.")

(defun macmod-load ()
  "Load the macOS Swift module.
User should call this after building the Swift module."
  (interactive)
  (if macmod-loaded-p
      (message "macOS module already loaded")
    (if (file-exists-p macmod-module-path)
        (progn
          (module-load macmod-module-path)
          (setq macmod-loaded-p t)
          (message "macOS module loaded from %s" macmod-module-path))
      (error "Module not found at %s. Run 'swift build' in %s first"
             macmod-module-path
             (file-name-directory macmod-module-path)))))

(defun macmod-loaded-functions ()
  "List all loaded macmod/ functions."
  (let ((funcs '()))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (string-prefix-p "macmod/" (symbol-name sym)))
         (push sym funcs))))
    (sort funcs (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'macmod-core)
;;; macmod-core.el ends here
;;; macmod-core.el --- Core functionality for macOS module -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2"))
;; Keywords: multimedia, macos, speech
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

;; Core loading and initialization for the macOS Swift module.
;; This provides the foundation for TTS, STT, and other macOS integrations.

;;; Code:

(defgroup macmod nil
  "macOS Swift module integration."
  :group 'multimedia
  :prefix "macmod-")

(defcustom macmod-module-path
  (expand-file-name ".build/arm64-apple-macosx/debug/libMacmod.dylib"
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

(defun macmod--loaded-functions ()
  "List all loaded macmod/ functions (internal use)."
  (let ((funcs '()))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (string-prefix-p "macmod/" (symbol-name sym)))
         (push sym funcs))))
    (sort funcs (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun macmod-loaded-functions ()
  "Return a list of all loaded macmod functions."
  (macmod--loaded-functions))

(provide 'macmod-core)
;;; macmod-core.el ends here
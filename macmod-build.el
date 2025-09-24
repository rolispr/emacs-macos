;;; macmod-build.el --- Build helper for macOS module -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2"))
;; Keywords: tools, build
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

;; Optional build helper that can compile the Swift module from Emacs.

;;; Code:

(require 'macmod-core)

(defun macmod-build ()
  "Build the Swift module using swift build."
  (interactive)
  (let ((module-dir (file-name-directory
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
  (let ((module-dir (file-name-directory
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
  (let ((module-dir (file-name-directory
                     (or load-file-name buffer-file-name)))
        (default-directory module-dir))
    (message "Cleaning build directory...")
    (shell-command "rm -rf .build")
    (macmod-build)))

(provide 'macmod-build)
;;; macmod-build.el ends here
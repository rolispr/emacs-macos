;;; macmod.el --- macOS Swift module for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (cl-lib "0.5"))
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

;; Main entry point for the macOS module package.
;; Provides a unified interface to all module functionality.

;;; Code:

(require 'macmod-core)
(require 'macmod-tts)
(require 'macmod-stt)
(require 'macmod-voice)

(defalias 'macmod-speak 'macmod-tts-speak
  "Speak text using the TTS system.")
(defalias 'macmod-stop 'macmod-tts-stop
  "Stop ongoing speech synthesis.")
(defalias 'macmod-voices 'macmod-tts-list-voices
  "List available system voices.")

(defun macmod-status ()
  "Show macOS module status in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*macOS Module Status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "macOS Module Status\n")
      (insert "===================\n\n")

      (insert (format "Module loaded: %s\n"
                      (if macmod-loaded-p "✓" "✗")))

      (when macmod-loaded-p
        (insert "\nAvailable functions:\n")
        (dolist (func (macmod-loaded-functions))
          (insert (format "  • %s\n" func))))

      (insert "\nTTS Configuration:\n")
      (insert (format "  Default rate: %.2f\n" macmod-tts-default-rate))
      (insert (format "  Default pitch: %.2f\n" macmod-tts-default-pitch))
      (insert (format "  Pronunciations: %d rules\n"
                      (length macmod-tts-pronunciations)))

      (insert "\nSTT Status:\n")
      (insert (format "  Recording: %s\n"
                      (if macmod-stt-recording-p "yes" "no")))
      (when macmod-stt-last-transcription
        (insert (format "  Last transcription: %s\n"
                        macmod-stt-last-transcription))))
    (display-buffer buffer)))

(provide 'macmod)
;;; macmod.el ends here
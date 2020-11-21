;;; mediainfo-mode.el --- Show mediainfo data when opening media file in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <>
;; Keywords: matching

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

;; Show mediainfo data when opening media file in Emacs
;;
;; Call `TORRENT-MODE-SETUP'.

;;; Code:

(defvar mediainfo-mode-command "mediainfo %s"
  "The shell command to use for `mediainfo-mode'.")

(defun mediainfo-mode-revert-buffer ()
  (interactive)
  (when (eq 'mediainfo-mode major-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (shell-command-to-string
        (format mediainfo-mode-command
                (shell-quote-argument (buffer-file-name)))))
      (set-buffer-modified-p nil))))

;;;###autoload
(define-derived-mode mediainfo-mode special-mode "Mediainfo"
  (mediainfo-mode-revert-buffer)
  (read-only-mode))

;;;###autoload
(defun mediainfo-mode-setup ()
  "Make `mediainfo-mode' get called automatically for mediainfo files."
  (add-to-list
   'auto-mode-alist
   (cons
    (rx "."
        (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov"
            "3gp" "vob" "wmv" "aiff" "wav")
        eos)
    'mediainfo-mode)))

(provide 'mediainfo-mode)
;;; mediainfo-mode.el ends here

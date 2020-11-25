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
;; Call `MEDIAINFO-MODE-SETUP'.

;;; Code:


;;;; REQUIRES

(require 'rx)


;;;; VARIABLES

(defvar mediainfo-mode-command "mediainfo %s"
  "The shell command to use for `mediainfo-mode'.")

(defvar mediainfo-mode-file-regexp
  (rx "."
      (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov"
          "3gp" "vob" "wmv" "aiff" "wav")
      eos)
  "A regexp used to distinguish mediainfo-supported files.")


;;;; FUNCTIONS

(defun mediainfo-mode--run-command (filename)
  "Run medainfo command on `FILENAME' and return a result string."
  (shell-command-to-string
   (format mediainfo-mode-command (shell-quote-argument filename))))

(defun mediainfo-mode--file-handler (operation &rest args)
  "A special file handler for mediainfo.
Apply `INSERT-FILE-CONTENTS' `OPERATION' on `ARGS'."
  (cl-case operation
    (insert-file-contents
     (cl-destructuring-bind (filename &optional visit beg end replace) args
       (when (or beg end)
         (error "Beginning and end does not make sense for mediainfo"))
       (when (and visit (/= 0 (buffer-size)) (not replace))
         (error "Cannot visit non-empty buffer"))
       (let* ((filename (expand-file-name filename))
              (mediainfo-string (mediainfo-mode--run-command filename)))
         (if replace
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert mediainfo-string))
           (insert mediainfo-string))
         (when visit
           (set-visited-file-name filename)
           (set-visited-file-modtime (current-time)))
         (list filename (length mediainfo-string)))))
    (t
     (let ((inhibit-file-name-handlers
            (cons 'mediainfo-mode--file-handler
                  (and (eq inhibit-file-name-operation operation)
                       inhibit-file-name-handlers)))
           (inhibit-file-name-operation operation))
       (apply operation args)))))


;;;; COMMANDS

;;;###autoload
(define-derived-mode mediainfo-mode special-mode "Mediainfo"
  (read-only-mode))

;;;###autoload
(defun mediainfo-mode-setup ()
  "Make `mediainfo-mode' get called automatically for mediainfo files."
  (interactive)

  (add-to-list
   'file-name-handler-alist
   (cons mediainfo-mode-file-regexp 'mediainfo-mode--file-handler))

  (add-to-list
   'auto-mode-alist
   (cons mediainfo-mode-file-regexp 'mediainfo-mode)))

(provide 'mediainfo-mode)
;;; mediainfo-mode.el ends here

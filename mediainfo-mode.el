;;; mediainfo-mode.el --- Show mediainfo data when opening media file -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
;; Keywords: multimedia
;; Version: 0.1.0
;; URL: https://github.com/xFA25E/mediainfo-mode
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Show mediainfo data when opening media file in Emacs
;;
;; Call `MEDIAINFO-MODE-SETUP'.

;;; Code:


;;;; REQUIRES

(require 'rx)
(require 'cl-lib)
(require 'font-lock)
(require 'simple)
(require 'imenu)


;;;; VARIABLES

(defvar mediainfo-mode--font-lock-defaults
  `(;; Sections
    (,(rx bol (+ (not (any ":" "\n"))) eol)
     . font-lock-function-name-face)
    ;; Fields
    (,(rx bol (group (+? any)) (+ space) ":")
     . (1 font-lock-variable-name-face))
    ;; Names
    (,(rx bol (+? any) (+ space) ": " (group (*? any)) eol)
     . (1 font-lock-constant-face)))
  "`MEDIAINFO-MODE' font-lock defaults.")

(defvar mediainfo-mode--imenu-generic-expression
  `(("Sections" ,(rx bol (+ (not (any ":" "\n"))) eol) 0)
    ("Fields" ,(rx bol (group (+? any)) (+ space) ":") 1))
  "Generic `MEDIAINFO-MODE' expression for imenu.")


;;;; CUSTOM

(defgroup mediainfo-mode nil
  "View mediainfo files"
  :group 'applications)

(defcustom mediainfo-mode-command
  "mediainfo %s"
  "The shell command to use for `mediainfo-mode'."
  :type 'string
  :group 'mediainfo-mode)

(defcustom mediainfo-mode-file-regexp
  (rx "."
      (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi"
          "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav")
      eos)
  "A regexp used to distinguish mediainfo-supported files."
  :type 'string
  :group 'mediainfo-mode)


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
     (cl-destructuring-bind (filename &optional visit beg end replace)
         args
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

(defun mediainfo-mode--get-ffmpeg-info (file)
  "Get information about `FILE' from ffmpeg."
  (string-trim
   (shell-command-to-string (format "ffmpeg -i %s" file))))

(defun mediainfo-mode--own-thumbnail-p (file)
  "Check if `FILE' already have buit-in thumbnail."
  ;;; Usually stream with thumbnail have this property
  ;;; TODO Find common pattern for all streams with thumbnail
  (s-contains? "Video: mjpeg (Baseline)"
               (mediainfo-mode--get-ffmpeg-info file)))

(defun mediainfo-mode--get-own-thumbnail (file output)
  "Get built-in thumbnail `FILE' from STREAM to `OUTPUT'."
  (call-process-shell-command
   (format "ffmpeg -i %s -map 0:%s -c copy %s"
           file
           ;;; 2 is a Stream from ffmpeg info
           ;;; TODO Add autodetect stream with thumbnail, not hardcode
           2
           output) nil 0)
  output)

(defun mediainfo-mode--get-middle-time (file)
  "Get middle in time of the `FILE'."
  (string-trim
   (shell-command-to-string
    (format "ffmpeg -i %s 2>&1 | grep Duration | awk '{print $2}' | tr -d , | awk -F ':' '{print ($3+$2*60+$1*3600)/2}'"
            file))))

(defun mediainfo-mode--get-thumbnail-from-middle (file output)
  "Save thumbnail from middle of `FILE' if it's video to `OUTPUT'."
  (call-process-shell-command
    (format "ffmpeg -ss %s -i %s -vframes 1 -vcodec png %s"
            (mediainfo-mode--get-middle-time file) file output) nil 0)
  output)


;;;; COMMANDS

;;;###autoload
(define-derived-mode mediainfo-mode special-mode "Mediainfo"
  (setq-local font-lock-defaults '(mediainfo-mode--font-lock-defaults))
  (setq imenu-generic-expression mediainfo-mode--imenu-generic-expression)
  (read-only-mode)
  (goto-char (point-min)))


;;; It's very trash code, but it works!
;;; TODO Get sha sum entierly from file, not from filename
;;; TODO Add custom folder for cache thumbnail in defcustom
;;; TODO Extend this logic for audio file's too
;;;###autoload
(defun mediainfo-mode-get-thumbnail (file)
  "Return image object from given media `FILE'."
  (let ((output (concat "~/" (md5 file) ".png")))
        (create-image (if (mediainfo-mode--own-thumbnail-p file)
                          (mediainfo-mode--get-own-thumbnail file output)
                        (mediainfo-mode--get-thumbnail-from-middle file output))
                      ;;; image type (auto detect from first bytes)
                      nil
                      ;;; image data (hz chto eto)
                      nil
                      :scale 0.2)))

;;;###autoload
(defun mediainfo-mode-setup ()
  "Initialize `mediainfo-mode'."
  (interactive)

  (add-to-list
   'file-name-handler-alist
   (cons mediainfo-mode-file-regexp 'mediainfo-mode--file-handler))

  (add-to-list
   'auto-mode-alist
   (cons mediainfo-mode-file-regexp 'mediainfo-mode)))

(provide 'mediainfo-mode)
;;; mediainfo-mode.el ends here

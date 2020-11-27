;;; mediainfo-mode.el --- Show mediainfo data when opening media file -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
;; Keywords: multimedia
;; Version: 0.2.0
;; URL: https://github.com/xFA25E/mediainfo-mode
;; Package-Requires: ((emacs "26.1"))

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
;; Call mediainfo-mode-setup.
;;
;; Features: imenu, thumbnails

;;; Code:


;;;; REQUIRES

(require 'rx)
(require 'cl-lib)
(require 'font-lock)
(require 'simple)
(require 'image)
(require 'xdg)
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

(defgroup mediainfo nil
  "View mediainfo files"
  :group 'applications)

(defcustom mediainfo-mode-command
  "mediainfo %s"
  "The shell command to use for `mediainfo-mode'."
  :type 'string
  :group 'mediainfo)

(defcustom mediainfo-mode-file-regexp
  (rx "."
      (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi"
          "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav")
      eos)
  "A regexp used to distinguish mediainfo-supported files."
  :type 'regexp
  :group 'mediainfo)

(defcustom mediainfo-mode-thumbnail-cache-directory
  (expand-file-name "emacs/mediainfo-thumbnails" (xdg-cache-home))
  "Cache directory used to store generated thumbnails for media files."
  :type 'directory
  :group 'mediainfo)


;;;; FUNCTIONS

(defun mediainfo-mode--run-command (filename)
  "Run medainfo command on `FILENAME' and return a result string."
  (shell-command-to-string
   (format mediainfo-mode-command (shell-quote-argument filename))))

(defun mediainfo-mode--draw (file mediainfo-string)
  "Insert `FILE's thumbnail and `MEDIAINFO-STRING'."
  (when-let ((thumbnail (ignore-errors
                          (mediainfo-mode--get-thumbnail file))))
    (insert-image thumbnail)
    (insert "\n"))
  (insert mediainfo-string))

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
               (mediainfo-mode--draw filename mediainfo-string))
           (mediainfo-mode--draw filename mediainfo-string))
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

;;;;; THUMBNAILS

(defun mediainfo-mode--thumbnail-cache-path (file)
  "Get thumbnail cache path for a media `FILE'."
  (expand-file-name
   (concat (md5 file) ".png") mediainfo-mode-thumbnail-cache-directory))

(defun mediainfo-mode--resize-thumbnail (file)
  "Resize thumbnail `FILE'."
  (when (executable-find "convert")
    (call-process "convert" nil nil nil file "-resize" "x400" file)))

(defun mediainfo-mode--has-thumbnail-p (file)
  "Check if `FILE' already have buit-in thumbnail.
Return thumbnail stream."
  (ignore-errors
    (with-temp-buffer
      (call-process "ffmpeg" nil t nil "-i" file)
      (goto-char (point-min))
      (search-forward-regexp
       (rx "Stream" (+ space) "#"
           (group (+ num) ":" (+ num))
           (* (not ":")) ":" (+ space)
           "Video:" (+ space) "mjpeg"))
      (match-string 1))))

(defun mediainfo-mode--extract-video-thumbnail (stream file output)
  "Get built-in thumbnail `FILE' from `STREAM' to `OUTPUT'."
  (make-directory (file-name-directory output) t)
  (unless (and (equal 0 (call-process
                         "ffmpeg" nil nil nil
                         "-i" file "-map" stream "-c" "copy" output))
               (file-exists-p output))
    (error "Ffmpeg failed to extract a video thumbnail"))
  (mediainfo-mode--resize-thumbnail output))

(defun mediainfo-mode--get-video-duration (file)
  "Get video duration from `FILE'."
  (with-temp-buffer
    (call-process "ffmpeg" nil t nil "-i" file)
    (goto-char (point-min))
    (search-forward-regexp
     (rx "Duration:" (* space) (group (+ num)) ":" (group (+ num)) ":" (group (+ num))))
    (+ (* 3600 (string-to-number (match-string 1)))
       (* 60 (string-to-number (match-string 2)))
       (string-to-number (match-string 3)))))

(defun mediainfo-mode--extract-video-frame-at (time file output)
  "Save video frame at `TIME' of video `FILE' to `OUTPUT'."
  (make-directory (file-name-directory output) t)
  (unless (and (equal 0 (call-process
                         "ffmpeg" nil nil nil
                         "-ss" (number-to-string time)
                         "-i" file
                         "-vframes" "1" "-vcodec" "png"
                         output))
               (file-exists-p output))
    (error "Ffmpeg failed to extract a video frame"))
  (mediainfo-mode--resize-thumbnail output))

(defun mediainfo-mode--get-thumbnail (file)
  "Return image object from given media `FILE'."
  (unless (and (string-match-p mediainfo-mode-file-regexp file)
               (file-exists-p file))
    (error "Not a valid media file"))

  (let* ((file (expand-file-name file))
         (output (mediainfo-mode--thumbnail-cache-path file)))
    (if (file-exists-p output)
        (create-image output)
      (if-let ((stream (mediainfo-mode--has-thumbnail-p file)))
          (progn
            (mediainfo-mode--extract-video-thumbnail stream file output)
            (create-image output))
        (let ((duration (mediainfo-mode--get-video-duration file)))
          (mediainfo-mode--extract-video-frame-at (/ duration 2) file output)
          (create-image output))))))


;;;; COMMANDS

;;;###autoload
(define-derived-mode mediainfo-mode special-mode "Mediainfo"
  "A mode for showing mediafile contents."
  (goto-char (point-min))
  (setq-local font-lock-defaults '(mediainfo-mode--font-lock-defaults))
  (setq imenu-generic-expression mediainfo-mode--imenu-generic-expression))

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

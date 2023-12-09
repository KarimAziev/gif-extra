;;; gif-extra.el --- Extra utils for gif-screencast -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gif-extra
;; Version: 0.1.0
;; Keywords: tools, multimedia
;; Package-Requires: ((emacs "26.1") (gif-screencast "1.2"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extra utils for gif-screencast

;;; Code:



(require 'gif-screencast)

(defcustom gif-extra-outfile-base-name "demo-%F-%T.gif"
  "Base name for outfile."
  :type 'string
  :group 'gif-extra)


(defcustom gif-extra-minimal-autoopen-duration 3
  "Minimal duration in seconds for auto opening recorded gifs."
  :group 'gif-extra
  :type 'integer)

(defcustom gif-extra-allow-mode-line-indicator t
  "Whether to show countdown and duration in mode-line."
  :group 'gif-extra
  :type 'boolean)

(defvar gif-extra-seconds-timer nil)
(defvar gif-extra-duration-seconds 0)
(defvar gif-extra-countdown-timer nil)
(defvar gif-extra-countdown 0)

(defun gif-extra-read-directory (prompt)
  "Read directory with PROMPT."
  (let*
      ((other-dir "*other directory*")
       (dir
        (completing-read prompt
                         (append (list other-dir)
                                 (mapcar #'abbreviate-file-name
                                         (gif-extra-get-active-buffers-dirs))))))
    (if (string= dir other-dir)
        (read-directory-name prompt)
      dir)))

(defun gif-extra-get-active-buffers-dirs ()
  "Return default directories from all buffers."
  (let ((curr-buf (current-buffer)))
    (seq-sort-by (lambda (it)
                   (cond ((equal it default-directory)
                          3)
                         (t (if (get-buffer-window it)
                                (if (eq curr-buf it)
                                    1
                                  2)
                              -1))))
                 '>
                 (delete-dups (mapcar (lambda (buff)
                                        (when-let ((dir (buffer-local-value
                                                         'default-directory
                                                         buff)))
                                          (unless (file-remote-p dir)
                                            (expand-file-name dir))))
                                      (buffer-list))))))

(defvar gif-extra-out-file nil)
(defvar gif-extra-out-record-time nil)
(defvar gif-extra-duration
  nil)

(defun gif-extra-record-duration ()
  "Set duration of gif recording."
  (if gif-screencast-mode
      (setq gif-extra-out-record-time (current-time))
    (setq gif-extra-duration
          (when gif-extra-out-record-time
            (round (- (time-to-seconds (time-since gif-extra-out-record-time))
                      gif-screencast-countdown))))))

;;;###autoload
(defun gif-extra-record-out-file ()
  "Set `gif-extra-out-file'."
  (interactive)
  (setq gif-extra-out-file
        (expand-file-name
         (if
             (string-match-p "%[a-z]"
                             gif-extra-outfile-base-name)
             (format-time-string
              gif-extra-outfile-base-name
              (current-time))
           gif-extra-outfile-base-name)
         (or (and (file-writable-p gif-screencast-output-directory)
                  gif-screencast-output-directory)
             (gif-extra-read-directory
              "Save output to directory: ")))))

(defun gif-extra-screencast--generate-gif (process event)
  "Generate GIF file from PROCESS and EVENT.."
  (when process
    (gif-screencast-print-status process event))
  (message "Compiling GIF with %s..." gif-screencast-convert-program)
  (let* ((output-filename
          (or gif-extra-out-file
              (setq gif-extra-out-file
                    (expand-file-name
                     (if
                         (string-match-p "%[a-z]"
                                         gif-extra-outfile-base-name)
                         (format-time-string
                          gif-extra-outfile-base-name
                          (current-time))
                       gif-extra-outfile-base-name)
                     (or (and (file-writable-p gif-screencast-output-directory)
                              gif-screencast-output-directory)
                         (gif-extra-read-directory
                          "Save output to directory: "))))))
         (delays
          (cl-loop for (this-frame next-frame . _)
                   on gif-screencast--frames
                   by #'cdr ;; Converters delays are expressed in centiseconds.
                   for delay =
                   (when next-frame
                     (format "%d"
                             (* 100
                                (float-time
                                 (time-subtract (gif-screencast-frame-timestamp
                                                 next-frame)
                                                (gif-screencast-frame-timestamp
                                                 this-frame))))))
                   when next-frame
                   collect delay))
         (delays (cons gif-screencast-first-delay delays))
         (files-args (cl-loop for frame in gif-screencast--frames
                              for delay in delays
                              append (list "-delay" delay (gif-screencast-frame-filename
                                                           frame))))
         (convert-args (append gif-screencast-convert-args
                               files-args
                               (list output-filename)))
         (convert-process (gif-screencast--start-process
                           gif-screencast-convert-program
                           convert-args)))
    (set-process-sentinel convert-process (lambda (process event)
                                            (gif-screencast-print-status process
                                                                         event)
                                            (when (and
                                                   gif-screencast-want-optimized
                                                   (eq (process-status
                                                        process)
                                                       'exit)
                                                   (= (process-exit-status
                                                       process)
                                                      0))
                                              (if gif-screencast-want-optimized
                                                  (gif-extra-screencast-optimize
                                                   output-filename)
                                                (gif-extra-process-file
                                                 output-filename)))
                                            (when (and
                                                   gif-screencast-autoremove-screenshots
                                                   (eq (process-status
                                                        process)
                                                       'exit)
                                                   (zerop (process-exit-status
                                                           process)))
                                              (dolist (f gif-screencast--frames)
                                                (delete-file (gif-screencast-frame-filename
                                                              f))))))))

(defun gif-extra-screencast-optimize (file)
  "Optimize GIF FILE asynchronously."
  (message "Optimizing with %s..." gif-screencast-optimize-program)
  (let ((p (gif-screencast--start-process
            gif-screencast-optimize-program
            (append
             gif-screencast-optimize-args
             (list file)))))
    (set-process-sentinel p (lambda (process event)
                              (gif-screencast-print-status
                               process event)
                              (gif-extra-process-file file)))))

(defun gif-extra-outfile-open-file (file)
  "Browse FILE with xwidgets or `browse-url'."
  (if (and (file-exists-p file)
           (display-graphic-p)
           (featurep 'xwidget-internal)
           (fboundp 'xwidget-webkit-browse-url))
      (xwidget-webkit-browse-url (concat "file:///" file))
    (browse-url (concat "file:///" file))))

(defun gif-extra-process-file (file)
  "Read and perfoms actions for new gif FILE."
  (when (> gif-extra-duration gif-extra-minimal-autoopen-duration)
    (condition-case nil
        (let ((char (car (read-multiple-choice
                          (format "Open %s?"
                                  file)
                          '((?o "yes")
                            (?r "rename")
                            (?R "rename and open")
                            (?n "no"))))))
          (pcase char
            (?o (gif-extra-outfile-open-file file))
            ((or ?r ?R)
             (let* ((dir (gif-extra-read-directory
                          "Move to directory: "))
                    (new-name (expand-file-name
                               (read-string
                                "New name: "
                                (file-name-nondirectory file))
                               dir)))
               (if (not (file-exists-p new-name))
                   (progn (rename-file file new-name)
                          (when (eq char ?o)
                            (gif-extra-outfile-open-file new-name)))
                 (when (yes-or-no-p (format "%s exists. OK?" new-name))
                   (rename-file file new-name)
                   (when (eq char ?o)
                     (gif-extra-outfile-open-file new-name))))))))
      (error nil))))

(defun gif-extra-timer-cancel ()
  "Cancel `gif-extra-seconds-timer'."
  (when (timerp gif-extra-seconds-timer)
    (cancel-timer gif-extra-seconds-timer))
  (setq gif-extra-seconds-timer nil))

(defun gif-extra-increment-duration-seconds ()
  "Increment `gif-extra-duration-seconds' and update modeline."
  (setq gif-extra-duration-seconds (1+ gif-extra-duration-seconds))
  (force-mode-line-update)
  (gif-extra-timer-cancel)
  (setq gif-extra-seconds-timer
        (run-with-timer 1 nil #'gif-extra-increment-duration-seconds)))

(defun gif-extra-timer-countdown-cancel ()
  "Cancel `gif-extra-countdown-timer'."
  (when (timerp gif-extra-countdown-timer)
    (cancel-timer gif-extra-countdown-timer)
    (setq gif-extra-countdown-timer nil)))


(defun gif-extra-update-countdown ()
  "Update countdown in mode-line."
  (gif-extra-timer-countdown-cancel)
  (gif-extra-timer-cancel)
  (if (> gif-extra-countdown 0)
      (progn
        (setq gif-extra-countdown (1- gif-extra-countdown))
        (force-mode-line-update)
        (setq gif-extra-countdown-timer
              (run-with-timer 1 nil #'gif-extra-update-countdown)))
    (when (= gif-extra-countdown 0)
      (setq gif-extra-duration-seconds 0)
      (setq gif-extra-seconds-timer
            (run-with-timer 1 nil #'gif-extra-increment-duration-seconds)))))

(defconst gif-extra-mode-line-format '(:eval (gif-extra-mode-line-indicator)))

(defsubst gif-extra-mode-line-indicator ()
  "Return a string for the mode line with countdown or duration."
  (when (or gif-extra-countdown gif-extra-duration-seconds)
    (let ((sep (propertize " " 'face 'highlight))
          (vsep (propertize " " 'face '(:inherit variable-pitch))))
      (propertize (concat sep vsep
                          (or
                           (if (and gif-extra-countdown
                                    (>
                                     gif-extra-countdown
                                     0))
                               (format "▶ in %s"
                                       gif-extra-countdown)
                             (when (and gif-extra-duration-seconds
                                        (>= gif-extra-duration-seconds 0))
                               (format "● %s"
                                       gif-extra-duration-seconds)))
                           "")
                          vsep sep)
                  'face 'highlight))))

(defvar gif-extra-capture-timer nil)

(defun gif-extra-cleanup-mode-line ()
  "Remove gif mode line."
  (dolist (buff (buffer-list))
    (when-let ((mline (buffer-local-value 'mode-line-format buff)))
      (cond ((and (listp mline)
                  (or
                   (eq gif-extra-mode-line-format
                       (car-safe
                        gif-extra-mode-line-format))
                   (memq gif-extra-mode-line-format mline)))
             (with-current-buffer buff
               (setq mode-line-format
                     (remove gif-extra-mode-line-format
                             mode-line-format)))))
      buff)))

;;;###autoload
(defun gif-extra-screencast ()
  "Start recording the GIF with mode-line indicator.
A screenshot is taken every second and before every command."
  (interactive)
  (when (timerp gif-extra-capture-timer)
    (cancel-timer gif-extra-capture-timer)
    (setq gif-extra-capture-timer nil))
  (gif-extra-timer-countdown-cancel)
  (gif-extra-timer-cancel)
  (if gif-screencast-mode
      (progn (gif-screencast-stop)
             (gif-extra-cleanup-mode-line)
             (setq mode-line-format (remove gif-extra-mode-line-format
                                            mode-line-format)))
    (if (not (executable-find gif-screencast-program))
        (message
         "Screenshot program '%s' not found (See `gif-screencast-program')"
         gif-screencast-program)
      (dolist (d (list gif-screencast-output-directory
                       gif-screencast-screenshot-directory))
        (unless (file-exists-p d)
          (make-directory d 'parents)))
      (setq gif-screencast--frames '())
      (setq gif-screencast--counter 0)
      (gif-screencast-mode 1)
      (when (and (listp mode-line-format)
                 (not (member gif-extra-mode-line-format mode-line-format)))
        (setq mode-line-format
              (cons gif-extra-mode-line-format
                    mode-line-format)))
      (setq gif-extra-countdown gif-screencast-countdown)
      (setq gif-extra-duration-seconds 0)
      (gif-extra-update-countdown)
      (when (> gif-screencast-gc-cons-threshold 0)
        (setq gif-screencast--gc-cons-threshold-original gc-cons-threshold)
        (setq gc-cons-threshold gif-screencast-gc-cons-threshold))
      (setq gif-extra-capture-timer (run-with-timer gif-screencast-countdown 0.3
                                                    #'gif-screencast-capture)))))



;;;###autoload
(define-minor-mode gif-extra-global-mode
  "Override `gif-screencast--generate-gif' to prompt directory and open file."
  :lighter " gif+"
  :global t
  :group 'gif-screencast
  (advice-remove 'gif-screencast--generate-gif
                 #'gif-extra-screencast--generate-gif)
  (remove-hook 'gif-screencast-mode-hook #'gif-extra-record-duration)
  (advice-remove 'gif-screencast #'gif-extra-record-out-file)
  (when gif-extra-global-mode
    (advice-add 'gif-screencast--generate-gif :override
                #'gif-extra-screencast--generate-gif)
    (advice-add 'gif-screencast :before
                #'gif-extra-record-out-file)
    (add-hook 'gif-screencast-mode-hook #'gif-extra-record-duration)))

(provide 'gif-extra)
;;; gif-extra.el ends here
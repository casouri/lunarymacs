;;; stretch-reminder.el --- Display stretch alerts every 30 minuets  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(defvar stretch-reminder-text
  (propertize "TIME\nTO\nSTRETCH" 'face '(:height 1200))
  "The reminder to show.")

(defvar stretch-reminder--buffer "*rest reminder*"
  "Buffer name of the alert buffer.")

(defvar stretch-alert--idle-timer nil
  "Timer used to show the alert buffer as soon as there’s an idle.")

(defvar stretch-alert--scheduler-timer nil
  "Timer used to schedule a idle timer every 30 minutes.")

(defun stretch-reminder-show-reminder ()
  "Pop up a buffer and show the reminder."
  (let ((buf (get-buffer-create stretch-reminder--buffer)))
    (unless (get-buffer-window buf)
      (pop-to-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert stretch-reminder-text)
        (special-mode))))

  (when (timerp stretch-alert--idle-timer)
    (cancel-timer stretch-alert--idle-timer))
  (setq stretch-alert--idle-timer nil))

(defun stretch-reminder--schedule-reminder ()
  "Schedules an idle timer that shows stretch reminder on next idle."
  (unless stretch-alert--idle-timer
    (setq stretch-alert--idle-timer
          (run-with-idle-timer 3 nil #'stretch-reminder-show-reminder))))

(define-minor-mode stretch-reminder-mode
  "Reminds you to stretch every 30 minutes."
  :global t
  :lighter ""
  :group 'applications
  (when stretch-reminder-mode
    (setq stretch-alert--scheduler-timer
          (run-with-timer (* 60 30) (* 60 30)
                          #'stretch-reminder--schedule-reminder))
    (when stretch-alert--scheduler-timer
      (cancel-timer stretch-alert--scheduler-timer))))


(provide 'stretch-reminder)

;;; stretch-reminder.el ends here

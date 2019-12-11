;;; console-buffer.el --- Console buffer/window      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(defvar luna-console-buffer-alist '((emacs-lisp-mode . "*scratch*"))
  "An alist with element (major-mode . console buffer).")

(defvar-local luna-console-buffer-p nil
  "T if this buffer is a console buffer.")

(defvar luna-console-window nil
  "A window at bottom dedicated to console buffer.")

(defun luna--get-console-buffer (major-mode)
  "Return the console buffer corresponding to MAJOR-MODE.
Return nil if none exists."
  (if-let ((console-buffer (alist-get major-mode luna-console-buffer-alist)))
      console-buffer
    (message "No console buffer, use `luna-set-console-buffer' to set one")
    nil))

(defun luna-toggle-console ()
  "Toggle display of console buffer.
When console window is live, jump between console window and previous window;
when console window is not live, switch between console buffer and previous buffer."
  (interactive)
  (if (window-live-p luna-console-window)
      ;; jump between console window and previous window
      (if luna-console-buffer-p
          (if-let ((win (window-parameter luna-console-window 'luna-console-jump-back)))
              (select-window win)
            (select-window (previous-window))
            (message "Could not find previous window, guess one"))
        (let ((old-window (selected-window)))
          (select-window luna-console-window)
          (set-window-parameter nil 'luna-console-jump-back old-window)))
    ;; switch between console buffer and previous buffer
    (if luna-console-buffer-p
        (previous-buffer)
      (switch-to-buffer (luna--get-console-buffer major-mode))
      (setq-local luna-console-buffer-p t))))

(defun luna-set-console-buffer (buffer)
  "Set current console buffer to BUFFER."
  (interactive "b")
  (setf (alist-get major-mode luna-console-buffer-alist)
        (get-buffer buffer)))

(defun luna-toggle-console-window ()
  "Toggle display of console window."
  (interactive)
  (if (window-live-p luna-console-window)
      (delete-window luna-console-window)
    (when-let ((buf (luna--get-console-buffer major-mode)))
      (setq luna-console-window
            (display-buffer-at-bottom (get-buffer buf) '((window-height . 0.2)))))))

(provide 'console-buffer)

;;; console-buffer.el ends here

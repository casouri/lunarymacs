;;; console-buffer.el --- Console buffer/window      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package lets you toggle your interactive console for any major
;; mode. You can:
;;
;;   1. Switch back and forth between main buffer and console buffer
;;   2. Toggle a console window a the bottom
;;   3. Switch between the console window and the main window
;;
;; There are two commands ‘luna-toggle-console’ (1 & 3) and
;; ‘luna-toggle-console-window’ (2). See their doc string for more
;; info. Edit ‘luna-console-buffer-alist’ to add buffer name pattern
;; for each major mode. Use ‘luna-set-console-buffer’ to quickly set a
;; console buffer for current buffer.

;;; Code:
;;

(defvar luna-console-buffer-alist '((emacs-lisp-mode . "*scratch*")
                                    (python-mode . "*Python*")
                                    (sage-shell:sage-mode . "*Sage*")
                                    (lisp-mode . console-buffer-sly))
  "An alist with element (MAJOR-MODE . CONSOLE-BUFFER-NAME).
If the buffer’s name is dynamic, CONSOLE-BUFFER-NAME can also be
a function that returns the buffer.")

(defvar-local luna-console-buffer-p nil
  "T if this buffer is a console buffer.")

(defun luna-console-window ()
  "Return the window displaying the console buffer."
  (if luna-console-buffer-p
      (selected-window)
    (get-buffer-window (luna--get-console-buffer major-mode))))

(defun luna--get-console-buffer (mode)
  "Return the console buffer corresponding to MODE.
Return nil if none exists."
  (if-let ((console-buffer (alist-get mode luna-console-buffer-alist)))
      (if (functionp console-buffer)
          (funcall console-buffer)
        console-buffer)
    (message
     "Coulen’t find the console buffer, use `luna-set-console-buffer' to set one")
    nil))

(defun luna-toggle-console ()
  "Toggle display of console buffer.
When console window is live, jump between console window and
previous window; when console window is not live, switch between
console buffer and previous buffer."
  (interactive)
  (if luna-console-buffer-p
      ;; We want to jump back, either by jumping to another window, or
      ;; switching to previous buffer.
      (if-let ((win (window-parameter (luna-console-window)
                                      'luna-console-jump-back)))
          (select-window win)
        (previous-buffer))
    ;; We want to jump to a console buffer, either by jumping to
    ;; another window, or by switching to a console buffer.
    (if (window-live-p (luna-console-window))
        (select-window (luna-console-window))
      (when-let ((buf (luna--get-console-buffer major-mode)))
        (switch-to-buffer buf)
        (setq-local luna-console-buffer-p t)))))

(defun luna-set-console-buffer (buffer)
  "Set current console buffer to BUFFER."
  (interactive "b")
  (setf (alist-get major-mode luna-console-buffer-alist)
        (get-buffer buffer)))

(defun luna-toggle-console-window ()
  "Toggle display of console window."
  (interactive)
  (if (equal (selected-window) (luna-console-window))
      (user-error "You are in the console window, maybe use luna-toggle-console instead")
    (if (window-live-p (luna-console-window))
        (if (window-prev-buffers luna-console-window)
            (set-window-buffer
             luna-console-window
             (caar (window-prev-buffers (luna-console-window))))
          (delete-window luna-console-window))
      (when-let ((buf (luna--get-console-buffer major-mode)))
        (setq luna-console-window
              (display-buffer (get-buffer buf)
                              '(display-buffer--maybe-pop-up-window
                                . ((inhibit-same-window . t)))))))))

;;; Intergration

(defun console-buffer-sly ()
  "Return console buffer of sly."
  (require 'sly)
  (sly-mrepl--find-create (sly-current-connection)))

(provide 'console-buffer)

;;; console-buffer.el ends here

;;; bottom-line.el --- Global mode-line  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(defgroup bottom-line nil
  "Display a frame-global mode-line at the bottom."
  :group 'mode-line)

(defface bottom-line '((t . (:inherit mode-line)))
  "Face for bottom line."
  :group 'bottom-line)

(defvar bottom-line-pixel-height 23
  "The pixel height of the bottom line.")

(defsubst bottom-line-buffer ()
  "Return the bottom line buffer."
  (get-buffer-create " *bottom-line*"))

(defvar bottom-line-window nil
  "Window for bottom line.")

(defvar bottom-line-format nil
  "Like ‘mode-line-format’.")

(define-minor-mode bottom-line-mode
  "Show a global mode-line at the bottom."
  :global t
  :group 'convenience
  (if bottom-line-mode
      (let ((buf (bottom-line-buffer)))
        (with-current-buffer buf
          (setq mode-line-format nil
                window-size-fixed t))
        (setq bottom-line-window
              (display-buffer-in-side-window
               buf '((side . bottom)
                     (window-height . 1)
                     (dedicated . t))))
        ;; Initialize window and buffer.
        (set-window-parameter bottom-line-window 'no-other-window t)
        (set-window-parameter bottom-line-window
                              'no-delete-other-windows t)
        (setq-default mode-line-format nil)
        ;; This is actually pretty fast, so don’t worry.
        (add-hook 'post-command-hook #'bottom-line-update))
    (when bottom-line-window
      (delete-window bottom-line-window))
    (remove-hook 'post-command-hook #'bottom-line-update)))

(defun bottom-line-update ()
  "Update the bottom line with current buffer’s mode-line."
  (let ((line (format-mode-line bottom-line-format 'mode-line)))
    (with-current-buffer (bottom-line-buffer)
      (erase-buffer)
      (insert line)
      (let ((delta (- bottom-line-pixel-height
                      (window-pixel-height bottom-line-window)))
            (window-resize-pixelwise t))
        (window-resize bottom-line-window delta nil t t)))))


(provide 'bottom-line)

;;; bottom-line.el ends here

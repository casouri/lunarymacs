;;; bottom-line.el --- Global mode-line  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package displays a frame-global mode-line at the bottom of the
;; frame.
;;
;; Recommended usage:
;;
;; 1. Disable normal mode-line.
;;
;;     (setq-default mode-line-format nil)
;;
;; 2. Draw separation lines between vertically-split windows.
;;
;;     (setq window-divider-default-places 'bottom-only
;;           window-divider-default-bottom-width 1)
;;     (window-divider-mode)
;;     (bottom-line-mode)
;;
;; 3. Set ‘bottom-line-format’.
;;
;;     (setq-default bottom-line-format "(Just like mode-line-format)")
;;
;; 4. Enable ‘bottom-line-mode’.
;;
;;     M-x bottom-line-mode RET
;;
;; Caveat:
;;
;; 1. Bottom side-windows will appear below bottom-line.

;;; Code:

(defgroup bottom-line nil
  "Display a frame-global mode-line at the bottom."
  :group 'mode-line)

(defface bottom-line '((t . (:inherit mode-line)))
  "Face for bottom line."
  :group 'bottom-line)

(defvar bottom-line-pixel-height 23
  "The pixel height of the bottom line.")

(defvar-local bottom-line-format nil
  "Like ‘mode-line-format’.")

(defsubst bottom-line-buffer ()
  "Return the bottom line buffer."
  (get-buffer-create " *bottom-line*"))

(defvar bottom-line-window nil
  "Window for bottom line.")

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
              (display-buffer-at-bottom
               buf '((inhibit-same-window . t)
                     (inhibit-switch-frame . t)
                     (window-height . 1)
                     (dedicated . t))))
        ;; Initialize window and buffer.
        (set-window-parameter bottom-line-window 'no-other-window t)
        (set-window-parameter bottom-line-window
                              'no-delete-other-windows t)
        (setq-default mode-line-format nil)
        ;; This is actually pretty fast, so don’t worry.
        (add-hook 'post-command-hook #'bottom-line-update 50)
        (advice-add 'eldoc-minibuffer-message :after
                    #'bottom--line-eldoc-advice))
    (when bottom-line-window
      (delete-window bottom-line-window))
    (remove-hook 'post-command-hook #'bottom-line-update)
    (advice-remove 'eldoc-minibuffer-message
                   #'bottom--line-eldoc-advice)))

(defvar-local bottom-line--string nil
  "The content of the bottom-line.")

(defun bottom-line-update ()
  "Update the bottom line with current buffer’s information."
  (unless (minibufferp)
    (let ((line (format-mode-line bottom-line-format 'bottom-line)))
      (with-current-buffer (bottom-line-buffer)
        (erase-buffer)
        (insert line)
        (setq bottom-line--string line)
        (let ((delta (- bottom-line-pixel-height
                        (window-pixel-height bottom-line-window)))
              (window-resize-pixelwise t))
          (when (not (eql delta 0))
            (window-resize bottom-line-window delta nil t t)))))))

(defun bottom--line-eldoc-advice (&rest _)
  "Show eldoc message in bottom-line."
  (when (and bottom-line-mode (minibufferp))
    (with-current-buffer (bottom-line-buffer)
      (erase-buffer)
      (insert eldoc-mode-line-string
              bottom-line--string))))

(provide 'bottom-line)

;;; bottom-line.el ends here

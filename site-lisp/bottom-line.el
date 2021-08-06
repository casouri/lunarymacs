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
;; 2. Show separation lines between vertically-split windows.
;;
;;     (setq window-divider-default-places 'bottom-only
;;           window-divider-default-bottom-width 1)
;;     (window-divider-mode)
;;     (bottom-line-mode)
;;
;; 3. Set ‘bottom-line-format’.
;;
;;     (setq bottom-line-format "(Just like mode-line-format)")
;;
;; 4. Enable ‘bottom-line-mode’.
;;
;;     M-x bottom-line-mode RET

;;; Code:

(defgroup bottom-line nil
  "Display a frame-global mode-line at the bottom."
  :group 'mode-line)

(defface bottom-line '((t . (:inherit mode-line)))
  "Face for bottom line."
  :group 'bottom-line)

(defvar bottom-line-pixel-height 23
  "The pixel height of the bottom line.")

(defvar bottom-line-format nil
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
        (add-hook 'post-command-hook #'bottom-line-update)
        ;; FIXME: debug.
        (add-hook 'window-configuration-change-hook
                  (lambda ()
                    (when (and bottom-line-window
                               (not (window-live-p bottom-line-window)))
                      (debug)))
                  nil t)
        )
    (when bottom-line-window
      (delete-window bottom-line-window))
    (remove-hook 'post-command-hook #'bottom-line-update)))

(defun bottom-line-update ()
  "Update the bottom line with current buffer’s mode-line."
  (let ((line (format-mode-line bottom-line-format 'bottom-line)))
    (with-current-buffer (bottom-line-buffer)
      (erase-buffer)
      (insert line)
      (let ((delta (- bottom-line-pixel-height
                      (window-pixel-height bottom-line-window)))
            (window-resize-pixelwise t))
        (window-resize bottom-line-window delta nil t t)))))


(provide 'bottom-line)

;;; bottom-line.el ends here

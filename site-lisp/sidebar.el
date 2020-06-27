;;; sidebar.el --- Sidebar      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Display a sidebar showing the outline for this buffer.
;; In the sidebar, type TAB to toggle heading, RET to jump to
;; the heading in the base buffer.
;;
;; Enable sidebar for the current buffer by
;;
;;     M-x sidebar-mode RET.
;;
;; Enable sidebar globally (for certain major modes) by
;;
;;     M-x global-sidebar-mode RET.
;;
;; See ‘sidebar-enabled-mode-list’ for more.

;;; Code:
;;

(require 'outline+)
(require 'color-outline)

(defvar sidebar-enabled-mode-list
  '(org-mode markdown-mode)
  "A list of major modes where sidebar is enabled.
This variable only has effect in ‘global-sidebar-mode’.")

(defvar-local sidebar--overlay nil
  "Overlay put over sidebar buffer.")

(defvar-local sidebar--buffer nil
  "Sidebar buffer for this buffer.")

(defvar sidebar--window nil
  "Sidebar window for this buffer.")

(defvar-local sidebar--buffer-p nil
  "Non-nil indicates that this buffer is a sidebar buffer.")

(defun sidebar--buffer-for (buffer)
  "Return a sidebar buffer for BUFFER."
  (with-current-buffer
      (let ((buf-name (concat " outline: " (buffer-name buffer))))
        (or (and (buffer-live-p sidebar--buffer)
                 sidebar--buffer)
            (get-buffer buf-name)
            (setq sidebar--buffer
                  (make-indirect-buffer
                   buffer buf-name))))
    (outline-hide-sublevels 1)
    (when sidebar--overlay
      (delete-overlay sidebar--overlay))
    (setq sidebar--overlay
          (make-overlay (point-min) (point-max)
                        (current-buffer) nil t))
    (overlay-put sidebar--overlay
                 'face '(:height 0.8))
    (local-set-key (kbd "TAB") #'outline-cycle)
    (local-set-key (kbd "RET") #'sidebar--jump)
    (setq-local cursor-in-non-selected-windows nil
                sidebar--buffer-p t)
    (let ((base-outline-regexp
           (buffer-local-value 'outline-regexp buffer))
          (base-outline-level
           (buffer-local-value 'outline-level buffer))
          (base-hi-lock-patterns
           (buffer-local-value 'hi-lock-file-patterns buffer)))
      (setq-local outline-regexp base-outline-regexp)
      (setq-local outline-level base-outline-level)
      (hi-lock-set-file-patterns base-hi-lock-patterns)
      (outline-minor-mode)
      (hi-lock-mode))
    (current-buffer)))

(defun sidebar--jump ()
  "Jump to point in the related buffer.
EVENT is the mouse click event."
  (interactive)
  (let ((p (point))
        (base-buffer (buffer-base-buffer)))
    (when base-buffer
      (select-window (display-buffer base-buffer))
      (goto-char p)
      (recenter 1)
      (ignore-errors (outline-show-subtree)))))

(defun sidebar--window-change-hook ()
  "React to window state change."
  (if (or sidebar-mode
          (and global-sidebar-mode
               (apply #'derived-mode-p
                      sidebar-enabled-mode-list)))
      ;; Update sidebar.
      (sidebar--show)
    (unless (or (minibufferp) sidebar--buffer-p)
      (sidebar--hide))))

(defun sidebar--hide ()
  "Hide sidebar."
  (when (window-live-p sidebar--window)
    (delete-window sidebar--window)))

(defun sidebar--show ()
  "Show sidebar."
  (setq sidebar--window
        (display-buffer-in-side-window
         (sidebar--buffer-for (current-buffer))
         '((side . left) (window-width . 20)))))

(define-minor-mode global-sidebar-mode
  "Show sidebar globally.
Which buffer gets a sidebar depends on ‘sidebar-enabled-mode-list’."
  :global t
  :lighter ""
  ;; Enable outline+ if possible.
  (if global-sidebar-mode
      (progn (add-hook 'window-state-change-hook
                       #'sidebar--window-change-hook)
             (sidebar--show))
    (remove-hook 'window-state-change-hook
                 #'sidebar--window-change-hook)
    (sidebar--hide)))

(define-minor-mode sidebar-mode
  "Show sidebar for this buffer.
This mode doesn’t care about ‘sidebar-enabled-mode-list’."
  :lighter ""
  (if sidebar-mode
      (progn (add-hook 'window-state-change-hook
                       #'sidebar--window-change-hook)
             (sidebar--show))
    (sidebar--hide)))

(provide 'sidebar)

;;; sidebar.el ends here

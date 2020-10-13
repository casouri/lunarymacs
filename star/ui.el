;; -*- lexical-binding: t -*-

;;; Key

(when (not (display-graphic-p))
  (luna-def-key
   "M-h" #'windmove-left
   "M-j" #'windmove-down
   "M-k" #'windmove-up
   "M-l" #'windmove-right))

(luna-def-key
 :leader
 "w" '("display")
 "wr" #'luna-desktop-read
 "tsb" #'sidebar-mode

 :---
 "s-y" #'luna-toggle-console
 "C-s-y" #'luna-toggle-console-window

 "C-s-k" #'buf-move-up
 "C-s-j" #'buf-move-down
 "C-s-h" #'buf-move-left
 "C-s-l" #'buf-move-right

 "s-h" #'windmove-left
 "s-j" #'windmove-down
 "s-k" #'windmove-up
 "s-l" #'windmove-right
 "s-s" #'save-buffer
 "s-w" #'delete-frame

 "<M-up>" #'outline-previous-visible-heading
 "<M-down>" #'outline-next-visible-heading)

;;; Package

(add-to-list 'luna-toggle-theme-list 'light)
(add-to-list 'luna-toggle-theme-list 'cyberpunk)
(setq custom-theme-directory
      (expand-file-name "site-lisp" user-emacs-directory))


(load-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config (with-current-buffer (get-buffer-create "*scratch*")
            (rainbow-delimiters-mode)))


(load-package rainbow-mode
  :commands rainbow-mode)


(load-package highlight-parentheses
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (setq hl-paren-colors
        (list (face-attribute
               'hl-paren-face :foreground)))
  (global-highlight-parentheses-mode)
  ;; highlight only the most inner pair
  (add-hook 'luna-load-theme-hook
            (lambda ()
              (setq hl-paren-colors
                    (list (face-attribute
                           'hl-paren-face :foreground)))
              (global-highlight-parentheses-mode -1)
              (global-highlight-parentheses-mode))))


(load-package nyan-lite
  :init (setq nyan-lite-add-mode-line nil
              nyan-lite-progress-bar nil
              nyan-lite-animate t)
  :commands nyan-lite-mode)


(load-package hl-todo
  :config
  (push 'org-mode hl-todo-text-modes)
  (push 'fundamental-mode hl-todo-text-modes)
  (setq hl-todo-keyword-faces ; override
        (append '(("FAIL" . "red1")
                  ("TOTEST" . "#d0bf8f")
                  ("UNSURE" . "#FE6266")
                  ("TRY" . "#FFF100")
                  ("GOOD" . "#52DEA1"))
                hl-todo-keyword-faces))
  (global-hl-todo-mode))


(load-package sidebar
  :commands sidebar-mode global-sidebar-mode)


;; Use the site-lisp version.
(load-package form-feed
  :hook ((emacs-lisp-mode-hook text-mode-hook special-mode-hook)
         . form-feed-mode))


(with-eval-after-load 'info
  (require 'info+)
  (info-pretty-mode))


;; (load-package treemacs
;;   :config (treemacs-resize-icons 11))


(load-package buffer-move
  :commands
  buf-move-up
  buf-move-dowan
  buf-move-left
  buf-move-right)


(load-package console-buffer
  :commands
  luna-toggle-console
  luna-toggle-console-window)

;;; Functions

(defun chunyang-alpha-param-adjust ()
  "调节当前 Frame 的透明度."
  (interactive)
  (let ((alpha (or (frame-parameter nil 'alpha) 100)))
    (while (pcase (read-key
                   (format "%d%%, use +,-,0 for further adjustment" alpha))
             ((or ?+ ?=) (setq alpha (1+ alpha)))
             (?- (setq alpha (1- alpha)))
             (?0 (setq alpha 100))
             (_  nil))
      (cond ((> alpha 100) (setq alpha 100))
            ((< alpha 0) (setq alpha 0)))
      (modify-frame-parameters nil `((alpha . ,alpha))))))


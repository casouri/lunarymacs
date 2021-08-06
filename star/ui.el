;; -*- lexical-binding: t -*-

;;; Key

(when (not (display-graphic-p))
  (luna-def-key
   "M-h" #'windmove-left
   "M-j" #'windmove-down
   "M-k" #'windmove-up
   "M-l" #'windmove-right))

(luna-def-key
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
 "s-w" #'delete-frame)


;;; Config

(setq custom-theme-directory
      (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*Warnings*")
               . (display-buffer-in-side-window
                  . ((side . bottom)))))

;;; Package

(load-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config (with-current-buffer (get-buffer-create "*scratch*")
            (rainbow-delimiters-mode)))


(load-package rainbow-mode
  :commands rainbow-mode)


(load-package highlight-parentheses
  :config
  ;; Highlight only the most inner pair. Face is set in light-theme
  ;; and cyberpunk-theme.
  (setq highlight-parentheses-colors
        (lambda ()
          (list (if (eq (frame-parameter nil 'background-mode) 'light)
                    "red" "green"))))
  (global-highlight-parentheses-mode))


(load-package hl-todo
  :config
  (push 'org-mode hl-todo-text-modes)
  (push 'fundamental-mode hl-todo-text-modes)
  (let ((warning '(:inherit (warning bold)))
        (error '(:inherit (error bold)))
        (success '(:inherit (success bold))))
    (setq hl-todo-keyword-faces
          `(("FAIL" . ,error)
            ("HACK" . ,error)
            ("KLUDGE". ,error)
            ("FIXME" . ,error)
            ("DONT" . ,error)
            ("TOTEST" . ,warning)
            ("UNSURE" . ,warning)
            ("TRY" . ,warning)
            ("TODO" . ,warning)
            ("TEMP" . ,warning)
            ("GOOD" . ,success)
            ("DONE" . ,success)
            ("NOTE" . ,success)
            ("OKAY" . ,success)
            ("NEXT" . ,success))))
  (global-hl-todo-mode))


;; (load-package sidebar
;;   :commands sidebar-mode global-sidebar-mode)


;; Use the site-lisp version.
(load-package form-feed
  :hook ((emacs-lisp-mode-hook text-mode-hook special-mode-hook)
         . form-feed-mode))


;; (load-package info+
;;   :hook (Info-mode-hook . info-pretty-mode))


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

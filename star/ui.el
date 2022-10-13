;; -*- lexical-binding: t -*-

;;; Key

(when (not (display-graphic-p))
  (luna-key-def
   "M-h" #'windmove-left
   "M-j" #'windmove-down
   "M-k" #'windmove-up
   "M-l" #'windmove-right))

(luna-key-def
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

 "s-F" #'tab-bar-switch-to-next-tab
 "s-B" #'tab-bar-switch-to-prev-tab

 ;; `winner-redo' is defined in angel.el as M-o.
 "s-." #'winner-redo

 :leader
 "tr" #'tab-rename)

;;; Config

(setq custom-theme-directory
      (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*Warnings*")
               . (display-buffer-in-side-window
                  . ((side . bottom)))))

;;; Package

(load-package rainbow-delimiters
  :autoload-hook (prog-mode-hook . rainbow-delimiters-mode)
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

(load-package winner
  :config (winner-mode))

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

(load-package annotate
  :commands annotate-mode
  :config (setq annotate-annotation-position-policy :new-line))

(load-package diff-hl
  :autoload-hook (prog-mode-hook . diff-hl-mode)
  :config (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Use the site-lisp version.
(load-package form-feed
  :autoload-hook ((emacs-lisp-mode-hook text-mode-hook special-mode-hook)
                  . form-feed-mode))

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

;; (load-package tab-bar-echo-area
;;   :autoload-hook (tab-bar-mode-hook . tab-bar-echo-area-mode)
;;   :config (setq tab-bar-tab-name-function #'luna-tab-bar-name))

(with-eval-after-load 'tab-bar-mode
  (setq tab-bar-close-button-show nil)
  (setq-default tab-bar-show t))

(defun luna-tab-bar-name ()
  "Construct tab name by major mode and buffer name."
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (format "%s: %s" (format-mode-line mode-name) (buffer-name))))

(with-eval-after-load 'face-remap
  (setq text-scale-remap-header-line t)
  ;; Also remap monospace font.
  (defun remap-mono-font-size ()
    "Remap ‘fixed-pitch’ face according to current text scaling."
    (face-remap--remap-face 'fixed-pitch)
    (add-hook 'text-scale-mode-hook #'remap-mono-font-size)))

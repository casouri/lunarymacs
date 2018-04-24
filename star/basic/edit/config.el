;;; -*- lexical-binding: t -*-

(use-package| expand-region
  :commands er/expand-region
  :general (default-leader
	     "v" #'er/expand-region
             ))

(use-package| embrace
  :config
  (global-set-key (kbd "C-,") #'embrace-commander))


(use-package| undo-tree
  :config (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package| recentf-ext
  :commands (recentf counsel-recentf)
  )

;; (use-package| smooth-scrolling
;;   :init
;;   (defun setup-smooth-scrolling ()
;;     (require 'smooth-scrolling)
;;     (smooth-scrolling-mode 1))
;;   (delay-load| setup-smooth-scrolling))

(use-package| avy
  :config
  (setq avy-background t)
  (setq avy-all-windows nil))

(post-config| general
  (default-leader
    "k" #'avy-goto-char-timer))

(use-package| minimap
  :config
  (setq
   minimap-width-fraction 0.1
   minimap-window-location 'right
   minimap-update-delay 0)
  ;; (overlay-put minimap-line-overlay 'face '(:background "blue" :foreground "blue"))
  (custom-theme-set-faces 'user
                          '(minimap-active-region-background
                            ((((background dark)) (:background "#61526E"))
                             (t (:background "#d3d3e7")))))
  :commands minimap-mode)

;; (custom-theme-set-faces 'user
;;                         '(minimap-active-region-background
;;                           ((((background dark)) (:background "#56B0EC" :foreground "#61526E"))
;;                            (t (:background "#56B0EC" :foreground "#56B0EC")))))
(post-config| general
  (default-leader "tm" #'minimap-mode))

;;
;; Config
;;

(electric-pair-mode 1)

;; scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(post-config| general
  (general-define-key
   :states 'insert
   "<C-return>" #'moon/jump-newline-below
   "<C-S-return>" #'moon/jump-newline-above
   "C-;" #'moon/insert-semi-at-eol
   )
  (general-define-key
   :states 'normal
   "J" #'moon/scroll-down-reserve-point
   "K" #'moon/scroll-up-reserve-point)) 

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; fold
;; (add-hook 'prog-mode-hook (lambda () (outline-minor-mode 1)))

(defun open-next-line (arg)
      "Move to the next line and then opens a line.
    See also `newline-and-indent'."
      (interactive "p")
      (end-of-line)
      (open-line arg)
      (next-line 1)
      (when newline-and-indent
        (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)

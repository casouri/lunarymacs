;;; -*- lexical-binding: t -*-

(use-package| expand-region
  :commands er/expand-region
  :general (default-leader
	     "v" #'er/expand-region
             )
  )

(use-package| winum
  :config (winum-mode)
  )

(defun moon-switch-to-window (num)
  "Switch to sindow NUM, split window if not exist."
  (if (> num winum--window-count)
      (call-interactively #'moon/split-to)
    (winum-select-window-by-number num)))

(defun moon/split-to (direction)
  "Split window and switch to it according to DIRECTION.
Accept one character for DIRECTION.
  k     ^
h   l <   >
  j     v
"
  (interactive "cswitch to window: h j k l")
  (pcase direction
    (?k (split-window-below))                 ; up
    (?j (select-window (split-window-below))) ; down
    (?h (split-window-right))                 ; left
    (?l (select-window (split-window-right))) ; right
    ))

(defun moon/switch-to-window-1 ()
  "Switch to window 1, split window if not exist."
  (interactive)
  (moon-switch-to-window 1))

(defun moon/switch-to-window-2()
  "Switch to window 2, split window if not exist."
  (interactive)
  (moon-switch-to-window 2))

(defun moon/switch-to-window-3 ()
  "Switch to window 3, split window if not exist."
  (interactive)
  (moon-switch-to-window 3))

(defun moon/switch-to-window-4 ()
  "Switch to window 4, split window if not exist."
  (interactive)
  (moon-switch-to-window 4))

(defun moon/switch-to-window-5()
  "Switch to window 5, split window if not exist."
  (interactive)
  (moon-switch-to-window 5))

(defun moon/switch-to-window-6 ()
  "Switch to window 6, split window if not exist."
  (interactive)
  (moon-switch-to-window 6))

(defun moon/switch-to-window-7 ()
  "Switch to window 7, split window if not exist."
  (interactive)
  (moon-switch-to-window 7))

(defun moon/switch-to-window-8 ()
  "Switch to window 8, split window if not exist."
  (interactive)
  (moon-switch-to-window 8))

(defun moon/switch-to-window-9 ()
  "Switch to window 9, split window if not exist."
  (interactive)
  (moon-switch-to-window 9))


(post-config| general
  (default-leader
    "1" #'moon/switch-to-window-1
    "2" #'moon/switch-to-window-2
    "3" #'moon/switch-to-window-3
    "4" #'moon/switch-to-window-4
    "5" #'moon/switch-to-window-5
    "6" #'moon/switch-to-window-6
    "7" #'moon/switch-to-window-7
    "8" #'moon/switch-to-window-8
    "9" #'moon/switch-to-window-9
    ))

(post-config| which-key
  ;; create a fake key to represent all ten keys 
  (push '(("\\(.*\\) 0" . "winum-select-window-0-or-10") . ("\\1 0..9" . "window 0..9")) which-key-replacement-alist)
  ;; hide other keys
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
  )

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
  :config (setq avy-background t)
  )

(post-config| general
  (default-leader
    "k" #'avy-goto-char-timer))

(use-package| minimap
  :init
  (setq
   minimap-width-fraction 0.1
   minimap-window-location 'right
   minimap-update-delay 0)
  :commands minimap-mode)


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

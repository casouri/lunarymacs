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

(post-config| general
  (default-leader
    "0" #'winum-select-window-0-or-10
    "1" #'winum-select-window-1
    "2" #'winum-select-window-2
    "3" #'winum-select-window-3
    "4" #'winum-select-window-4
    "5" #'winum-select-window-5
    ))

(post-config| which-key
  ;; create a fake key to represent all ten keys 
  (push '(("\\(.*\\) 0" . "winum-select-window-0-or-10") . ("\\1 0..9" . "window 0..9")) which-key-replacement-alist)
  ;; hide other keys
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
  )

(use-package| undo-tree
  :delight (undo-tree-mode)
  :config (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package| recentf-ext
  :commands (recentf counsel-recentf)
  )

(use-package| smooth-scrolling
  :hook (prog-mode . smooth-scrolling-mode))

(use-package| avy
  :hook (prog-mode . (lambda () (require 'avy)))
  )

(post-config| general
  (default-leader
    "SPC" #'avy-goto-char-2))

;;
;; Config
;;

(electric-pair-mode 1)

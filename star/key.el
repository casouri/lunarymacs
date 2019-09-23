;; separate C-i from TAB
(when window-system
  ;; use as (kbd "<C-i>")
  (define-key input-decode-map "\C-i" [C-i]))

;; translate iTerm sequences
(require 'cl-lib)
(cl-loop for char from ?a to ?z
         do (define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "s-%c" char))))

;;;;; general

(load-package general
  :config
  (general-override-mode)

  (general-create-definer luna-default-leader
    :prefix "C-SPC"
    :keymaps 'override)

  (general-create-definer luna-cc-leader
    :keymaps 'override
    :prefix "C-c")

  (general-create-definer luna-cx-leader
    :keymaps 'override
    :prefix "C-x")

  (luna-default-leader
    "f" '(:ignore t :which-key "file")
    "F" '(:ignore t :which-key "Frame")
    "i" '(:ignore t :which-key "insert")
    "h" '(:ignore t :which-key "help")
    "j" '(:ignore t :which-key "jump")
    "r" '(:ignore t :which-key "register")
    "s" '(:ignore t :which-key "search")
    "T" '(:ignore t :which-key "Theme")
    "p" '(:ignore t :which-key "project")
    "w" '(:ignore t :which-key "window")
    "b" '(:ignore t :which-key "buffer")
    "q" '(:ignore t :which-key "quit")
    "m" '(:ignore t :which-key "major-mode")
    "e" '(:ignore t :which-key "error")
    "a" '(:ignore t :which-key "action")
    "t" '(:ignore t :which-key "toggle")
    "g" '(:ignore t :which-key "git")
    "p" '(:ignore t :which-key "project")
    "u" '(:ignore t :which-key "utility")
    "o" '(:ignore t :which-key "open")
    ;; Themes
    "Tc" #'customize-themes
    ;; Frame
    "Fd" #'delete-frame
    ;; align
    "="  #'align-regexp
    ;; open
    "o t"  #'luna-open-in-iterm
    "o f"  #'luna-open-in-finder
    "o p"  '((lambda () (interactive) (find-file "~/p")) :which-key "open ~/p")
    "o d"  '((lambda () (interactive) (find-file "~/Downloads")) :which-key "open ~/Downloads")
    "o D"  '((lambda () (interactive) (find-file "~/Desktop/")) :which-key "open ~/Desktop")
    "o h"  '((lambda () (interactive) (find-file "~/")) :which-key "open home dir")
    ;; file
    "fR"  #'luna-rename-file
    "fs"  #'save-buffer
    "fed" #'luna-open-init-file
    "feD" #'luna-compare-init-to-example
    "fD"  '(:ignore t :which-key "delete file")
    "fDD" #'luna-delete-file-and-buffer
    ;; quit
    "qq"  #'save-buffers-kill-emacs
    ;; buffer
    "bm"  '((lambda () (interactive) (switch-to-buffer "*Messages*"))
            :which-key "goto message buffer")
    "bd"  #'kill-buffer-and-window
    "bh"  #'luna-close-help
    "bo"  #'luna-kill-other-buffer
    "bh"  #'luna-kill-helper
    "bb"  #'list-buffers
    "bs"  '((lambda () (interactive) (switch-to-buffer "*scratch*"))
            :which-key "goto scratch buffer")
    ;; toggle
    "tt"  #'luna-switch-theme
    "tM"  #'toggle-frame-maximized
    "tf"  #'luna-toggle-format-on-save
    "td"  #'toggle-debug-on-error
    ;; search
    "si"  #'imenu
    )
  (general-define-key
   :keymaps 'smerge-mode-map
   "C-c n" #'smerge-next
   "C-c p" #'smerge-prev
   "C-c o" #'smerge-keep-lower ; other
   "C-c m" #'smerge-keep-upper ; mine
   )

  (general-define-key
   "s-h" #'windmove-left
   "s-j" #'windmove-down
   "s-k" #'windmove-up
   "s-l" #'windmove-right
   "s-s" #'save-buffer
   "s-w" #'delete-frame)

  (luna-provide 'key.general))

  ;;;;; which-key
(load-package which-key
  :after general
  :config (which-key-mode))

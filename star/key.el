;; -*- lexical-binding: t; -*-

;;; Translation

;; separate C-i from TAB
(when window-system
  ;; use as (kbd "<C-i>")
  (define-key input-decode-map "\C-i" [C-i])
  ;; iterm2 escape sequence
  (define-key input-decode-map "\e[1;Ci" [C-i]))

;; translate iTerm sequences
(require 'cl-lib)
(cl-loop for char from ?a to ?z
         do (define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "s-%c" char))))

;;; Key

(load-package general
  :config
  (general-override-mode)

  (general-create-definer luna-default-leader
    :prefix "C-SPC"
    :keymaps 'override)

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
    ;; insert
    "ii" #'insert-char
    ;; Themes
    "Tc" #'customize-themes
    ;; Frame
    "Fd" #'delete-frame
    ;; align
    "="  #'align-regexp
    ;; file
    "fR"  #'luna-rename-file
    "fs"  #'save-buffer
    "fed" #'luna-open-init-file
    "feD" #'luna-compare-init-to-example
    "fD"  '(:ignore t :which-key "delete file")
    "fDD" #'luna-delete-file-and-buffer
    ;; buffer
    "bm"  '((lambda () (interactive) (switch-to-buffer "*Messages*"))
            :which-key "goto message buffer")
    "bs"  '((lambda () (interactive) (switch-to-buffer "*scratch*"))
            :which-key "goto scratch buffer")
    ;; toggle
    "tt"  #'luna-switch-theme
    "tm"  #'toggle-frame-maximized
    "tf"  #'luna-toggle-format-on-save
    "td"  #'toggle-debug-on-error
    "tn"  #'treemacs
    "tl"  #'display-line-numbers-mode
    ;; search
    "si"  #'imenu)

  ;;;; other builtin keymaps
  (general-define-key
   :keymaps 'smerge-mode-map
   "C-c n" #'smerge-next
   "C-c p" #'smerge-prev
   "C-c o" #'smerge-keep-lower ; other
   "C-c m" #'smerge-keep-upper ; mine
   "C-c l" #'smerge-keep-lower
   "C-c u" #'smerge-keep-upper)

  (general-define-key
   :keymaps 'comint-mode-map
   "<up>" #'comint-previous-input
   "<down>" #'comint-next-input)

  (provide 'luna-general-config))

(load-package which-key
  :after general
  :config (which-key-mode))

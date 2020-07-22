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

(luna-def-key
 :leader
 "p" '("project")
 ;; align
 "="  #'align-regexp
 ;; file
 "f" '("file")
 "fR"  #'luna-rename-file
 ;; buffer
 "b" '("buffer")
 "bm"  '("goto message buffer" .
         (lambda () (interactive) (switch-to-buffer "*Messages*")))
 "bs"  '("goto scratch buffer" .
         (lambda () (interactive) (switch-to-buffer "*scratch*")))
 ;; toggle
 "t" '("toggle")
 "tt"  #'luna-switch-theme
 "tm"  #'toggle-frame-maximized
 "tf"  #'luna-toggle-format-on-save
 "td"  #'toggle-debug-on-error
 "tl"  #'display-line-numbers-mode
 ;; search
 "s" '("search")
 "si"  #'imenu
 
 :---
 :keymaps 'smerge-mode-map
 "C-c n" #'smerge-next
 "C-c p" #'smerge-prev
 "C-c o" #'smerge-keep-lower ; other
 "C-c m" #'smerge-keep-upper ; mine
 "C-c l" #'smerge-keep-lower
 "C-c u" #'smerge-keep-upper
 
 :keymaps 'comint-mode-map
 "<up>" #'comint-previous-input
 "<down>" #'comint-next-input)

(load-package which-key
  :config
  (setq which-key-enable-extended-define-key t)
  (which-key-mode))

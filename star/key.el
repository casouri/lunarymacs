;; -*- lexical-binding: t; -*-

;;; Translation

;; ;; separate C-i from TAB
;; (when window-system
;;   ;; use as (kbd "<C-i>")
;;   (define-key input-decode-map "\C-i" [C-i])
;;   ;; iterm2 escape sequence
;;   (define-key input-decode-map "\e[1;Ci" [C-i]))

;; translate iTerm sequences
(require 'cl-lib)
(cl-loop for char from ?a to ?z
         do (define-key input-decode-map
              (format "\e[1;P%c" char) (kbd (format "s-%c" char))))

(define-key input-decode-map "\e[1;P=" (kbd "C-="))
(define-key input-decode-map "\e[1;P;" (kbd "C-;"))
(define-key input-decode-map "\e[1;P'" (kbd "C-'"))

;;; Key

(luna-def-key
 :leader
 ;; align
 "="  #'align-regexp
 ;; file
 "f" '("File")
 "fr"  #'luna-rename-file
 ;; buffer
 "b" '("Buffer")
 "bm"  '("message buffer" .
         (lambda () (interactive) (switch-to-buffer "*Messages*")))
 "bs"  '("scratch buffer" .
         (lambda () (interactive) (switch-to-buffer "*scratch*")))
 ;; toggle
 "st"  #'luna-switch-theme
 "tm"  #'toggle-frame-maximized
 "td"  #'toggle-debug-on-error
 "dl"  #'display-line-numbers-mode
 "ir" #'inhibit-read-only-mode
 "ip" #'info-pretty-mode
 "af" #'auto-fill-mode
 ;; search
 "s" '("Search")
 "si"  #'imenu
 "sg"  #'grep
 ;; open
 "o" '("Open")
 "of" #'open-in-finder
 "oi" #'open-in-iterm
 ;; etc
 "ld" #'luna-dump
 "cw" #'count-words
 "cm" #'customize
 "fl" #'find-library
 "de" #'debug-on-entry
 "cd" #'cancel-debug-on-entry
 "ci" #'cowgirl-install
 "cd" #'cowgirl-delete
 "wu" #'winner-undo
 "wr" #'winner-redo
 "sc" #'shell-command
 
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
  ;; We handle this in `luna-def-key'.
  ;; :init (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode))

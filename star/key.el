;; -*- lexical-binding: t; -*-

;;; Translation

;; ;; separate C-i from TAB
;; (when window-system
;;   ;; use as (kbd "<C-i>")
;;   (define-key input-decode-map "\C-i" [C-i])
;;   ;; iterm2 escape sequence
;;   (define-key input-decode-map "\e[1;Ci" [C-i]))

;; Translate iTerm sequences.
(require 'cl-lib)
(cl-loop for char from ?a to ?z
         do (define-key input-decode-map
              (format "\e[1;P%c" char) (kbd (format "s-%c" char))))

(define-key input-decode-map "\e[1;PC-f" (kbd "C-s-f"))
(define-key input-decode-map "\e[1;PC-b" (kbd "C-s-b"))

(define-key input-decode-map "\e[1;P=" (kbd "C-="))
(define-key input-decode-map "\e[1;P;" (kbd "C-;"))
(define-key input-decode-map "\e[1;P'" (kbd "C-'"))

;;; Key

(luna-key-def
 :leader
 ;; align
 "="  #'align-regexp
 
 "f" '("File")
 "fr"  #'luna-rename-file

 "b" '("Buffer")
 "bm" '("message buffer" .
        (lambda () (interactive) (switch-to-buffer "*Messages*")))
 "bs" '("scratch buffer" .
        (lambda () (interactive) (switch-to-buffer "*scratch*")))
 "bn" '("note buffer" .
        (lambda () (interactive) (switch-to-buffer "*note*") (text-mode)))

 "t" '("Toggle")
 "tt"  #'luna-switch-theme
 "tm"  #'toggle-frame-maximized
 "td"  #'toggle-debug-on-error
 "tl"  #'display-line-numbers-mode

 "s" '("Search")
 "si"  #'imenu

 "o" '("Open/Org")
 "of" #'open-in-finder
 "oi" #'open-in-iterm

 "r" '("Replace")
 "rq" #'query-replace
 "rr" #'replace-regexp
 "rs" #'replace-string

 "e" '("Eval")
 "l" '("Luna/load")
 "p" '("Profile")
 "h" '("Hide")

 "lf" #'luna-load-font
 "lt" #'luna-load-theme
 "se" #'luna-sudo-edit
 "cc" #'compile
 "ld" #'luna-dump
 "cw" #'count-words
 "cm" #'customize
 "fl" #'find-library
 "ci" #'cowgirl-install
 "ps" '("profiler-start" . (lambda () (interactive)
                             (profiler-start 'cpu+mem)))
 "pr" '("profiler-report" . (lambda () (interactive)
                              (profiler-stop)
                              (profiler-report)))
 
 :---
 :keymaps 'smerge-mode-map
 "C-c n" #'smerge-next
 "C-c p" #'smerge-prev
 "C-c l" #'smerge-keep-lower
 "C-c u" #'smerge-keep-upper
 "C-c a" #'smerge-keep-all
 
 :keymaps 'comint-mode-map
 "<up>" #'comint-previous-input
 "<down>" #'comint-next-input

 :keymaps 'minibuffer-local-map
 "C-<return>" '("insert newline" .
                (lambda () (interactive) (insert "\n"))))

;;; Package

(load-package which-key
  ;; We handle this in `luna-key-def'.
  ;; :init (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode))

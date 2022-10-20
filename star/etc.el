;;-*- lexical-binding: t -*-

;;; Config

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)
;; UTF-8
(set-default-coding-systems 'utf-8)
;; mouse-1 to follow link
(put 'default-button 'follow-link t)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(advice-add #'enable-theme :after
            (lambda (&rest _)
              ;; Otherwise title bar’s text’s color doesn’t look right.
              (when (featurep 'ns)
                (set-frame-parameter
                 nil 'ns-appearance
                 (frame-parameter nil 'background-mode)))))

;;;; Term mouse
;; (unless (display-graphic-p)
;;   (require 'mouse)
;;   (xterm-mouse-mode t)
;;   (setq mouse-sel-mode t))

(global-set-key (kbd "<mouse-4>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-5>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-6>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-7>") #'mwheel-scroll)

;;; Packages

(load-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(load-package benchmark-init :defer)

(load-package exec-path-from-shell :defer)

;;; Custom

(custom-set-variables
 ;; Use ‘sort-lines’ to sort the entries.
 ;; SYMBOL EXP NOW REQUEST COMMENT.
 '(apropos-do-all t nil nil nil "Do a more comprehensive search")
 '(auto-save-timeout 5 nil nil nil "Auto-save every 5 seconds.")
 '(backup-by-copying t nil nil "Prevent Emacs from breaking hard links.")
 '(blink-cursor-mode nil nil nil "Don’t blink cursor.")
 '(compilation-always-kill t nil nil "Auto-kill compilation buffer.")
 '(compilation-ask-about-save nil nil nil "Auto-save before compile.")
 '(confirm-nonexistent-file-or-buffer t)
 '(create-lockfiles nil)
 '(cursor-in-non-selected-windows nil)
 '(cursor-type 'bar)
 '(delete-auto-save-files nil)
 '(delete-by-moving-to-trash t)
 '(dictionary-use-single-buffer t)
 '(display-line-numbers-width 3)
 '(doc-view-resolution 300)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-display-truncation-message nil)
 '(eldoc-echo-area-display-truncation-message nil)
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-echo-area-use-multiline-p 2)
 '(eldoc-idle-delay 0.1)
 '(electric-pair-mode t)
 '(enable-recursive-minibuffers t)
 '(fast-but-imprecise-scrolling t)
 '(global-so-long-mode t)
 '(help-window-select t)
 '(history-delete-duplicates t)
 '(history-length 500)
 '(idle-update-delay 0.5 nil nil "Idle time before update stuff on screen")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-ring-max 100)
 '(load-prefer-newer t)
 '(make-cursor-line-fully-visible nil)
 '(minibuffer-electric-default-mode t)
 '(mouse-scroll-delay 0)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(0.03))
 '(mouse-wheel-tilt-scroll t)
 '(ns-pop-up-frames nil nil nil "This way emacsclient doesn't open new frames.")
 '(outline-minor-mode-cycle t)
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("nongnu" . "https://elpa.nongnu.org/packages") ("melpa" . "http://melpa.org/packages/") ("org" . "https://orgmode.org/elpa/")))
 '(pixel-scroll-precision-mode t)
 '(recentf-max-saved-items 500)
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 1)
 '(scroll-preserve-screen-position nil)
 '(send-mail-function 'mailclient-send-it)
 '(sentence-end-double-space nil)
 '(show-paren-mode nil nil nil "Since Emacs 28, show-paren mode is on by default, but it only highlights matching paren when point is on a paren, whereas highlight-parentheses-mode always highlights the matching paren.")
 '(split-height-threshold nil nil nil "Don’t split vertically.")
 '(split-width-threshold 150 nil nil "Do split horizontally")
 '(tab-width 4)
 '(use-dialog-box nil)
 '(use-system-tooltips nil)
 '(user-full-name "Yuan Fu")
 '(user-mail-address "casouri@gmail.com")
 '(window-resize-pixelwise t)
 '(word-wrap-by-category t)
 '(xref-prompt-for-identifier '(not xref-find-references xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame));; Ends here.
 )

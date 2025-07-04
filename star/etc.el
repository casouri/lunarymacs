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
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (let ((backup-dir (expand-file-name "backup"
                                      no-littering-var-directory)))
    (unless (file-exists-p backup-dir)
      (mkdir backup-dir t))
    (add-to-list 'backup-directory-alist `("." . ,backup-dir))))

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
 '(cursor-type '(box . 15))
 '(delete-auto-save-files nil)
 '(delete-by-moving-to-trash t)
 '(dictionary-use-single-buffer t)
 '(display-line-numbers-width 3)
 '(doc-view-resolution 300)
 '(double-click-time 400 nil nil nil "500 is too long.")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-display-truncation-message nil)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
 '(eldoc-echo-area-display-truncation-message nil)
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-idle-delay 0.1)
 '(electric-pair-mode t)
 '(enable-recursive-minibuffers t)
 '(fast-but-imprecise-scrolling t)
 '(global-so-long-mode t)
 '(global-subword-mode t)
 '(help-window-select t)
 '(history-delete-duplicates t)
 '(history-length 500)
 '(icon-preference '(image symbol emoji text))
 '(idle-update-delay 0.5 nil nil "Idle time before update stuff on screen")
 '(indent-tabs-mode nil)
 '(kill-ring-max 100)
 '(load-prefer-newer t)
 '(make-cursor-line-fully-visible nil)
 '(minibuffer-electric-default-mode t)
 '(mouse-scroll-delay 0)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(0.03))
 '(mouse-wheel-tilt-scroll t)
 '(ns-pop-up-frames nil nil nil "This way emacsclient doesn't open new frames.")
 '(outline-minor-mode-cycle nil nil nil "We have S-i for cycling outline section, TAB might conflict with indenting.")
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("nongnu" . "https://elpa.nongnu.org/packages") ("melpa" . "http://melpa.org/packages/")))
 '(recentf-max-saved-items 500)
 '(ring-bell-function 'ignore)
 '(save-interprogram-paste-before-kill t nil nil "Obvious choice, so we don’t lose stuff copied from elsewhere.")
 '(savehist-autosave-interval 30)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 0 nil nil "If set to anything larger than 0, ‘pixel-scroll-precision-mode’ causes jitterring when scrolling if point is at the edge of the screen.")
 '(scroll-preserve-screen-position nil)
 '(send-mail-function 'mailclient-send-it)
 '(sentence-end-double-space nil)
 '(show-paren-mode nil nil nil "Since Emacs 28, show-paren mode is on by default, but it only highlights matching paren when point is on a paren, whereas highlight-parentheses-mode always highlights the matching paren.")
 '(split-height-threshold nil nil nil "Don’t split vertically.")
 '(split-width-threshold 150 nil nil "Do split horizontally")
 ;; '(switch-to-buffer-preserve-window-point nil nil nil "Always display buffer’s at its current point please")
 '(tab-always-indent 'complete)
 '(tab-width 4)
 '(use-dialog-box nil)
 '(use-system-tooltips nil)
 '(user-full-name "Yuan Fu")
 '(user-mail-address "casouri@gmail.com")
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(window-resize-pixelwise t)
 '(word-wrap-by-category t)
 '(xref-prompt-for-identifier '(not xref-find-references xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame));; Ends here.
 )

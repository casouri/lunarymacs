;;-*- lexical-binding: t -*-

(require 'luna-f)

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(setq-default
 ad-redefinition-action              'accept     ; silence advised function warnings
 apropos-do-all                      t           ; make `apropos' more useful
 compilation-always-kill             t           ; kill compilation process before starting another
 compilation-ask-about-save          nil         ; save all buffers on `compile'
 confirm-nonexistent-file-or-buffer  t
 idle-update-delay                   2           ; update ui less often

 ;; auto-save
 create-lockfiles                    nil
 history-length                      500
 make-backup-files                   t
 auto-save-default                   t
 delete-auto-save-files              nil
 backup-directory-alist              `((".*" . ,(luna-f-join luna-cache-dir "backup")))
 auto-save-list-file-prefix          (luna-f-join luna-cache-dir "auto-save-list/saves-")
 auto-save-timeout                   5

 ;; files
 abbrev-file-name                    (expand-file-name "abbrev.el" luna-cache-dir)
 recentf-save-file                   (expand-file-name "recentf" luna-cache-dir)
 recentf-max-saved-items             300
 tramp-persistency-file-name         (expand-file-name "tramp" luna-cache-dir)
 bookmark-default-file               (expand-file-name "bookmarks" luna-cache-dir)
 delete-by-moving-to-trash           t
 savehist-file                       (expand-file-name "history" luna-cache-dir)
 auto-save-file-name-transforms      (list (list ".*" (luna-f-join luna-cache-dir "auto-save/") t)
                                           (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                                 (luna-f-join luna-cache-dir "auto-save-remote") t))

 ;; edit
 indent-tabs-mode                    nil
 sentence-end-double-space           nil
 kill-ring-max                       100

 ;; ui
 use-dialog-box                      nil
 visible-cursor                      nil
 use-dialog-box                      nil
 visible-bell                        nil
 frame-title-format                  '("%f")               ; current file name
 display-line-numbers-width          3
 ns-pop-up-frames                    nil                   ; no new frame when emacsclient connected
 )

;;;; start up screen
(setq inhibit-startup-screen t)

;;;; minibuffer
(setq enable-recursive-minibuffers t
      ;; keep the point out of the minibuffer
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt
                  face minibuffer-prompt))

;;;; modes
(blink-cursor-mode -1)
(electric-pair-mode)
(electric-quote-mode)
(minibuffer-electric-default-mode)

;;;; smooth scrolling
(setq scroll-conservatively 101)
;; diabled for emacs-mac port
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse

;;;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;; split screen vertically in ediff
(setq ediff-split-window-function #'split-window-horizontally)

;;;; help
(setq help-window-select t)

;;;; Xref
(setq xref-prompt-for-identifier
      '(not xref-find-references xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame))

;;;; Term mouse
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  ;; (defun track-mouse (e))
  (setq mouse-sel-mode t))

(global-set-key (kbd "<mouse-4>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-5>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-6>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-7>") #'mwheel-scroll)

;;;; natural title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;;; "Dangerous Commands"
(put 'narrow-to-page 'disabled nil)

;;;; Save history
(savehist-mode)
(add-to-list 'savehist-additional-variables 'extended-command-history)

;;;; winner
(winner-mode)

;;;; hideshow
(add-hook 'prog-mode-hook #'hs-minor-mode)


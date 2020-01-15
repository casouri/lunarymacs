;;-*- lexical-binding: t -*-

;;; Setup

(add-hook 'after-init-hook
          (let ((old-list file-name-handler-alist))
            (lambda ()
              (setq file-name-handler-alist old-list
                    gc-cons-threshold 800000
                    gc-cons-percentage 0.1)
              (garbage-collect)))
          t)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

;;; Early init

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(require 'luna-f)

;; (when window-system
;;   (add-hook 'after-init-hook #'toggle-frame-maximized))

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(if (eq window-system 'mac)
    ;; have to enable menu bar on mac port
    ;; otherwise emacs lost focus
    (menu-bar-mode)
  (menu-bar-mode -1))

;;; Package
(require 'lunary)
(require 'cowboy)

(setq package-user-dir (expand-file-name "package" user-emacs-directory))
(cowboy-add-load-path)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq luna-company-manual nil)
(add-to-list 'luna-package-list 'use-package)

(luna-message-error (require 'use-package))
;; core must load first because other configs depends on them
(luna-load-relative "star/builtin-config.el")
(luna-load-relative "star/key.el")
(luna-load-relative "star/recipe.el")
(luna-load-relative "star/angel.el")
(luna-load-relative "star/ui.el")
(luna-load-relative "star/mode-line.el")
(luna-load-relative "star/edit.el")
(luna-load-relative "star/homepage.el")
;; (luna-load-relative "star/helm.el")
(luna-load-relative "star/ivy.el")
(luna-load-relative "star/checker.el")
(luna-load-relative "star/company.el")
(luna-load-relative "star/eglot.el")
(luna-load-relative "star/python.el")
(luna-load-relative "star/elisp.el")
(luna-load-relative "star/git.el")
(luna-load-relative "star/dir.el")
(luna-load-relative "star/org.el")
(luna-load-relative "star/tex.el")
(luna-load-relative "star/term.el")
;; (luna-load-relative "star/shell.el")
(luna-load-relative "star/simple-mode.el")
(require 'utility)

;;; Customize

;;;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(luna-load-or-create custom-file)
(add-hook 'kill-emacs-hook #'customize-save-customized)

;;;; Misc
(setq-default luna-format-on-save t)
(setq-default bidi-display-reordering nil) ;; faster long line
(setq scroll-margin 4)
(setq ispell-program-name "aspell")
(setq user-mail-address "casouri@gmail.com"
      send-mail-function #'sendmail-send-it
      message-send-mail-function #'message-send-mail-with-sendmail)
(setq split-height-threshold nil ; Popup window to right
      split-width-threshold 80)

;;;; theme
(when window-system
  ;; (setq doom-cyberpunk-bg 'violet)
  (setq doom-cyberpunk-dark-mode-line nil)
  (luna-load-theme nil t))

;;;; Font
(when window-system
  (luna-load-font)
  (luna-load-cjk-font))

(setq luna-cjk-font-scale 1.1)

(luna-enable-apple-emoji)

;; WenYue GuDianMingChaoTi (Non-Commercial Use) W5
;; WenYue XHGuYaSong (Non-Commercial Use)
;; WenyueType GutiFangsong (Non-Commercial Use)
;; SiaoyiWangMingBold
;; FZQingKeBenYueSongS-R-GB
;; FZSongKeBenXiuKaiS-R-GB

;;;; nyan
;; (nyan-lite-mode)
;; (setq nyan-wavy-trail t)
;; enabling this makes highlight on buttons blink
;; (nyan-start-animation)

;;;; server
;; checking whether server started can be slow
;; see emacs-horror
(run-with-idle-timer
 3 nil
 (lambda () (ignore-errors (server-start))))

;;;; Mac port
(setq mac-option-modifier 'meta
      mac-command-modifier 'super
      mac-pass-command-to-system nil ; fix cmd h
      mac-system-move-file-to-trash-use-finder t)

(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-v") #'yank)

;;;; notmuch
;; (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/notmuch")
;; (setq notmuch-init-file (luna-f-join user-emacs-directory "star/notmuch-config.el"))
;; (setq message-auto-save-directory "~/mail/draft")
;; (setq message-kill-buffer-on-exit t)
;; (setq notmuch-search-oldest-first nil)
;; (require 'notmuch)

;;;; ENV
(luna-load-env)

;;;; ghelp

(add-to-list 'load-path "~/p/ghelp")
(require 'ghelp)
(run-with-idle-timer
 3 nil
 #'ghelp-global-minor-mode)

;;;; trivial-copy
(luna-when-mac
 (add-to-list 'load-path "~/p/trivial-copy")
 (require 'trivial-copy))

;;;; notdeft
(add-to-list 'load-path "~/attic/notdeft")

;;; -*- lexical-binding: t -*-

;;; Init

(setq-default lexical-binding t)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(setq package-user-dir (expand-file-name "package" user-emacs-directory))
(require 'lunary)
(require 'cowboy)
(require 'luna-f)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (setq package-archives '(("melpa" . "https://elpa.emacs-china.org/melpa/")
;;                          ("gnu" . "https://elpa.emacs-china.org/gnu/")))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;;;; Loadpath

(cowboy-add-load-path)

;;;; Startup setting

(add-hook 'after-init-hook
          ;; make it closure
          (let ()
            (lambda ()
              (setq file-name-handler-alist file-name-handler-alist
                    ;; gc-cons-threshold 800000
                    gc-cons-threshold 8000000
                    gc-cons-percentage 0.1)
              (garbage-collect))) t)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

;;; Package

(setq luna-lsp 'eglot)

(add-to-list 'luna-package-list 'use-package)

(luna-message-error (require 'use-package))
(luna-load-relative "star/core-edit.el")
(luna-load-relative "star/core-ui.el")
;; core must load first because other configs depends on them
(luna-load-relative "star/other.el")
(luna-load-relative "star/key.el")
(luna-load-relative "star/recipe.el")
(luna-load-relative "star/angel.el")
(luna-load-relative "star/ui.el")
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
;; (luna-load-relative "star/shell.el")
(luna-load-relative "star/simple-mode.el")


;;; Customize

;;;; Custom

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(luna-load-or-create custom-file)
(add-hook 'kill-emacs-hook #'customize-save-customized)

;;;; theme
(when window-system
  ;; (setq doom-cyberpunk-bg 'violet)
  (setq doom-cyberpunk-dark-mode-line nil)
  (luna-load-theme nil t))

;;;; Faster long lines
(setq-default bidi-display-reordering nil)

;;;; format on save
(setq-default luna-format-on-save t)

;;;; scroll margin
(setq scroll-margin 4)

 ;;;; Font
(when window-system
  (luna-load-font)
  (luna-load-cjk-font))
;;;;; Chinese

;; WenYue GuDianMingChaoTi (Non-Commercial Use) W5
;; WenYue XHGuYaSong (Non-Commercial Use)
;; WenyueType GutiFangsong (Non-Commercial Use)
;; SiaoyiWangMingBold
;; FZQingKeBenYueSongS-R-GB
;; FZSongKeBenXiuKaiS-R-GB

;; | 对齐 |
;; | good |

(when luna-font
  (add-to-list 'face-font-rescale-alist
               (cons (plist-get (alist-get (intern luna-cjk-font)
                                           luna-cjk-font-alist)
                                :family)
                     1.3)))

;;;;; Emoji
;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
;;                   nil 'prepend)

;;;; nyan
;; (nyan-lite-mode)
;; (setq nyan-wavy-trail t)
;; enabling this makes highlight on buttons blink
;; (nyan-start-animation)

;;;; server
(ignore-errors (server-start))

;;;; Max
(when window-system
  (toggle-frame-maximized))

;;;; Mac port

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

(setq mac-pass-command-to-system nil ; fix cmd h
      mac-system-move-file-to-trash-use-finder t)

(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-v") #'yank)

;;;; path from shell
(when window-system
  (load-package exec-path-from-shell
    :config (exec-path-from-shell-initialize)))

;;;; UI element

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(if (eq window-system 'mac)
    ;; have to enable menu bar on mac port
    ;; otherwise emacs lost focus
    ;; https://bitbucket.org/mituharu/emacs-mac/src/892fa7b2501a403b4f0aea8152df9d60d63f391a/doc/emacs/macport.texi?at=master#macport.texi-529
    (menu-bar-mode)
  (menu-bar-mode -1))


;;;; Term mouse

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  ;; (defun track-mouse (e))
  (setq mouse-sel-mode t))

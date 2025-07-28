;; -*- lexical-binding: t; -*-

;;; Keys

(luna-key-def
 :keymaps 'bklink-minor-mode-map
 "C-c l" #'bklink-summary-mode
 "C-c i" #'bklink-insert
 :keymaps 'text-mode-map
 ;; "<tab>" #'tab-to-tab-stop
 "<C-tab>" '("insert-tab" . (lambda () (interactive) (insert "\t")))
 :keymaps 'texinfo-mode-map
 "C-c l" #'texinfo-caps-to-vars
 :clear
 :leader
 "df" #'xeft)

;;; Packages

(load-package zeft
  :commands zeft
  :config
  (defun iimg-prune-image-data ()
    "Clear any iimg-data in current buffer."
    (require 'iimg)
    (goto-char (point-min))
    (while (re-search-forward iimg--data-regexp nil t)
      (let ((inhibit-read-only t))
        (delete-region (match-beginning 0) (match-end 0)))))
  (add-hook 'zeft-load-file-hook #'iimg-prune-image-data)
  (setq zeft-directory (expand-file-name "~/deft"))
  (add-hook 'zeft-find-file-hook #'auto-fill-mode)
  (set-face-attribute 'zeft-inline-highlight nil
                      :inherit 'highlight))

(load-package bklink
  :commands bklink-minor-mode
  :config (setq bklink-summary-read-only-p t
                bklink-prune-summary-p nil))

(load-package iimg
  :autoload-hook (text-mode-hook . iimg-enable)
  :config (setq iimg-prune-slices-p nil))

(load-package quanjiao
  :autoload-hook (text-mode-hook . quanjiao-mode))

(load-package eclectic-quote
  :commands eclectic-quote-minor-mode)

(load-package xeft
  :commands xeft
  :config
  (setq xeft-directory "~/deft"
        xeft-database "~/deft/db")
  (set-face-attribute 'xeft-inline-highlight nil
                      :inherit 'highlight)
  (defun xeft-setup ()
    (auto-fill-mode))
  (add-hook 'xeft-find-file-hook #'xeft-setup)
  (add-hook 'bklink-minor-mode-hook #'xeft-setup)
  (luna-load-font-spec
   'xeft-excerpt-title
   '("IBM Plex Sans" "Source Han Sans SC" 1
     (:weight semi-bold) (:weight medium))
   16)
  (luna-load-font
   'xeft-excerpt-body "IBM Plex Sans" 16))

(load-package flique :defer t)

(load-package eclectic-quote
  :commands eclectic-quote-minor-mode)

(with-eval-after-load 'texinfo
  (defun texinfo-caps-to-vars ()
    "Transform NAME at point to @var{name}."
    (interactive)
    (skip-chars-forward " ")
    (let ((var (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (delete-region (car bounds) (cdr bounds))
      (insert (format "@var{%s}" (downcase var)))))
  (defun setup-texinfo ()
    "Setup for texinfo mode."
    (electric-quote-local-mode -1)
    (setq-local outline-regexp (rx (or "@heading" "@subheading" "@defun" "@defvar" "@section" "@subsection" "@subsubsection")))
    (setq-local outline-minor-mode-highlight 'append)
    (setq-local outline-level (lambda () 2))
    (outline-minor-mode))

  (add-hook 'texinfo-mode-hook #'setup-texinfo))

(defun setup-text-mode ()
  "Setup for ‘text-mode’."
  ;; Stolen from ‘markdown-mode’, this way we can fill individual list
  ;; items.
  (setq paragraph-start "\f\\|[ 	\f]*$\\|\\(?:[ 	]*>\\)+[ 	\f]*$\\|[ 	]*[*+-][ 	]+\\|[ 	]*\\(?:[0-9]+\\|#\\)\\.[ 	]+\\|[ 	]*\\[\\S-*\\]:[ 	]+\\|[ 	]*:[ 	]+\\|^|"))
(add-hook 'text-mode-hook #'setup-text-mode)

;;; Deprecated

(when nil
  (load-package binder
    :commands
    binder-toggle-sidebar
    binder-reveal-in-sidebar)

  (load-package iscroll
    :autoload-hook (text-mode-hook . iscroll-mode)
    :config
    (setq iscroll-preserve-screen-position t))

  (load-package multi-translate
    :commands multi-translate-at-point
    :config (push 'youdao multi-translate-word-backends)
    :init (defalias 'mtran 'multi-translate-at-point))

  (defun dic ()
    "Open Dictionary.app and lookup word at point."
    (interactive)
    (shell-command-to-string
     (format "open dict://%s" (word-at-point))))
  )

;; -*- lexical-binding: t; -*-

;;; Keys

(luna-def-key
 :keymaps 'bklink-minor-mode-map
 "C-c l" #'bklink-show-back-link
 "C-c i" #'bklink-insert
 :keymaps 'text-mode-map
 ;; "<tab>" #'tab-to-tab-stop
 "<C-tab>" '("insert-tab" . (lambda () (interactive) (insert "\t")))
 :clear
 :leader
 "df" #'xeft)

;;; Packages

;; (load-package binder
;;   :commands
;;   binder-toggle-sidebar
;;   binder-reveal-in-sidebar)

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
  :commands bklink-minor-mode)

(load-package iimg
  :hook (text-mode-hook . iimg-enable))

(load-package quanjiao
  :hook (text-mode-hook . quanjiao-mode))

(load-package iscroll
  :hook (text-mode-hook . iscroll-mode)
  :config
  (setq iscroll-preserve-screen-position t))

;; (load-package multi-translate
;;   :commands multi-translate-at-point
;;   :config (push 'youdao multi-translate-word-backends)
;;   :init (defalias 'mtran 'multi-translate-at-point))

;; (defun dic ()
;;   "Open Dictionary.app and lookup word at point."
;;   (interactive)
;;   (shell-command-to-string
;;    (format "open dict://%s" (word-at-point))))

(load-package xeft
  :commands xeft
  :config
  (require 'flique)
  (setq xeft-directory "~/deft"
        xeft-database "~/deft/db")
  (set-face-attribute 'xeft-inline-highlight nil
                      :inherit 'highlight)
  (defun xeft-setup ()
    (auto-fill-mode)
    (flique-append-to-index (buffer-file-name))
    (local-set-key (kbd "M-]") #'flique-forward)
    (local-set-key (kbd "M-[") #'flique-backward)
    (flique-show-navigation))
  (add-hook 'xeft-find-file-hook #'xeft-setup)
  (add-hook 'bklink-minor-mode-hook #'xeft-setup))

(load-package flique :defer t)

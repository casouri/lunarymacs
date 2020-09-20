;; -*- lexical-binding: t; -*-

;;; Keys

(luna-def-key
 :keymaps 'bklink-minor-mode-map
 "C-c l" #'bklink-show-back-link
 "C-c i" #'bklink-insert
 :keymaps 'text-mode-map
 "TAB" #'indent-for-tab-command
 "<C-tab>" '("insert-tab" . (lambda () (interactive) (insert "\t"))))

;;; Packages

;; (load-package binder
;;   :commands
;;   binder-toggle-sidebar
;;   binder-reveal-in-sidebar)

(load-package deft
  :commands deft
  :config
  (require 'iimg)
  (require 'bklink)
  (require 'color-outline)
  :hook
  (deft-open-file-hook . (color-outline-mode-maybe
                          bklink-minor-mode
                          auto-fill-mode))
  (deft-cache-file-hook . iimg-clean-data)
  :config
  (defun iimg-clean-data ()
    "Clear any iimg-data in current buffer."
    (goto-char (point-min))
    (while (re-search-forward iimg--data-regexp nil t)
      (let ((inhibit-read-only t))
        (delete-region (match-beginning 0) (match-end 0)))))
  (push '(text-mode "#") color-outline-comment-char-alist)
  (setq deft-directory (expand-file-name "~/deft/")
        deft-use-filter-string-for-filename t))

(load-package bklink
  :hook
  (bklink-minor-mode-hook
   . (lambda () (if bklink-minor-mode (color-outline-mode-maybe))))
  :config (setq bklink-more-match t))

(load-package iimg
  :hook (text-mode-hook . iimg-minor-mode))

(load-package quanjiao
  :hook (text-mode-hook . quanjiao-mode))

(load-package multi-translate
  :commands multi-translate-at-point
  :config (push 'youdao multi-translate-word-backends)
  :init (defalias 'mtran 'multi-translate-at-point))

(defun dic ()
  "Open Dictionary.app and lookup word at point."
  (interactive)
  (shell-command-to-string
   (format "open dict://%s" (word-at-point))))

;;; end

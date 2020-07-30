;; -*- lexical-binding: t; -*-

;;; Keys

(luna-def-key
 :keymaps 'bklink-minor-mode-map
 "C-c l" #'bklink-toggle-back-link
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
  :hook (deft-open-file-hook . (color-outline-mode-maybe
                                bklink-minor-mode
                                auto-fill-mode))
  :config
  (push '(text-mode "#") color-outline-comment-char-alist)
  (setq deft-directory (expand-file-name "~/deft/")
        deft-use-filter-string-for-filename t))

(load-package bklink
  :hook (text-mode-hook . bklink-minor-mode))

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

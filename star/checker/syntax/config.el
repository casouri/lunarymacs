;;; -*- lexical-binding: t -*-

(use-package| flycheck
  ;; :commands global-flycheck-mode
  ;; :init (add-hook-for-once| prog-mode-hook (lambda () (global-flycheck-mode 1)))
  :config
  (require 'exec-path-from-shell)
  (global-flycheck-mode 1)
  )

(post-config| general
  (default-leader
    "el" #'flycheck-list-errors))


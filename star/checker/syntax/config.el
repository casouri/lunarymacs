;;; -*- lexical-binding: t -*-

(use-package| flycheck
  :after exec-path-from-shell
  ;; :commands global-flycheck-mode
  ;; :init (add-hook-for-once| prog-mode-hook (lambda () (global-flycheck-mode 1)))
  :config (global-flycheck-mode 1)
  )

(post-config| general
  (default-leader
    "en" #'flycheck-next-error
    "ep" #'flycheck-previous-error
    "el" #'flycheck-list-errors))


;;; -*- lexical-binding: t -*-

(use-package| flycheck
  :after exec-path-from-shell
  :config (flycheck-mode 1)
  )

(post-config| general
  (default-leader
    "en" #'flycheck-next-error
    "ep" #'flycheck-previous-error
    "el" #'flycheck-list-errors))


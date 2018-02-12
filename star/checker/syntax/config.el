;;; -*- lexical-binding: t -*-

(use-package| flycheck
  :delight (flycheck-mode " ⓕ")
  :after exec-path-from-shell
  :hook (prog-mode . flycheck-mode)
  )

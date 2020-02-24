;; -*- lexical-binding: t -*-

(with-eval-after-load 'luna-general-config
  (luna-default-leader
    "gs" #'magit-status
    "gf" '(:ignore t :which-key "file")
    "gfc" #'magit-file-checkout
    "gfl" #'magit-log-buffer-file))

(load-package magit
  :commands magit-status
  :config
  (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)
  (add-to-list 'luna-buffer-bottom-list "magit:")
  (add-to-list 'luna-buffer-bottom-list "magit-process:"))

(load-package magit-patch-changelog
  :after magit)

(load-package magit-todos
  :hook (magit-mode . magit-todos-mode))

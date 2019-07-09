;;; -*- lexical-binding: t -*-

(load-package magit
  :commands magit-status
  :config (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle))

(luna-with-eval-after-load 'key.general
  (luna-default-leader
    "gs" #'magit-status
    "gf" '(:ignore t :which-key "file")
    "gfc" #'magit-file-checkout
    "gfl" #'magit-log-buffer-file))

(load-package magit-todos
  :hook (magit-mode . magit-todos-mode))

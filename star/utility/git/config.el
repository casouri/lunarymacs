;;; -*- lexical-binding: t -*-

(use-package| magit
  :commands magit-status
  :config (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle))

(post-config| general
  (default-leader
    "gs" #'magit-status
    "gfc" #'magit-file-checkout
    "gfl" #'magit-log-buffer-file))

(use-package| evil-magit
  :defer t
  :hook (magit-mode . (lambda () (require 'evil-magit))))

(use-package| magit-todos
  :after magit)

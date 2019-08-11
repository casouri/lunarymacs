;;; ivy.el --- Ivy config      -*- lexical-binding: t; -*-

(luna-with-eval-after-load 'key.general
  (luna-default-leader
    "s s" #'swiper))

(load-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t))

(load-package swiper
  :commands (swiper))

(load-package counsel
  :config (counsel-mode))


;;; ivy.el ends here

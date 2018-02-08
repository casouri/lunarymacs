;;; -*- lexical-binding: t -*-

(use-package| neotree
  :commands neotree-toggle
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  (require 'all-the-icons)
  )

(post-config| general
  (default-leader
    "tn" #'neotree-toggle))

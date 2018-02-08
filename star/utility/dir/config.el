;;; -*- lexical-binding: t -*-

(use-package| neotree
  :commands neotree-toggle
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :config
  (require 'all-the-icons)
  )

(post-config| general
  (default-leader
    "tn" #'neotree-toggle)
  (general-define-key
   :states 'normal
   :keymaps 'neotree-mode-map
   "TAB" #'neotree-enter
   "SPC" #'neotree-quick-look
   "q" #'neotree-hide
   "RET" #'neotree-enter
   ))

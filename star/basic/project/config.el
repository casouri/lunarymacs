;;; -*- lexical-binding: t -*-

(use-package| projectile
  :init (setq projectile-known-projects-file
              (concat moon-local-dir "projectile-bookmarks.eld"))
  :config (projectile-mode 1))

(use-package| counsel-projectile
  :commands
  (counsel-projectile
   counsel-projectile-find-file
   counsel-projectile-find-dir
   counsel-projectile-switch-to-buffer
   counsel-projectile-rg)
  :config (counsel-projectile-mode 1))

(post-config| general
  (default-leader
    "p SPC" #' counsel-projectile
    "pf"    #'counsel-projectile-find-file
    "pd"    #'counsel-projectile-find-dir
    "pb"    #'counsel-projectile-switch-to-buffer
    "ps"    #'counsel-projectile-rg
    "pp"    #'counsel-projectile-switch-project
    "pr"    #'projectile-recentf))

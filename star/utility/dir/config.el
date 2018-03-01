;;; -*- lexical-binding: t -*-

;;
;; Func
;;


(use-package| neotree
  :commands neotree-toggle
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-show-hidden-files t)
  :config
  (require 'all-the-icons)
  )

(post-config| general
  (default-leader
    "tn" #'neotree-toggle
    "tr" #'ranger
    "th" #'moon/toggle-hidden-file)
  (general-define-key
   :states 'normal
   :keymaps 'neotree-mode-map
   "TAB" #'neotree-enter
   "SPC" #'neotree-quick-look
   "q" #'neotree-hide
   "RET" #'neotree-enter
   ))

(use-package| ranger
  :init (setq ranger-show-hidden t)
  :commands ranger
  )

(use-package| dired-narrow
  :commands dired-narrow
  :config (define-key dired-mode-map "F" #'dired-narrow))

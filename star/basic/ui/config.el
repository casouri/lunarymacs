;;
;; Config
;;

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

(global-hl-line-mode 1)

;;
;; Package
;;

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode)
  )


;; (use-package powerline
;;   :config
;;   (require 'powerline)
;;   (setq powerline-default-separator 'slant)
;;   (setq powerline-image-apple-rgb t)
;;   (setq powerline-height 26)
;;   (powerline-default-theme)
;;   )

(use-package spaceline
  :defer t
  :init (add-hook
         'moon-post-init-hook
         (lambda ()
           (require 'spaceline-config)
           (setq powerline-default-separator 'slant)
           (setq powerline-image-apple-rgb t)
           (setq powerline-height 26)
           (spaceline-spacemacs-theme)
           (kill-buffer)
           (get-buffer-create moon-homepage-buffer)
           (switch-to-buffer moon-homepage-buffer)
           (moon-draw-homepage)
           )
         t
         )
  )



(use-package nlinum
  :hook (prog-mode . nlinum-mode)
  :defer t
  )

;; (use-package hlinum
;;   :hook linum-mode
;;   :config
;;   (hlinum-activate)
;;   (moon/sync-hlinum-face)
;;   (add-hook 'spacemacs-post-theme-change-hook #'sync-hlinum-face)
;;   )

(use-package diminish
  :config (diminish 'eldoc-mode))

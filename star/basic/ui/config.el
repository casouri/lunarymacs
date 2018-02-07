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
  :hook prog-mode
  :config (rainbow-delimiters-mode 1))


(use-package powerline
  :defer t
  :init
  (add-hook
   'moon-post-init-hook
   (lambda ()
     (require 'powerline)
     (setq powerline-default-separator 'utf-8)
     (powerline-default-theme)
     )
   )
  )

(use-package nlinum
  :config
  (global-nlinum-mode 1))

;; (use-package hlinum
;;   :hook linum-mode
;;   :config
;;   (hlinum-activate)
;;   (moon/sync-hlinum-face)
;;   (add-hook 'spacemacs-post-theme-change-hook #'sync-hlinum-face)
;;   )

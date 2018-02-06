;;
;; Config
;;

(global-hl-line-mode 1)

(use-package rainbow-delimiters
	      :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; (use-package| highlight-parentheses
;; 	      :hook prog-mode
;; 	      :config (global-highlight-parentheses-mode t))

;; (use-package spaceline
;;   :config
;;   (add-hook 'moon-post-init-hook
;; 	    (lambda ()
;; 	      (require 'spaceline-config)
;; 	      (spaceline-spacemacs-theme)
;; 	      (setq powerline-default-separator 'utf-8)
;; 	      (spaceline-compile)))
;;   )

(use-package hlinum
  :hook linum-mode
  :config
  (hlinum-activate)
  (sync-hlinum-face)
  (add-hook 'spacemacs-post-theme-change-hook #'sync-hlinum-face)
  )

;;
;; Var
;;

;; https://www.reddit.com/r/emacs/comments/4v7tcj/does_emacs_have_a_hook_for_when_the_theme_changes/


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

(use-package| rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode)
  )


;; (use-package| powerline
;;   :config
;;   (require 'powerline)
;;   (setq powerline-default-separator 'slant)
;;   (setq powerline-image-apple-rgb t)
;;   (setq powerline-height 26)
;;   (powerline-default-theme)
;;   )

(use-package| spaceline
  :defer t
  :init (add-hook
         'moon-post-init-hook
         (lambda ()
           (run-at-time
            "0.3 sec" nil
            (lambda ()
              (require 'spaceline-config)
              (setq powerline-default-separator 'slant)
              (setq powerline-image-apple-rgb t)
              (setq powerline-height 26)
              (spaceline-spacemacs-theme)
              (kill-buffer moon-homepage-buffer)
              (get-buffer-create moon-homepage-buffer)
              (switch-to-buffer moon-homepage-buffer)
              (moon/draw-homepage)
              )
            )
           )
         t
         )
  )



(use-package| nlinum
  :hook (prog-mode . nlinum-mode)
  :config
  (moon/match-number-line-backgroud-color)
  (add-hook 'moon-post-load-theme-hook #'moon/match-number-line-backgroud-color)
  )

;; (use-package| hlinum
;;   :hook linum-mode
;;   :config
;;   (hlinum-activate)
;;   (moon/sync-hlinum-face)
;;   (add-hook 'spacemacs-post-theme-change-hook #'sync-hlinum-face)
;;   )

(use-package| diminish
  :config (diminish 'eldoc-mode))

;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar moon-spaceline-on-homepage nil
  "Whether to refresh homepage to see spaceline.

I create a separate thred to load spaceline, 
so homepage will use vanilla modeline.
If you want spaceline on homepage, set this to t 
and emacs will refresh homepage to update modeline.")

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



(use-package| spaceline
  :defer t
  :init
  (make-thread
   (lambda ()
     (require 'spaceline-config)
     (setq powerline-default-separator 'slant)
     (setq powerline-image-apple-rgb t)
     (setq powerline-height 26)
     (spaceline-spacemacs-theme)
     (when moon-spaceline-on-homepage
       (run-at-time "0.3 sec" nil
                    (lambda ()
                      (moon/redraw-homepage))))
     ;; fix different separator color problem after changing theme
     (add-hook 'moon-load-theme-hook #'powerline-reset))
   "spaceline-init")
  )


(use-package| linum
  :hook (prog-mode . linum-mode)
  :config
  (moon/sync-number-line-face)
  (add-hook 'moon-load-theme-hook #'moon/sync-number-line-face)
  )

(use-package| hlinum
  :hook (linum-mode . hlinum-activate)
  :config
  (moon/sync-hlinum-face)
  (add-hook 'moon-load-theme-hook #'moon/sync-hlinum-face)
  )

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

(use-package| rainbow-mode
  :commands rainbow-mode)


(use-package| spaceline
  :defer t
  :init
  (defun moon-load-spaceline ()
    (require 'spaceline-config)
    (setq powerline-default-separator 'slant)
    (setq powerline-image-apple-rgb t)
    (setq powerline-height 28)
    (spaceline-spacemacs-theme)
    ;; fix different separator color problem after changing theme
    (add-hook 'moon-load-theme-hook #'powerline-reset))
  (add-hook-for-once| prog-mode-hook moon-load-spaceline))


(use-package| nlinum
  :hook (prog-mode . linum-mode)
  :init 
  (add-hook 'moon-load-theme-hook #'moon/sync-linum-face)
  (add-hook 'prog-mode-hook #'moon/sync-linum-face t)
  :config
  (moon/sync-linum-face)
  )

(use-package| hlinum
  :hook (linum-mode . hlinum-activate)
  :config
  (moon/sync-hlinum-face)
  (add-hook 'moon-load-theme-hook #'moon/sync-hlinum-face)
  )

(use-package| nyan-mode
  :init(setq nyan-wavy-trail t)
  :hook (prog-mode . nyan-mode)
  :config
  (nyan-start-animation)
  )

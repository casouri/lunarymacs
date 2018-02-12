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
                      (moon/redraw-homepage)
                      )
                    )
       )
     )
   "spaceline-init"
   )
  )


(use-package| linum
  :hook (prog-mode . linum-mode)
  :config
  (moon/match-number-line-backgroud-color)
  (add-hook 'moon-post-load-theme-hook #'moon/match-number-line-backgroud-color)
  )

(use-package| hlinum
  :hook (nlinum-mode . hlinum-activate)
  :config
  (moon/sync-hlinum-face)
  )

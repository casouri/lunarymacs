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


(global-hl-line-mode 1)

;;
;; Package
;;

(use-package| rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package| rainbow-mode
  :commands rainbow-mode)

(use-package| highlight-parentheses
  :init
  ;; show only the most inner pair
  (setq hl-paren-colors '("green"))
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (global-highlight-parentheses-mode 1))


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

;; (use-package| powerline
;;   :config
;;     (setq powerline-default-separator 'slant)
;;     (setq powerline-image-apple-rgb t)
;;     (setq powerline-height 28)
;;     (powerline-default-theme))

(use-package| nlinum
  :config (global-nlinum-mode)
  :init 
  (add-hook 'moon-load-theme-hook #'moon/sync-nlinum-face)
  (add-hook 'moon-load-theme-hook #'moon/sync-nlinum-highlight-face)
  (setq nlinum-highlight-current-line t)
  :config
  (moon/sync-nlinum-face)
  (moon/sync-nlinum-highlight-face)
  )


(use-package| nyan-mode
  :init(setq nyan-wavy-trail t)
  :config
  (nyan-mode)
  (nyan-start-animation)
  )

(use-package| hl-todo
  :defer t
  :init
  (defun setup-hl-todo ()
    (require 'hl-todo)
    (global-hl-todo-mode))
  (add-hook-for-once|
   after-change-major-mode-hook
   setup-hl-todo))

(use-package| dim
  :after spaceline
  :config
  (dim-minor-names
   '((eldoc-mode "" eldoc)
     (auto-revert-mode "" autorevert)
     (visual-line-mode "" simple)
     (evil-escape-mode "" evil-escape)
     (undo-tree-mode "" undo-tree)
     (which-key-mode "" which-key)
     (company-mode " Ⓒ" company)
     (flycheck-mode " ⓕ" flycheck)
     (ivy-mode " ⓘ" ivy)
     (lsp-mode " Ⓛ" lsp)
     (lispyville-mode " ⓟ" lispyville)
     (highlight-parentheses-mode "")
     ))
  )

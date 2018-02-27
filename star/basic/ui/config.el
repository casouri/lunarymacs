;;; -*- lexical-binding: t -*-

;;
;; Config
;;

(global-hl-line-mode 1)

;;
;; Cursor Color
;;


(defun moon-normal-state-cursor-color ()
  "Cursor color in normal state."
  (cond
   ((equal moon-current-theme "spacemacs-dark")
    lunary-yellow)
   ((equal moon-current-theme "spacemacs-light")
    spacemacs-light-purple)
   ))

(defun moon-insert-state-cursor-color ()
  "Cursor color in insert state."
  lunary-pink)

(change-cursor-on-hook| evil-normal-state-entry-hook moon-normal-state-cursor-color)
;; secure cursor color after changing theme
(change-cursor-on-hook| moon-load-theme-hook moon-normal-state-cursor-color)
(change-cursor-on-hook| evil-insert-state-entry-hook moon-insert-state-cursor-color)

;;
;; Font
;;

;; (moon-set-font| :family "Source Code Pro" :weight 'light :size 14)
(moon-set-font| :family "SF Mono" :weight 'light :size 14)

;;
;; Package
;;

(use-package| spacemacs-theme
  :defer t
  :init
  (add-to-list 'custom-theme-load-path (car (directory-files (concat moon-package-dir "elpa/") t "spacemacs-theme.+")) t)
  (load-theme 'spacemacs-dark t))

(use-package| rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package| rainbow-mode
  :commands rainbow-mode)

(use-package| highlight-parentheses
  :init
  ;;highlight only the most inner pair
  (setq hl-paren-colors '("green"))
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (global-highlight-parentheses-mode 1))


;; (use-package| spaceline
;;   :defer t
;;   :init
;;   (defun moon-load-spaceline ()
;;     (require 'spaceline-config)
;;     (setq powerline-default-separator 'slant)
;;     (setq powerline-image-apple-rgb t)
;;     (setq powerline-height 28)
;;     (spaceline-spacemacs-theme)
;;     ;; fix different separator color problem after changing theme
;;     (add-hook 'moon-load-theme-hook #'powerline-reset))
;;   (add-hook-for-once| prog-mode-hook moon-load-spaceline)
;;   )

(use-package| powerline
  :defer t
  :init
  (defun moon-load-powerline ()
    (setq powerline-default-separator 'slant)
    (setq powerline-image-apple-rgb t)
    (setq powerline-height 28)
    (load (concat moon-star-dir "basic/ui/lunaryline"))
    (lunaryline-default-theme)
    ;; fix different separator color after changing theme
    (add-hook 'moon-load-theme-hook #'powerline-reset)
    )
  ;; (add-hook 'moon-init-hook #'moon-load-powerline)
  (add-hook-for-once| prog-mode-hook moon-load-powerline)
  (add-hook-for-once| text-mode-hook moon-load-powerline)
  ;; (moon-load-powerline)
  )

;; not working
;; (after-load| spacemacs-theme
;;   (set-face-attribute 'powerline-active1 nil :foreground "black")
;;   )


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

(defvar moon-enable-nyan nil
  "Whether to enable nyan cat")

(use-package| nyan-mode
  :init (setq nyan-wavy-trail t)
  :config
  (when moon-enable-nyan
    (nyan-mode)
    (nyan-start-animation)
    )
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

;; I don't show minor mode
;; in modeline anymore

;; (use-package| dim
;;   :after powerline
;;   :config
;;   (dim-minor-names
;;    '((eldoc-mode "" eldoc)
;;      (auto-revert-mode "" autorevert)
;;      (visual-line-mode "" simple)
;;      (evil-escape-mode "" evil-escape)
;;      (undo-tree-mode "" undo-tree)
;;      (which-key-mode "" which-key)
;;      (company-mode " Ⓒ" company)
;;      (flycheck-mode " ⓕ" flycheck)
;;      (ivy-mode " ⓘ" ivy)
;;      (lsp-mode " Ⓛ" lsp)
;;      (lispyville-mode " ⓟ" lispyville)
;;      (highlight-parentheses-mode "")
;;      (counsel-mode "" counsel)
;;      (flyspell-mode " Ⓢ" flyspell)
;;      ))
;;   )

(use-package| eyebrowse
  :config
  (eyebrowse-mode 1))

(post-config| general
  (default-leader
    "w`" #'delete-other-windows
    "w1" #'eyebrowse-switch-to-window-config-1
    "w2" #'eyebrowse-switch-to-window-config-2
    "w3" #'eyebrowse-switch-to-window-config-3
    "w4" #'eyebrowse-switch-to-window-config-4
    "w5" #'eyebrowse-switch-to-window-config-5
    "w6" #'eyebrowse-switch-to-window-config-6
    "wd" #'eyebrowse-close-window-config))


(use-package| winum
  :config (winum-mode 1))

(post-config| general
  (default-leader
    "1" #'moon/switch-to-window-1
    "2" #'moon/switch-to-window-2
    "3" #'moon/switch-to-window-3
    "4" #'moon/switch-to-window-4
    "5" #'moon/switch-to-window-5
    "6" #'moon/switch-to-window-6
    "7" #'moon/switch-to-window-7
    "8" #'moon/switch-to-window-8
    "9" #'moon/switch-to-window-9
    ))


(post-config| which-key
  ;; create a fake key to represent all ten winum function
  (push '(("\\(.*\\) 1" . "moon/switch-to-window-1") . ("\\1 1..9" . "window 1..9")) which-key-replacement-alist)
  ;; hide other keys
  (push '((nil . "moon/switch-to-window-[2-9]") . t) which-key-replacement-alist)
  ;; create a fake key to represent all ten eyebrowse function
  (push '(("\\(.*\\) 1" . "eyebrowse-switch-to-window-config-1") . ("\\1 1..9" . "workspace 1..9")) which-key-replacement-alist)
  ;; hide other keys
  (push '((nil . "eyebrowse-switch-to-window-config-[2-9]") . t) which-key-replacement-alist)
  )


;;
;; Desktop
;;

(defvar moon-save-desktop nil
  "Whether to save desktop across session.")

(add-hook 'kill-emacs-hook
             (lambda ()
               (when moon-save-desktop
                 (desktop-save moon-local-dir))))

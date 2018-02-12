;;; -*- lexical-binding: t -*-

(use-package| company
  :delight (company-mode " Ⓒ")
  :hook (prog-mode . company-mode)
  :defer t
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  )

(load| short-finger)

(add-hook 'org-mode-hook #'short-finger-mode)
(add-hook 'markdown-mode-hook #'short-finger-mode)

(use-package| yasnippet
  :delight (yas-minor-mode " Ⓨ")
  :init
  (add-to-list 'load-path (concat moon-emacs-d-dir "snippet/"))
  (add-hook-for-once| prog-mode-hook #'yas-global-mode)
  )

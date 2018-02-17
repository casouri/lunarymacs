;;; -*- lexical-binding: t -*-

(use-package| company
  :delight (company-mode " Ⓒ")
  ;; :hook (prog-mode . company-mode)
  :defer 2.5
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-require-match nil
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
  :defer 2.5
  :delight (yas-minor-mode " Ⓨ")
  ;; :hook (prog-mode . yas-minor-mode)
  :init
  (setq yas-snippet-dirs (list (concat moon-emacs-d-dir "snippet/")))
  (setq yas-verbosity 0) ; don't message anything
  (add-hook-for-once| prog-mode-hook #'yas-global-mode)
  :config
  (yas-minor-mode)
  (yas-reload-all)
  )

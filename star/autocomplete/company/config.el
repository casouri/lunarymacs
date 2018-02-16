;;; -*- lexical-binding: t -*-

(delay-use-package| company
  :delight (company-mode " Ⓒ")
  ;; :hook (prog-mode . company-mode)
  :defer t
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

(delay-use-package| yasnippet
  :delight (yas-minor-mode " Ⓨ")
  ;; :hook (prog-mode . yas-minor-mode)
  :init
  (add-to-list 'load-path (concat moon-emacs-d-dir "snippet/"))
  (add-hook-for-once| prog-mode-hook #'yas-global-mode)
  :config
  (yas-minor-mode)
  (yas-reload-all)
  )

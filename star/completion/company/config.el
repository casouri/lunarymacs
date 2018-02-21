;;; -*- lexical-binding: t -*-

(use-package| company
  :delight (company-mode " Ⓒ")
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

(post-config| delight
  (delight 'short-finger-mode " ⓢ" "short-finger"))

(add-hook 'org-mode-hook #'short-finger-mode)
(add-hook 'markdown-mode-hook #'short-finger-mode)

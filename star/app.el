;; -*- lexical-binding: t -*-
;;
;; Standalone applications.

;;; Key

(luna-key-def
 "C-h C-h" #'ghelp-describe
 "C-h r"   #'ghelp-resume
 "C-h o"   #'ghelp-describe-elisp
 "C-h f"   #'ghelp-describe-function
 "C-h v"   #'ghelp-describe-variable
 "C-h k"   #'ghelp-describe-key)

;;; Package

(load-package keycast
  :commands
  keycast-mode
  keycast-log-mode
  :init (setq keycast-insert-after 'mode-line-misc-info))

(with-eval-after-load 'erc
  (setq erc-nick "yuan"
        erc-prompt-for-nickserv-password nil
        erc-fill-column 70
        erc-port 6667)
  (require 'erc-services)
  (erc-services-mode))

(load-package helpful :defer)

(load-package ghelp
  :commands
  ghelp-describe
  ghelp-describe-function
  ghelp-describe-variable
  ghelp-describe-key
  :config
  (defun luna-ghelp-hook ()
    (setq truncate-lines t))
  :hook (ghelp-mode-hook . luna-ghelp-hook))

(load-package inspector
  :commands inspect-expression
  :config
  (setq inspector-end-column 70)
  (defun inspector-quit ()
    "Quit the Emacs inspector."
    (interactive)
    (setq inspector-history nil)
    (if (window-prev-buffers)
        (quit-window)
      (delete-window))))

(load-package tramp
  :defer
  :init
  (setq tramp-default-method "ssh")
  ;; `tramp-backup-directory-alist' is for remote backup on remote
  ;; machines, so it is not useful. The following is the real way:
  ;; REF: https://stackoverflow.com/questions/3893727/setting-emacs-tramp-to-store-local-backups
  (let ((backup-dir (expand-file-name
                     "var/tramp/backup" user-emacs-directory)))
    (unless (file-exists-p backup-dir)
      (mkdir backup-dir t))
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp backup-dir)))
  :config
  ;; Make sure TRAMP works with guix.
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path
                  "~/.guix-profile/bin" "~/.guix-profile/sbin"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"))))

;; (load-package rime
;;   :defer
;;   :config
;;   (luna-on "Brown"
;;     (setq rime-show-candidate 'posframe
;;           rime-posframe-style 'vertical
;;           rime-user-data-dir "/Users/yuan/Library/Rime"
;;           rime-librime-root "/opt/local"
;;           rime-show-preedit 'inline
;;           rime-emacs-module-header-root "~/emacs-head/src"
;;           rime-posframe-properties (list :font "Source Han Sans SC"
;;                                          :weight 'light
;;                                          :internal-border-width 10))
;;     (add-hook 'input-method-activate-hook
;;               (lambda () (interactive)
;;                 (setq-local cursor-type 'hollow)))
;;     (add-hook 'input-method-inactivate-hook
;;               (lambda () (interactive)
;;                 (kill-local-variable 'cursor-type)))))

(with-eval-after-load 'gnus
  (setq gnus-select-method
        '(nntp "news.gmane.io" (nntp-port-number 119))
        gnus-visible-headers "^From:\\|^Subject:"
        gnus-use-full-window nil
        gnus-summary-line-format "%U%R%z%(%*%[%n%]%) %s\n"))

(load-package restclient
  :commands restclient-mode)

(defun luna-customize-setup ()
  "Setup function for Customize."
  (buffer-face-set '(:family "SF Pro Text" :height 150))
  (setq truncate-lines t
        line-spacing 0.2))
(add-hook 'Custom-mode-hook #'luna-customize-setup)

(load-package vterm
  :commands vterm)

(load-package dwim-shell-command
  :config (require 'dwim-shell-commands))

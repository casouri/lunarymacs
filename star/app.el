;; -*- lexical-binding: t -*-
;;
;; Standalone applications.

(with-eval-after-load 'erc
  (setq erc-nick "casouri"
        erc-nickserv-passwords
        '((freenode (("casouri" . "XF234567ic"))))
        erc-prompt-for-nickserv-password nil)
  (require 'erc-services)
  (erc-services-mode))

(load-package helpful)

(load-package ghelp
  :commands
  ghelp-describe
  ghelp-describe-function
  ghelp-describe-variable)

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

(load-package rime
  :defer
  :config
  (luna-on "Brown"
    (setq rime-show-candidate 'posframe
          rime-posframe-style 'vertical
          rime-user-data-dir "/Users/yuan/Library/Rime"
          rime-librime-root "/opt/local"
          rime-show-preedit 'inline
          rime-emacs-module-header-root "~/emacs-head/src"
          rime-posframe-properties (list :font "Source Han Sans"
                                         :weight 'light
                                         :internal-border-width 10))
    (add-hook 'input-method-activate-hook
              (lambda () (interactive)
                (setq-local cursor-type 'hbar)))
    (add-hook 'input-method-inactivate-hook
              (lambda () (interactive)
                (kill-local-variable 'cursor-type)))))

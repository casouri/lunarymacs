;; -*- lexical-binding: t -*-

(require 'luna-f)
(require 'outline+)

;;; Key

(luna-def-key
 :leader
 "hh" #'hs-hide-all
 "hs" #'hs-show-all
 "rr" #'vr/replace

 :---
 "C-h C-h" #'ghelp-describe
 "C-h r"   #'ghelp-resume
 "C-h o"   #'ghelp-describe-elisp
 "C-h f"   #'ghelp-describe-function
 "C-h v"   #'ghelp-describe-variable
 "C-h k"   #'ghelp-describe-key
 "C-x i"   #'luna-insert-special-symbol
 "C-c '"   #'separedit
 "C-/"     #'undo-only
 "C-."     #'undo-redo
 ;; "C-c C-h" #'hs-toggle-hiding
 "C-="     #'er/expand-region
 "C--"     #'er/contract-region
 "M-q"     #'ftable-fill
 :keymaps 'override
 "C-j"     #'avy-goto-word-1
 :keymaps '(outline-minor-mode-map org-mode-map outline-mode-map)
 "s-i"     #'outline-cycle
 "C-s-i"   #'outline-cycle-buffer
 "M-]"     '("outline-next-heading"
             . (lambda () (interactive)
                 (outline-next-heading)
                 (recenter-top-bottom)))
 "M-["     '("outline-previous-heading"
             . (lambda () (interactive)
                 (outline-previous-heading)
                 (recenter-top-bottom)))
 :keymaps 'rime-active-mode-map
 "C-\\"    (lambda () (interactive)
             (rime--return)
             (toggle-input-method))
 ;; "<tab>" #'rime-inline-ascii
 :keymaps 'rime-mode-map
 "C-`" #'rime-send-keybinding)


;;; Package

(load-package avy
  :commands avy-goto-word-1)


(load-package ws-butler
  ;; global mode interferes with magit
  :hook (prog-mode . ws-butler-mode))


(load-package expand-region
  :config
  ;; It interferes angel.el's region transient map specifically, the
  ;; region-deactive-hook doesn't run right after the region highlight
  ;; is off.
  (setq expand-region-fast-keys-enabled nil)
  :commands
  er/expand-region
  er/contract-region)


(load-package isolate
  :commands
  isolate-quick-add
  isolate-quick-change
  isolate-quick-delete
  isolate-long-add
  isolate-long-change
  isolate-long-delete)


(load-package diff-hl
  :hook (prog-mode-hook . diff-hl-mode)
  :config (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(load-package separedit
  :commands separedit
  :config
  (setq separedit-default-mode 'text-mode
        separedit-remove-trailing-spaces-in-comment t))


(load-package yasnippet
  :config
  (yas-global-mode)
  (yas--define-parents 'minibuffer-inactive-mode '(emacs-lisp-mode))
  (with-eval-after-load 'hippie-exp
    (add-to-list 'hippie-expand-try-functions-list #'yas-expand)))


(load-package color-outline
  :hook (prog-mode-hook . color-outline-mode))


(load-package visual-regexp
  :commands
  vr/replace
  vr/query-replace
  vr/mc-mark)


(load-package ghelp)


(load-package hideshow
  :hook (prog-mode-hook . hs-minor-mode))


(load-package helpful)


(load-package iscroll
  :commands iscroll-mode)


(load-package tramp
  :defer
  :init
  (setq tramp-default-method "ssh")
  ;; `tramp-backup-directory-alist' is for remote backup on remote
  ;; machines, so it is not useful. The following is the real way:
  ;; https://stackoverflow.com/questions/3893727/setting-emacs-tramp-to-store-local-backups
  (let ((backup-dir (expand-file-name
                     "var/tramp/backup" user-emacs-directory)))
    (unless (file-exists-p backup-dir)
      (mkdir backup-dir t))
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp backup-dir))))


(load-package ftable
  :commands
  ftable-fill
  ftable-edit-cell
  ftable-reformat)


(defun company-complete-common-or-commit ()
  "Insert the common part of all candidates, or commit the selection."
  (interactive)
  (when (company-manual-begin)
    (let ((tick (buffer-chars-modified-tick)))
      (call-interactively 'company-complete-common)
      (when (eq tick (buffer-chars-modified-tick))
        (call-interactively 'company-complete-selection)))))

(luna-def-key
 :keymaps '(company-active-map company-search-map)
 "C-p" #'company-select-previous
 "C-n" #'company-select-next
 "RET" nil
 "<return>" nil
 "="   #'company-complete-selection
 :keymaps 'company-search-map
 "<escape>" #'company-abort)

(load-package company
  :hook (prog-mode-hook . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-tooltip-limit 15)
  ;; company dabbrev is annoying, make sure not to include it.
  (setq-default company-backends
                '(company-capf company-files company-dabbrev-code))
  (setq-default company-search-filtering t))


(load-package rime
  :defer
  :config
  (luna-on "Brown"
    (setq rime-show-candidate 'posframe
          rime-posframe-style 'vertical
          rime-user-data-dir "/Users/yuan/Library/Rime"
          rime-show-preedit 'inline
          rime-posframe-properties (list :font "Source Han Sans"
                                         :weight 'light
                                         :internal-border-width 10))
    (add-hook 'input-method-activate-hook
              (lambda () (interactive)
                (setq-local cursor-type 'hbar)))
    (add-hook 'input-method-inactivate-hook
              (lambda () (interactive)
                (kill-local-variable 'cursor-type)))))


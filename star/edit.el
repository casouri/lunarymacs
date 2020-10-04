;; -*- lexical-binding: t -*-

(require 'luna-f)
(require 'outline+)

;;; Key

(luna-def-key
 :leader
 "hh" #'hs-hide-all
 "hs" #'hs-show-all
 ;; utility.el
 "hm" #'helpme
 "ff" #'finder-toggle
 :---
 "C-h C-h" #'ghelp-describe
 "C-h r"   #'ghelp-resume
 "C-h o"   #'ghelp-describe-elisp
 "C-h f"   #'ghelp-describe-function
 "C-h v"   #'ghelp-describe-variable
 "C-h k"   #'ghelp-describe-key
 "C-x i"   #'luna-insert-special-symbol
 "C-c '"   #'separedit
 "C-s-i"   #'color-outline-toggle-all
 "C-/"     #'undo-only
 "C-."     #'undo-redo
 "C-s-i"   #'outline-cycle-buffer
 "C-c C-h" #'hs-toggle-hiding
 "C-="     #'expand-region
 "C--"     #'contract-region
 :keymaps 'override
 "C-j"     #'avy-goto-word-1
 :keymaps '(c-mode-map c++-mode-map)
 "M-RET" #'srefactor-refactor-at-point
 :keymaps '(outline-minor-mode-map org-mode-map outline-mode-map)
 "s-i" #'outline-cycle)

;;; Config

;; (which-function-mode)

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;;; Package

(load-package avy
  :commands avy-goto-word-1)


(load-package ws-butler
  ;; global mode interferes with magit
  :hook (prog-mode . ws-butler-mode))


(load-package expand-region
  :config
  ;; it interferes angel.el's region transient map
  ;; specifically, the region-deactive-hook
  ;; doesn't run right after the region highlight is off
  (setq expand-region-fast-keys-enabled nil)
  (require 'expand-region-hacks)
  :commands
  expand-region
  contract-region
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


(load-package srefactor
  :hook ((c-mode-hook c++-mode-hook) . semantic-mode))


(load-package ghelp)


(add-hook 'prog-mode-hook #'hs-minor-mode)


(load-package helpful)


(load-package tramp
  :defer
  :config
  (setq tramp-default-method "ssh")
  ;; Save tramp backups locally.
  (setq tramp-backup-directory-alist backup-directory-alist))

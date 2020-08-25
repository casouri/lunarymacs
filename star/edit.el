;; -*- lexical-binding: t -*-

(require 'luna-f)
(require 'outline+)

;;; Key

(luna-def-key
 :leader
 "sr" #'grep
 "hha" #'hs-hide-all
 "hsa" #'hs-show-all
 :---
 "C-h C-h" #'ghelp-describe
 "C-h r"   #'ghelp-resume
 "C-h k" #'ghelp-helpful-key
 "C-h e" #'ghelp-describe-elisp
 "C-x i" #'luna-insert-special-symbol
 "C-c '" #'separedit
 "C-s-i" #'color-outline-toggle-all
 "C-/"   #'undo-only
 "C-."   #'undo-redo
 "C-s-i" #'outline-cycle-buffer
 "C-c C-h" #'hs-toggle-hiding
 :keymaps '(c-mode-map c++-mode-map)
 "M-RET" #'srefactor-refactor-at-point
 :keymaps '(outline-minor-mode-map org-mode-map outline-mode-map)
 "s-i" #'outline-cycle
 :keymaps 'text-mode-map
 "TAB" #'self-insert-command)

;;; Config

;; (which-function-mode)

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;;; Package

(load-package ws-butler
  ;; global mode interferes with magit
  :hook (prog-mode . ws-butler-mode))


(load-package expand-region
  :config
  ;; it interferes angel.el's region transient map
  ;; specifically, the region-deactive-hook
  ;; doesn't run right after the region highlight is off
  (setq expand-region-fast-keys-enabled nil)
  :commands
  er/expand-region
  er/mark-defun
  er/mark-word
  er/mark-symbol
  er/mark-inside-quotes
  er/mark-outside-quotes
  er/mark-inside-pairs
  er/mark-outside-pairs
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

(add-to-list 'luna-package-list 'helpful)

;; -*- lexical-binding: t -*-
;;
;; Completion.

;;; Key

(luna-key-def
 "s-/"     #'transform-previous-char
 :keymaps 'corfu-map
 "=" #'corfu-insert
 "RET" #'corfu-quit-and-newline)

;;; Package

(load-package transform
  :commands transform-previous-char)

(load-package yasnippet
  :config
  (yas-global-mode)
  (yas--define-parents 'minibuffer-mode '(emacs-lisp-mode))
  (yas--define-parents 'minibuffer-inactive-mode '(emacs-lisp-mode))
  (with-eval-after-load 'hippie-exp
    (add-to-list 'hippie-expand-try-functions-list #'yas-expand)))

(load-package corfu
  :autoload-hook (prog-mode-hook . corfu-mode)
  :config
  ;; (setq completion-styles '(basic))
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2)
  (defun corfu-quit-and-newline ()
    "Quit corfu and insert newline."
    (interactive)
    (corfu-quit)
    (newline)))

(load-package consult
  :config
  (setq consult-preview-key nil)
  (consult-customize
   consult-line :preview-key 'any))

(load-package recentf-ext
  :config (recentf-mode))

(load-package selectrum
  :config
  (selectrum-mode)
  (defun advice-selectrum--selection-highlight (oldfn str)
    "Advice for ‘selectrum--selection-highlight’.
Don’t append face, override the faec."
    (propertize (copy-sequence str) 'face 'selectrum-current-candidate))
  (advice-add #'selectrum--selection-highlight :around
              #'advice-selectrum--selection-highlight))

(load-package selectrum-prescient
  :config
  (selectrum-prescient-mode))

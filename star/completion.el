;; -*- lexical-binding: t -*-
;;
;; Completion.

;;; Key

(luna-key-def
 "s-/"     #'transform-previous-char
 :keymaps 'corfu-map
 "=" #'corfu-insert
 "RET" #'corfu-quit-and-newline
 "C-a" #'corfu-c-a
 "C-e" nil
 [remap move-beginning-of-line] nil
 [remap move-end-of-line] nil)

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
        corfu-auto-delay 0
        corfu-auto-prefix 2)

  (defun corfu-quit-and-newline ()
    "Quit corfu and insert newline."
    (interactive)
    (corfu-quit)
    (newline 1 t))

  (defun corfu-c-a (&rest _)
    "Go to beginning of completion and quit corfu."
    (interactive)
    (corfu-prompt-beginning 1)
    (corfu-quit)))

(load-package recentf-ext
  :config
  (recentf-mode)
  (run-with-timer 300 300 #'recentf-save-list))

(load-package vertico
  :config
  (vertico-mode))

(load-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides
        '((file (styles basic partial-completion))
          (email
           (styles orderless substring partial-completion))
          (eglot
           (styles basic))
          (buffer
           (styles orderless basic substring))
          (unicode-name
           (styles orderless basic substring))
          (project-file
           (styles orderless substring))
          (xref-location
           (styles orderless substring))
          (info-menu
           (styles orderless basic substring))
          (symbol-help
           (styles orderless basic shorthand substring)))))

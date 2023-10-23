;; -*- lexical-binding: t -*-
;;
;; Completion.

;;; Key

(luna-key-def
 "s-/"     #'transform-previous-char
 :keymaps '(company-active-map company-search-map)
 "C-p" #'company-select-previous
 "C-n" #'company-select-next
 "C-j" (lambda () (interactive) (company-abort) (next-line 1))
 "RET" (lambda () (interactive) (company-abort) (newline 1 t))
 "<return>" (lambda () (interactive) (company-abort) (newline 1 t))
 "=" #'company-complete-selection
 ;; "<tab>" #'company-complete-common-or-commit
 ;; "<tab>" #'company-select-next
 :keymaps 'company-search-map
 "<escape>" #'company-abort)

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

(load-package company
  :autoload-hook (prog-mode-hook . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-tooltip-limit 15)
  ;; Company dabbrev is annoying, make sure not to include it.
  (setq-default company-backends
                '(company-capf company-files company-dabbrev-code))
  (setq-default company-search-filtering t)

  :init
  (defun company-complete-common-or-commit ()
    "Insert the common part of all candidates, or commit the selection."
    (interactive)
    (when (company-manual-begin)
      (let ((tick (buffer-chars-modified-tick)))
        (call-interactively 'company-complete-common)
        (when (eq tick (buffer-chars-modified-tick))
          (call-interactively 'company-complete-selection))))))

(use-package company-box
  :autoload-hook (company-mode . company-box-mode)
  :config
  (setq company-box-enable-icon nil))

(load-package recentf-ext
  :config (recentf-mode))

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
           (styles flex basic))
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

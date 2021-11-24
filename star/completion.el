;; -*- lexical-binding: t -*-
;;
;; Completion.

;;; Key

(luna-def-key
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
  :hook (prog-mode-hook . company-mode)
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

(load-package consult
  :init
  ;; Bindings are defined at the top.
  (defvar consult-binded-mode-map (make-sparse-keymap)
    "Minor mode map for ‘console-minor-mode’.")

  (define-minor-mode consult-binded-mode
    "Minor mode enabling consult commands."
    :global t
    :keymap consult-binded-mode-map
    :group 'convenience
    (if consult-binded-mode
        (message ":-)")
      (message ";-)")))
  :config
  (consult-binded-mode)
  (setq consult-preview-key (kbd "C-o"))
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

;;; -*- lexical-binding: t -*-

(defvar moon-smart-toggle-lsp-ui t
  "Whether to toggle lsp-ui doc and sideline automatically
depending on window width.")

(defvar moon-smart-toggle-threshold 120
  "If window width is above threshold, keep lsp-ui-doc/sideline on,
if under, turn them off.")

(use-package| lsp-mode
  :defer t
  :init
  (defun setup-lsp ()
    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
    (setq lsp-enable-eldoc nil)))

(after-load| lsp-ui-mode
  (require 'lsp-flycheck))

(use-package| lsp-ui
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  ;; completion
  (setq lsp-enable-completion-at-point t)
  ;; ui
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  ;; smart toggle lsp-ui doc & sideline
  (add-hook 'window-configuration-change-hook
            #'moon/smart-toggle-lsp-ui)
  (add-hook 'lsp-ui-mode-hook
            #'moon/smart-toggle-lsp-ui)
  ;; if manually toggled doc or sideline, disable smart-toggle
  (advice-add 'lsp-ui-doc-mode :after #'moon-force-lsp-ui)
  (advice-add 'lsp-ui-sideline-mode :after #'moon-force-lsp-ui)
  ;; peek color
  (moon/sync-peek-face)
  (add-hook 'moon-load-theme-hook #'moon/sync-peek-face)
  )

(use-package| company-lsp
  :after (company lsp-mode)
  :init  
  (setq company-lsp-async t)
  (add-to-list 'company-backends 'company-lsp)
  )


(post-config| general
  (after-load| lsp-mode
    (after-load| lsp-ui
      (default-leader
        "lr" #'lsp-ui-peek-find-references
        "ld" #'lsp-ui-peek-find-definitions
        "lR" #'lsp-rename
        "lf" #'lsp-format-buffer
        ))))

;;; -*- lexical-binding: t -*-

(defvar moon-smart-toggle-lsp-ui t
  "Whether to toggle lsp-ui doc and sideline automatically
depending on window width.")


(use-package| lsp-mode
  :defer 3
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck))
  (setq lsp-enable-eldoc nil)
  )

(after-load| lsp-mode
  (require 'lsp-flycheck))

(use-package| lsp-ui
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (default-leader
    "jr" #'lsp-ui-peek-find-references
    "jd" #'lsp-ui-peek-find-definitions
    "jD" #'xref-find-definition
    "jR" #'xref-find-references
    )
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
  (advice-add 'lsp-ui-doc-enable :after (lambda () (moon/smart-toggle-lsp-ui nil)))
  (advice-add 'lsp-ui-sideline-enable :after (lambda () (moon/smart-toggle-lsp-ui nil)))
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


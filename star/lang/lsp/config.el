;;; -*- lexical-binding: t -*-

(use-package| lsp-mode
  :delight (lsp-mode " Ⓛ")
  :defer 3
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck))
  (setq lsp-enable-eldoc nil)
  )

(after| lsp-mode
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
  (add-hook 'window-configuration-change-hook
            #'moon/smart-toggle-lsp-ui)
  (add-hook 'lsp-ui-mode-hook
            #'moon/smart-toggle-lsp-ui)
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

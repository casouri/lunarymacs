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
  (moon/sync-peek-face)
  (default-leader
    "jr" #'lsp-ui-peek-find-references
    "jd" #'lsp-ui-peek-find-definitions
    "jD" #'xref-find-definition
    "jR" #'xref-find-references
    )
  (setq lsp-ui-sideline-enable t)
  (setq lsp-enable-completion-at-point t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-enable t)
  )

(use-package| company-lsp
  :after (company lsp-mode)
  :init  
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  (add-to-list 'company-backends 'company-lsp)
  )

;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar lsp-ui-sideline-enable t
  "whether enable sideline")
(defvar lsp-enable-completion-at-point t
  "whether enable complition at point")
(defvar lsp-ui-doc-position 'at-point
  "specify doc position")
(defvar lsp-ui-doc-header t
  "whether enable doc header")
(defvar lsp-ui-doc-include-signature t
  "whether include signature")

(use-package| lsp-mode
  :delight (lsp-mode " Ⓛ")
  ;; :init (add-hook 'moon-delay-init-hook (lambda () (require 'lsp-mode)))
  :init (make-thread (lambda () (require 'lsp-mode)) "lsp-mode")
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck))
  )

(use-package| lsp-ui
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (moon/sync-peek-face)
  (default-leader
    "jr" #'lsp-ui-peek-find-references
    "jd" #'lsp-ui-peek-find-definitions
    )
  )

(use-package| company-lsp
  :after (company lsp-mode)
  :init  
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  (add-to-list 'company-backends 'company-lsp)
  )

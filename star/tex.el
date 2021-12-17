;; lexical-binding: t; -*-

;;; Keys

(luna-def-key
 :keymaps 'TeX-mode-map
 "$" nil
 :keymaps 'latex-mode-map
 "C-c p" #'latex-preview-update)

;;; Packages

;; (load-package cdlatex
;;   :autoload-hook (LaTeX-mode-hook . cdlatex-mode))

(load-package eglot
  :defer
  :config
  (add-to-list 'eglot-server-programs '(latex-mode . ("digestif"))))

;; This installs AucTex and messes up the hook, also it’s not all that
;; good.
;;
;; (load-package xenops
;;   :autoload-hook (latex-mode-hook . xenops-mode))

;;; Config

(defun tex-mode-setup ()
  ;; (prettify-symbols-mode)
  ;; (variable-pitch-mode)
  ;; (olivetti-mode)
  ;; (olivetti-set-width 90)
  (company-mode)
  (outline-minor-mode)
  (eglot-ensure))

(add-hook 'latex-mode-hook #'tex-mode-setup)

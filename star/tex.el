;; lexical-binding: t; -*-

;;; Keys

(luna-def-key
 :keymaps 'TeX-mode-map
 "$" nil)

;;; Packages

(add-to-list 'luna-package-list 'auctex)

;; (load-package cdlatex
;;   :hook (LaTeX-mode-hook . cdlatex-mode))

(add-to-list 'luna-package-list 'eglot)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(latex-mode . ("digestif"))))

;;; Config

(defun tex-mode-setup ()
  (prettify-symbols-mode)
  ;; (variable-pitch-mode)
  (olivetti-mode)
  (olivetti-set-width 90)
  (company-mode)
  (eglot-ensure))

;; This is AuCTex hooks and library.
;; The built-in ones are tex-mode and tex-mode-hook.
(with-eval-after-load 'latex
  (add-hook 'LaTeX-mode-hook #'tex-mode-setup))

(with-eval-after-load 'tex
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))

;; lexical-binding: t; -*-

;;; Packages

(add-to-list 'luna-package-list 'auctex)

(load-package cdlatex
  :hook (LaTeX-mode-hook . cdlatex-mode))

(load-package eglot
  :hook (tex-mode-hook . eglot-ensure)
  :init (add-to-list 'eglot-server-programs
                     '(latex-mode . ("digestif"))))

;;; Config

(defun tex-mode-setup ()
  (prettify-symbols-mode)
  ;; (variable-pitch-mode)
  (olivetti-mode)
  (olivetti-set-width 90))

(with-eval-after-load 'tex-mode
  (add-hook 'tex-mode-hook #'tex-mode-setup))

(with-eval-after-load 'tex
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))

;; (with-eval-after-load 'luna-general-config
;;   (luna-default-leader
;;     :keymaps 'tex-mode-map
;;     "tp" #'latex-preview-pane-mode))

;; (load-package latex-preview-pane
;;   :commands (latex-preview-pane-mode))

;; (load-package company-math
;;   :commands (luna-latex-company-setup)
;;   :init
;;   (add-hook
;;    'LaTeX-mode-hook
;;    (lambda ()
;;      (require 'company-math)
;;      (setq-local company-backends
;;                  (append '((company-math-symbols-latex
;;                             company-latex-commands))
;;                          company-backends)))))

;; (load-package webkit-katex-render
;;   :commands webkit-katex-render-mode)

;; (load-package auctex
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :init
;;   (setq TeX-parse-self t)
;;   (setq-default TeX-master nil)
;;   (add-hook 'latex-mode-hook #'luna-require-auctex)
;;   ;; (setq TeX-electric-math (cons "$" "$")) ; too slow
;;   ;; (setq TeX-command-default "XeLaTeX")
;;   ;; (setq TeX-show-compilation t)
;;   (setq TeX-save-query nil)
;;   (add-hook 'LaTeX-mode-hook
;;             (lambda ()
;;               (add-to-list 'TeX-command-list
;;                            '("XeLaTeX" "%`xelatex%(mode)%' %t"
;;                              TeX-run-TeX nil t))
;;               ;; tex-insert-dollar is useless and slow
;;               (define-key TeX-mode-map (kbd "$") nil))))


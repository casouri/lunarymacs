;;; tex.el --- Tex      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;
;;;; Key

(luna-with-eval-after-load 'key.general
  (luna-default-leader
    :keymaps 'tex-mode-map
    "tp" #'latex-preview-pane-mode))

;;;; Packages

(load-package latex-preview-pane
  :commands (latex-preview-pane-mode))

(load-package company-math
  :commands (luna-latex-company-setup)
  :init
  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (require 'company-math)
     (setq-local company-backends
                 (append '((company-math-symbols-latex company-latex-commands))
                         company-backends)))))

(load-package cdlatex
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'cdlatex-mode))

;; (load-package webkit-katex-render
;;   :commands webkit-katex-render-mode)

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '(latex-mode . ("digestif"))))

(load-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'latex-mode-hook #'luna-require-auctex)
  ;; (setq TeX-electric-math (cons "$" "$")) ; too slow
  ;; (setq TeX-command-default "XeLaTeX")
  ;; (setq TeX-show-compilation t)
  (setq TeX-save-query nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
              ;; tex-insert-dollar is useless and slow
              (define-key TeX-mode-map (kbd "$") nil))))

;; (defun luna-require-auctex ()
;;   "Require necessary files from auctex."
;;   (require 'tex-site)
;;   (require 'latex)
;;   (require 'font-latex)
;;   (require 'texmathp)
;;   (require 'preview)
;;   (TeX-latex-mode))

;;;; Config








;;; config.el ends here

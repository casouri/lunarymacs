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
  :commands latex-preview-pane-mode)

(load-package company-math
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'luna-latex-company-setup))

(defun luna-latex-company-setup ()
  (require 'company-math)
  (message "woome")
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))

;; (load-package webkit-katex-render
;;   :commands webkit-katex-render-mode)

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '(latex-mode . ("digestif"))))

(load-package auctex
  :defer t
  :init
  ;; (setq TeX-electric-math (cons "$" "$")) ; too slow
  (add-hook 'latex-mode-hook
            (lambda ()
              (luna-require-auctex)
              (TeX-latex-mode)
              (electric-quote-local-mode -1))))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            ;; (setq TeX-command-default "XeLaTeX")
            (setq TeX-save-query nil)
            ;; (setq TeX-show-compilation t)
            ;; tex-insert-dollar is useless and slow
            (define-key TeX-mode-map (kbd "$") nil)))

(defun luna-require-auctex ()
  "Require necessary files from auctex."
  (require 'tex-site)
  (require 'latex)
  (require 'font-latex)
  (require 'texmathp)
  (require 'preview))

;;;; Function
;;
;; auto {} after \

;; (defun luna-tex-post-insert (&rest _)
;;   "Insert {} after \\."
;;   (interactive)
;;   (when (eq (char-before) ?\\)
;;     (insert "{}")
;;     (backward-char 2)))

;; (add-hook 'latex-mode-hook (lambda ()
;;                              (add-hook 'post-self-insert-hook #'luna-tex-post-insert nil t)))

;;; config.el ends here

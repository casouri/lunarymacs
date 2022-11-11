;; -*- lexical-binding: t -*-

;;; Packages

(add-to-list 'load-path
             (expand-file-name "site-lisp"
                               user-emacs-directory))
(require 'lunary)
(require 'cowboy)
;; recipe.el is needed for adding subdir’s to load-path
(luna-load-relative "star/recipe.el")
(package-initialize)
(cowboy-add-load-path)
(setq luna-dumped-load-path load-path
      luna-dumped t)

(dolist (package '(org
                   helpful company which-key
                   recentf-ext aggressive-indent
                   winner elec-pair rainbow-delimiters
                   highlight-parentheses hl-todo buffer-move
                   savehist minions ws-butler
                   flyspell eglot
                   yasnippet which-func savehist
                   consult selectrum selectrum-prescient
                   no-littering dwim-shell-command dwim-shell-commands
                   ;; site-lisp
                   transform utility pause color-outline
                   ;; ranch
                   ghelp
                   ;; lib
                   subr-x cl-lib seq pcase svg))
  (require package))

;;; Env

(require 'exec-path-from-shell)
(setq luna-env-vars (exec-path-from-shell-getenvs
                     exec-path-from-shell-variables))

;;; Theme

(add-to-list 'custom-theme-load-path
             (expand-file-name "site-lisp" user-emacs-directory))
(load-theme 'light t t)
(load-theme 'pale t t)
(load-theme 'cyberpunk t t)

;;; Other files

(load "kinsoku.el")

;;; Dump

(message "Dumping to %s" luna-dump-file)
(dump-emacs-portable luna-dump-file)

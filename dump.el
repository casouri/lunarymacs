;; -*- lexical-binding: t -*-

;;; Packages

(add-to-list 'load-path
             (expand-file-name "site-lisp"
                               user-emacs-directory))
(require 'luna-f)
(require 'lunary)
(require 'cowboy)
;; recipe.el is needed for adding subdir’s to load-path
(luna-load-relative "star/recipe.el")
(package-initialize)
(cowboy-add-load-path)
(setq luna-dumped-load-path load-path
      luna-dumped t)

(dolist (package '(ivy counsel org helpful company which-key
                       recentf-ext swiper aggressive-indent
                       winner elec-pair rainbow-delimiters
                       highlight-parentheses hl-todo buffer-move
                       savehist minions ws-butler
                       expand-region flyspell flycheck eglot
                       yasnippet ivy-xref which-func savehist
                       ;; site-lisp
                       transform utility pause color-outline
                       outline+
                       ;; ranch
                       ghelp
                       ;; lib
                       subr-x cl-lib seq pcase))
  (require package))

(require 'no-littering)

;;; Theme

(add-to-list 'custom-theme-load-path
             (expand-file-name "site-lisp" user-emacs-directory))
(load-theme 'light t t)
(load-theme 'cyberpunk t t)

;;; Other files

(load "kinsoku.el")

;;; Dump

(message "Dumping to %s" luna-dump-file)
(dump-emacs-portable luna-dump-file)

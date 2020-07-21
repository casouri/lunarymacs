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
(setq package-user-dir (expand-file-name "package" user-emacs-directory))
(package-initialize)
(cowboy-add-load-path)
(setq luna-dumped-load-path load-path
      luna-dumped t)

(dolist (package '(ivy counsel org helpful company general
                       which-key recentf-ext swiper
                       aggressive-indent winner elec-pair
                       rainbow-delimiters highlight-parentheses
                       hl-todo buffer-move savehist eyebrowse
                       minions ws-butler expand-region flyspell
                       flycheck eglot minibuf-eldef yasnippet
                       wucuo ivy-xref dired-x subr-x pcase cl-lib
                       seq olivetti org-download htmlize
                       ;; site-lisp
                       transform utility pause info+ color-outline
                       commentary quanjiao outline+ luna-publish
                       ox-cjk-html sidebar
                       ;; ranch
                       valign ghelp isolate
                       ;; built-in
                       pcase cl-lib which-func savehist winner elec-pair))
  (require package))

;;; Theme

(add-to-list 'custom-theme-load-path
             (expand-file-name "site-lisp" user-emacs-directory))
(load-theme 'light t t)
(load-theme 'cyberpunk t t)

;;; Other files

(load "kinsoku.el")
(luna-load-relative "star/org/blogs.el")

;;; Env

(message "Setting ENV")
(luna-set-env)

;;; Dump

(setq luna-dump-file "/Applications/Emacs.app/Contents/MacOS/Emacs.pdmp")
(message "Dumping to %s" luna-dump-file)
(dump-emacs-portable luna-dump-file)

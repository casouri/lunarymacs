;; -*- lexical-binding: t -*-
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
(dolist (package '(use-package company ivy counsel org helpful
                    general helpful use-package general which-key
                    recentf-ext swiper ivy-prescient
                    aggressive-indent winner elec-pair
                    rainbow-delimiters
                    highlight-parentheses hl-todo buffer-move
                    treemacs savehist eyebrowse minions ws-butler
                    expand-region isolate flyspell
                    flycheck eglot minibuf-eldef ;; sly-el-indent
                    yasnippet flywrap transform utility pause))
  (require package))
(add-to-list 'custom-theme-load-path
             (expand-file-name "site-lisp" user-emacs-directory))
(load-theme 'light t t)
(load-theme 'cyberpunk t t)

(setq luna-dump-file "/Applications/Emacs.app/Contents/MacOS/Emacs.pdmp")
(message "Dumping to %s" luna-dump-file)
(dump-emacs-portable luna-dump-file)

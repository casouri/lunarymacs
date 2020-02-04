;;-*- lexical-binding: t -*-

(add-to-list 'load-path
             (expand-file-name "site-lisp"
                               user-emacs-directory))
(require 'luna-f)
(require 'lunary)
(require 'cowboy)
(setq package-user-dir (expand-file-name "package" user-emacs-directory))
(package-initialize)
(cowboy-add-load-path)
(require 'doom-themes)
(load-theme 'doom-one-light t)
(luna-load-relative "star/org/blogs.el")
(require 'htmlize)
(setq auto-save-default nil)

(toggle-debug-on-error)

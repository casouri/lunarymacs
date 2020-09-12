;;-*- lexical-binding: t -*-

(add-to-list 'load-path
             (expand-file-name "site-lisp"
                               user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "site-lisp"
                               user-emacs-directory))
(require 'luna-f)
(require 'lunary)
(require 'cowboy)
(package-initialize)
(cowboy-add-load-path)
(require 'light-theme)
(load-theme 'light t)
(luna-load-relative "star/org/blogs.el")
(require 'htmlize)
(setq auto-save-default nil
      make-backup-files nil)

(toggle-debug-on-error)

;;-*- lexical-binding: t -*-

(when (eq window-system 'mac)
  ;; have to enable menu bar on mac port
  ;; otherwise emacs lost focus
  (menu-bar-mode))

;;; Package

(add-to-list 'load-path
             (expand-file-name "site-lisp"
                               user-emacs-directory))
(require 'luna-f)
(require 'lunary)
(require 'cowboy)
(require 'package)
(require 'luna-local)

(luna-if-dump
    (progn
      (setq load-path luna-dumped-load-path)
      (global-font-lock-mode)
      (transient-mark-mode)
      (add-hook 'after-init-hook
                (lambda ()
                  (save-excursion
                    (switch-to-buffer "*scratch*")
                    (goto-char (point-min))
                    (insert ";; \t\t\tE M A C S\n")
                    (insert (format ";; %s\n" (make-string 55 ?-)))
                    (insert (format ";; Welcome to GNU Emacs %s. " emacs-version))
                    (insert (format "Today is %s.\n" (format-time-string "%A %Y.%-m.%-d")))
                    (insert ";;\n")
                    (lisp-interaction-mode)))))
  (setq package-user-dir (expand-file-name
                          "package" user-emacs-directory))
  ;; add load-path’s and load autoload files
  (package-initialize)
  (cowboy-add-load-path))

;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
;; (benchmark-init/activate)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq luna-company-manual nil)
(add-to-list 'luna-package-list 'use-package)

(luna-message-error (require 'use-package))
;; core must load first because other configs depends on them
(luna-load-relative "star/builtin-config.el")
(luna-load-relative "star/key.el")
(luna-load-relative "star/recipe.el")
(luna-load-relative "star/angel.el")
(luna-load-relative "star/ui.el")
(luna-load-relative "star/mode-line.el")
(luna-load-relative "star/edit.el")
(luna-load-relative "star/homepage.el")
;; (luna-load-relative "star/helm.el")
(luna-load-relative "star/ivy.el")
(luna-load-relative "star/checker.el")
(luna-load-relative "star/company.el")
(luna-load-relative "star/eglot.el")
;; (luna-load-relative "star/lsp.el")
(luna-load-relative "star/python.el")
(luna-load-relative "star/elisp.el")
(luna-load-relative "star/git.el")
(luna-load-relative "star/dir.el")
(luna-load-relative "star/org.el")
(luna-load-relative "star/tex.el")
;; (luna-load-relative "star/shell.el")
(luna-load-relative "star/simple-mode.el")
(luna-load-relative "star/hacks.el")
(require 'utility)

;;; Customize

;;;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(luna-load-or-create custom-file)
(luna-local-load)

;;;; Misc
(setq-default luna-format-on-save t)
(setq-default bidi-display-reordering nil) ;; faster long line
(setq scroll-margin 4)
(setq ispell-program-name "aspell")
(setq user-full-name "Yuan Fu"
      user-mail-address "casouri@gmail.com")
(setq split-height-threshold nil ; Popup window to right
      split-width-threshold 80)

;;;; theme
(when window-system
  (luna-load-theme))

;;;; Font
(when window-system
  (luna-load-font)
  (luna-load-cjk-font))
(setq luna-cjk-font-scale 1.1)
(luna-when-mac (luna-enable-apple-emoji))

;;;; server
;; checking whether server started can be slow
;; see emacs-horror
(unless luna-in-esup
  (run-with-idle-timer
   3 nil
   (lambda () (ignore-errors (server-start)))))

;;;; ghelp
(add-to-list 'load-path "~/p/ghelp")
(require 'ghelp)

;;;; vterm
;; (luna-load-relative "star/term.el")

(luna-when-mac
 ;; macports
 (add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
;;;; Mac port
 (setq mac-option-modifier 'meta
       mac-command-modifier 'super
       mac-pass-command-to-system nil ; fix cmd h
       mac-system-move-file-to-trash-use-finder t)

 (global-set-key (kbd "s-c") #'kill-ring-save)
 (global-set-key (kbd "s-v") #'yank)

;;;; ENV
 (luna-load-env)

;;;; trivial-copy
 (luna-when-mac
  (add-to-list 'load-path "~/p/trivial-copy")
  (require 'trivial-copy)))

;;;; Local unsynced customization

(luna-load-or-create (luna-f-join user-emacs-directory
                                  "local-config.el"))

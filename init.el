;;-*- lexical-binding: t -*-

;;; Init

(add-to-list 'load-path
             (expand-file-name "site-lisp"
                               user-emacs-directory))
(require 'luna-f)
(require 'lunary)
(require 'cowboy)
(require 'package)
(require 'luna-local)

;;; Custom & local

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(luna-safe-load custom-file)
(luna-local-load)

;;; Dump

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
                    (insert (format "Today is %s.\n" (format-time-string
                                                      "%A %Y.%-m.%-d")))
                    (insert ";;\n")
                    (lisp-interaction-mode)))))
  (setq package-user-dir (expand-file-name
                          "package" user-emacs-directory))
  ;; add load-path’s and load autoload files
  (luna-load-relative "star/recipe.el")
  (package-initialize)
  (cowboy-add-load-path))

;;; Benchmark

;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
;; (benchmark-init/activate)

;;; Configs

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq luna-company-manual nil)

;; core must load first because other configs depends on them
(luna-load-relative "star/builtin-config.el")
(luna-load-relative "star/key.el")
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
(luna-load-relative "star/org/blogs.el")
(luna-load-relative "star/tex.el")
;; (luna-load-relative "star/shell.el")
(luna-load-relative "star/simple-mode.el")
(require 'utility)

;;; Customize

;;;; Misc
(setq-default luna-format-on-save t)
(luna-on "Brown"
  (setq luna-dump-file
        "/Applications/Emacs.app/Contents/MacOS/Emacs.pdmp"
        luna-dump-emacs-file
        "/Applications/Emacs.app/Contents/MacOS/Emacs"))

;;;; theme
(when window-system
  (luna-load-theme))

;;;; Font
(when window-system
  (luna-load-font)
  (luna-load-cjk-font))
(luna-when-mac (luna-enable-apple-emoji))

;;;; server
;; checking whether server started can be slow
;; see emacs-horror
(run-with-idle-timer 3 nil (lambda () (server-start t t)))

;;;; vterm
;; (luna-load-relative "star/term.el")

;; Mac specific config starts here
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

 ;; Because Apple.
 (when (equal default-directory "/") (cd "~/")))

;; Mac specific config ends here

;;;; Local unsynced customization

(luna-safe-load (luna-f-join user-emacs-directory "local-config.el"))


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
                    (insert (format ";; %s\n" (make-string 58 ?-)))
                    (insert (format ";; Welcome to GNU Emacs %s. "
                                    emacs-version))
                    (insert (format "Today is %s.\n"
                                    (format-time-string "%A %Y.%-m.%-d")))
                    (insert ";;\n")
                    (lisp-interaction-mode)))))
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

(luna-key-def-preset :leader
  :keymaps 'override
  :prefix (if (display-graphic-p) "C-SPC" "C-@"))

(luna-key-def-preset :leader-prefix
  :prefix (if (display-graphic-p) "C-SPC" "C-@"))

;; core must load first because other configs depends on them
(luna-load-relative "star/etc.el")
(luna-load-relative "star/key.el")
(luna-load-relative "star/angel.el")
(luna-load-relative "star/ui.el")
(luna-load-relative "star/mode-line.el")
(luna-load-relative "star/edit.el")
(luna-load-relative "star/ivy.el")
(luna-load-relative "star/checker.el")
(luna-load-relative "star/company.el")
(luna-load-relative "star/eglot.el")
(luna-load-relative "star/python.el")
(luna-load-relative "star/git.el")
(luna-load-relative "star/dir.el")
(luna-load-relative "star/org.el")
(luna-load-relative "star/org/blogs.el")
(luna-load-relative "star/writing.el")
(luna-load-relative "star/tex.el")
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
(luna-load-theme)

;;;; Font
(when (display-graphic-p)
  (luna-load-font)
  (luna-load-cjk-font))
(luna-on "Brown" (luna-enable-apple-emoji))

;;;; server
(run-with-idle-timer
 3 nil (lambda ()
         (require 'server)
         (unless (eq (server-running-p) t)
           (server-start t t))))

;;;; vterm
;; (luna-load-relative "star/term.el")

;;;; Macports
(luna-on "Brown"
  ;; macports
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp"))

;;;; Emacs Mac port
(luna-on "Brown"
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-pass-command-to-system nil ; fix cmd h
        mac-system-move-file-to-trash-use-finder t)

  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-v") #'yank))

;;;; ENV
(luna-on "Brown"
  (luna-load-env)
  ;; Because Apple.
  (when (equal default-directory "/") (cd "~/")))

;;;; Smooth scrolling
(luna-on "Brown"
  (setq scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01
        scroll-margin 0
        scroll-conservatively 101))

;;;; Trash
(luna-on "Brown"
  (setq trash-directory "~/.Trash"))

;;;; Bidi
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;;;; Scroll

(require 'iscroll)
(iscroll-mode)
(setq iscroll-preserve-screen-position t)

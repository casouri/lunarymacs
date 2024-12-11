;;-*- lexical-binding: t -*-

;;; Init

(push (expand-file-name "site-lisp" user-emacs-directory) load-path)
(require 'lunary)
(require 'cowboy)
(require 'package)

;;; Custom file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(luna-safe-load custom-file)

;;; Dump

(luna-if-dump
    (progn
      (setq load-path luna-dumped-load-path)
      (global-font-lock-mode)
      (transient-mark-mode)
      ;; Re-run mode hooks.
      (add-hook 'after-init-hook
                (lambda ()
                  (switch-to-buffer "*scratch*")
                  (lisp-interaction-mode))))
  ;; Add load-paths and load autoload files.
  (luna-load-relative "star/recipe.el")
  (package-initialize)
  (cowboy-add-load-path))

;;; Benchmark

;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
;; (benchmark-init/activate)

;;; Configs

(luna-key-def-preset :leader
  :keymaps 'override
  :prefix (if (display-graphic-p) "C-SPC" "C-@"))

(luna-key-def-preset :leader-prefix
  :prefix (if (display-graphic-p) "C-SPC" "C-@"))

(luna-load-relative "star/etc.el")
(luna-load-relative "star/key.el")
(luna-load-relative "star/angel.el")
(luna-load-relative "star/ui.el")
(luna-load-relative "star/mode-line.el")
(luna-load-relative "star/edit.el")
(luna-load-relative "star/completion.el")
(luna-load-relative "star/checker.el")
(luna-load-relative "star/python.el")
(luna-load-relative "star/git.el")
(luna-load-relative "star/dir.el")
(luna-load-relative "star/org-mode.el")
(luna-load-relative "star/writing.el")
(luna-load-relative "star/tex.el")
(luna-load-relative "star/simple-mode.el")
(luna-load-relative "star/blog.el")
(luna-load-relative "star/app.el")
(require 'utility)
(setq initial-buffer-choice #'luna-splash-screen)

;;; Customize
;;;; Server
(luna-run-server)
;;;; Theme
(when (window-system)
  (luna-load-theme 'pale))

(luna-on "Brown"
  (luna-safe-load (expand-file-name "brown-init.el" user-emacs-directory)))

;;; Mac
(luna-when-mac
 (setq mac-option-modifier 'meta
       mac-command-modifier 'super
       mac-pass-command-to-system nil ; fix cmd h
       mac-system-move-file-to-trash-use-finder t)
 (global-set-key (kbd "s-c") #'kill-ring-save)
 (global-set-key (kbd "s-v") #'yank)
 ;; I sometimes hit this by accident.
 (global-set-key (kbd "s-H") #'ignore)

 (setq scroll-up-aggressively 0.01
       scroll-down-aggressively 0.01
       redisplay-skip-fontification-on-input t)
 (setq mouse-wheel-flip-direction t))

;;; Linux

(luna-on "blanche"
  (setq source-directory (expand-file-name "~/emacs"))
  (setq x-super-keysym 'ctrl)
  (setq x-ctrl-keysym 'super))

;;; Local init

(let ((local-init (expand-file-name
                   "local-init.el" user-emacs-directory)))
  (when (file-exists-p local-init)
    (luna-safe-load local-init)))

;;; Emacs devel

(defun run-treesit-tests (build)
  "Run tree-sitter tests for BUILD."
  (interactive (list (completing-read "Build: " '("emacs" "emacs-head") nil t)))
  (let ((test-file
         (expand-file-name "test/src/treesit-tests.el"
                           (expand-file-name build "~"))))
    (load test-file)
    (ert "t")))

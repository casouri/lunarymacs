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

;;; Customize
;;;; Server
(luna-run-server)
;;;; Theme
(when (window-system)
  (luna-load-theme 'pale))

(luna-on "Brown"
;;;; Font
  (when (display-graphic-p)
    (luna-enable-apple-emoji)
    (luna-load-font 'default "Cascadia" 14)
    (luna-load-font 'fixed-pitch "Cascadia" 14)
    (luna-load-font 'variable-pitch "Academica" 16)
    (luna-load-font 'fixed-pitch-serif "Cascadia" 14)
    (luna-load-font 'mode-line "Arial" 12 :weight 'regular :height 125)
    (with-eval-after-load 'shortdoc
      (luna-load-font 'shortdoc-section "Arial" 13
                      :weight 'medium :height 150))
    (add-hook 'luna-load-theme-hook
              (lambda ()
                (luna-load-font 'mode-line "Arial" 12
                                :weight 'regular :height 125))))
;;;; Frame
  (when (display-graphic-p)
    (set-frame-width (selected-frame) 150)
    (set-frame-height (selected-frame) 44)
    (set-frame-position nil 30 50))
;;;; Keys
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-pass-command-to-system nil ; fix cmd h
        mac-system-move-file-to-trash-use-finder t)
  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-v") #'yank)
  ;; I sometimes hit this by accident.
  (global-set-key (kbd "s-H") #'ignore)
;;;; Environment
  (if luna-dumped
      (dolist (var luna-env-vars)
        (exec-path-from-shell-setenv (car var) (cdr var)))
    (exec-path-from-shell-initialize))
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
  (setq source-directory (expand-file-name "~/emacs"))
  (setq xref-search-program 'ripgrep)
;;;; Scrolling
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

;;; Devel

(require 'treesit)
(push '(css-mode . css-ts-mode) major-mode-remap-alist)
(push '(python-mode . python-ts-mode) major-mode-remap-alist)
(push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
(push '(c-mode . c-ts-mode) major-mode-remap-alist)
(push '(c++-mode . c++-ts-mode) major-mode-remap-alist)
(push '(toml-mode . toml-ts-mode) major-mode-remap-alist)
(push '(tsx-mode . tsx-ts-mode) major-mode-remap-alist)
(push '(typescript-mode . tsx-ts-mode) major-mode-remap-alist)
(push '(javascript-mode . tsx-ts-mode) major-mode-remap-alist)

;; (setq treesit--font-lock-verbose t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) "ccls-clang-14"))
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode)
                 "typescript-language-server" "--stdio")))

(add-hook 'prog-mode-hook #'general-ts-mode-setup)
(add-hook 'c-ts-mode-hook #'c-ts-setup)
(add-hook 'css-ts-mode-hook 'ts-css-setup)

(defun general-ts-mode-setup ()
  ;; (treesit-font-lock-recompute-features
  ;;  nil
  ;;  '(property bracket delimiter operator variable function error))
  (when (derived-mode-p 'css-ts-mode)
    (treesit-font-lock-recompute-features
     '(property))))

(defun c-ts-setup ()
  (setq-local electric-quote-comment nil)
  (setq-local electric-quote-string nil)
  (indent-tabs-mode)
  (bug-reference-prog-mode)
  (treesit-font-lock-recompute-features '(emacs-devel))
  (setq c-ts-mode-emacs-sources-support nil))

(defun ts-css-setup ()
  (treesit-font-lock-recompute-features '(property) '(variable function)))

(defun luna-ts-largest-node-at-point ()
  "Get the largest node that starts at point."
  (let* ((node (treesit-node-at (point)))
         (next (treesit-node-parent node)))
    (while (and next (eq (point) (treesit-node-start next)))
      (setq node next
            next (treesit-node-parent next)))
    node))

(defun luna-ts-which-function-eldoc (&optional callback)
  "Eldoc documentation function, shows function at point."
  (when treesit-simple-imenu-settings
    (let ((node (treesit-node-at (point)))
          (stack nil))
      (while node
        (pcase-dolist (`(,category ,regexp ,pred ,name-fn)
                       treesit-simple-imenu-settings)
          (let ((matcher (if pred (cons regexp pred) regexp)))
            (when (treesit-node-match-p node matcher)
              (push (cons category
                          (funcall (or name-fn #'treesit-defun-name) node))
                    stack))))
        (setq node (treesit-node-parent node)))
      ;; Now STACK is a list of (CATEGORY . FUNC/CLASS/VAR-NAME).
      (string-join (mapcar (lambda (level)
                             (format "%s.%s" (car level) (cdr level)))
                           stack)
                   "\n"))))

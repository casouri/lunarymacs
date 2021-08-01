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
                    (lisp-interaction-mode)
                    (goto-char (point-max))))))
  ;; Add load-paths and load autoload files.
  (luna-load-relative "star/recipe.el")
  (package-initialize)
  (cowboy-add-load-path))

;;; Benchmark

;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
;; (benchmark-init/activate)

;;; Configs

;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(luna-key-def-preset :leader
  :keymaps 'override
  :prefix (if (display-graphic-p) "C-SPC" "C-@"))

(luna-key-def-preset :leader-prefix
  :prefix (if (display-graphic-p) "C-SPC" "C-@"))

(require 'no-littering)
(luna-load-relative "star/etc.el")
(luna-load-relative "star/key.el")
(luna-load-relative "star/angel.el")
(luna-load-relative "star/ui.el")
(luna-load-relative "star/mode-line.el")
(luna-load-relative "star/edit.el")
;; (luna-load-relative "star/ivy.el")
(luna-load-relative "star/checker.el")
(luna-load-relative "star/eglot.el")
(luna-load-relative "star/python.el")
(luna-load-relative "star/git.el")
(luna-load-relative "star/dir.el")
(luna-load-relative "star/org-mode.el")
(luna-load-relative "star/writing.el")
(luna-load-relative "star/tex.el")
(luna-load-relative "star/simple-mode.el")
(luna-load-relative "star/highres-icon.el")
;; (luna-load-relative "star/tool-bar.el")
(luna-load-relative "star/blog.el")
(require 'utility)

;;; Customize

;;;; Misc
(setq-default luna-format-on-save t)

;;;; Theme
(when (window-system)
  (luna-load-theme))

;;;; Font
(when (display-graphic-p)
  (luna-load-font)
  (luna-load-cjk-font))
(luna-on "Brown" (luna-enable-apple-emoji))

;;;; Server
(run-with-idle-timer
 3 nil (lambda ()
         (require 'server)
         (unless (eq (server-running-p) t)
           (server-start t t))))

;;;; Macports
(luna-on "Brown"
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp"))

;;;; Emacs Mac port
(luna-on "Brown"
  (menu-bar-mode -1)
  ;; (tool-bar-mode -1)
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
        scroll-conservatively 5))

;;;; Bidi
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;;; Local init

(let ((local-init (expand-file-name
                   "local-init.el" user-emacs-directory)))
  (when (file-exists-p local-init)
    (load local-init)))

(luna-on "Brown"
  (push "~/p/tree-sitter-expr/json-module" load-path)
  (push "~/p/tree-sitter-expr/c-module" load-path)
  (require 'tree-sitter-json)
  (require 'tree-sitter-c)
  (require 'tree-sitter)

  (defun tree-sitter-select-node (node)
    (let ((beg (tree-sitter-node-beginning node))
          (end (tree-sitter-node-end node)))
      (push-mark end)
      (activate-mark)
      (goto-char beg)))

  (defvar tree-sitter-expand-origin nil
    "Point before ‘tree-sitter-expand’ ran.")

  (defun tree-sitter-expand ()
    (interactive)
    (if (region-active-p)
        (let ((node (tree-sitter-node-in-range
                     (region-beginning) (region-end))))
          (tree-sitter-select-node
           (or (tree-sitter-node-parent node) root)))
      (setq tree-sitter-expand-origin (point))
      (tree-sitter-select-node
       (tree-sitter-node-in-range
        (point) (1+ (point))))))

  (defun tree-sitter-shrink ()
    (interactive)
    (when (and (region-active-p) tree-sitter-expand-origin)
      (let* ((beg (region-beginning))
             (end (region-end))
             (node (tree-sitter-node-in-range beg end))
             ;; Find a child that contains the original point.
             (child (car (tree-sitter-filter-child
                          node (lambda (child)
                                 (<= (tree-sitter-node-beginning child)
                                     tree-sitter-expand-origin
                                     (tree-sitter-node-end child)))))))
        (if child
            (tree-sitter-select-node child)
          (deactivate-mark)))))

  (luna-def-key "C-'" #'tree-sitter-expand
                "C-;" #'tree-sitter-shrink)

  (defun tree-sitter-show-buffer-tree ()
    (interactive)
    (let ((root-node (tree-sitter-parser-root-node
                      (or (car tree-sitter-parser-list)
                          (tree-sitter-create-parser (current-buffer) (tree-sitter-c))))))
      (pop-to-buffer (get-buffer-create "*tree-sitter-show-tree*"))
      (erase-buffer)
      (insert (pp-to-string (read (tree-sitter-node-string root-node)))))))

(put 'narrow-to-region 'disabled nil)



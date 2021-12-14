;; -*- lexical-binding: t -*-
;;
;; Simple config for programming modes.

(require 'utility)

;; Emacs Lisp
(luna-def-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "<S-return>" #'eval-defun
 "<C-S-return>" #'eval-defun-and-next
 :leader-prefix
 "eb" #'eval-buffer
 :---
 :keymaps 'lisp-interaction-mode-map
 "<S-return>" #'eval-print-sexp-at-point)

(defun eval-print-sexp-at-point ()
  "Evaluate top level sexp at point and print."
  (interactive)
  (save-excursion
    (end-of-defun)
    (skip-chars-backward "\n")
    (eval-print-last-sexp)))

(load-package markdown-mode
  :mode "\\.md$" "\\.markdown$" "\\.mk$")

(load-package yaml-mode
  :mode "\\.yaml$")

(load-package fish-mode
  :mode "\\.fish$")

(load-package haskell-mode
  :mode "\\.hs$"
  :config
  ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html#Interactive-Haskell
  (require 'console-buffer)
  (add-to-list 'luna-console-buffer-alist '(haskell-mode . "*haskell*"))
  (luna-def-key
   :keymaps 'haskell-mode-map
   "C-c C-l" #'haskell-process-load-file
   "C-c `"   #'haskell-interactive-bring
   "C-c C-t" #'haskell-process-do-type
   "C-c C-i" #'haskell-process-do-info
   "C-c C-c" #'haskell-process-cabal-build
   "C-c C-k" #'haskell-interactive-mode-clear
   "C-c c"   #'haskell-process-cabal
   :keymaps 'haskell-interactive-mode-map
   "C-a" #'haskell-interactive-mode-beginning))

(load-package web-mode
  :init
  (add-to-list 'luna-package-list 'flycheck t)
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'html-tidy 'web-mode))
  (add-hook 'web-mode-hook #'flycheck-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode)
  :mode
  "\\.phtml\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.html?\\'"
  :config (setq web-mode-markup-indent-offset 2
                web-mode-auto-close-style 2))

;; Common lisp.
(load-package sly
  :commands sly
  :init
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'common-lisp-mode-hook #'sly-mode)
  (setq inferior-lisp-program "sbcl")
  (add-to-list 'display-buffer-alist
               (cons "sly-xref for"
                     (cons 'display-buffer-in-side-window
                           '((side . bottom))))))

(load-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")

;; Javascript
(setq js-indent-level 2)

;; Makefile
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local whitespace-style '(tab-mark))
            (whitespace-mode)))

;; Scheme
;; Note: C-c C-a to activate a #lang operation in a racket file.
(load-package geiser
  :commands run-geiser
  :config
  (add-hook 'geiser-repl-mode
            (lambda ()
              (setq-local company-idle-delay nil)))
  (luna-def-key
   :keymaps 'geiser-mode-map
   "C-." nil
   "M-." nil))

(load-package geiser-racket
  :commands run-geiser run-racket)

(load-package geiser-guile
  :commands run-geiser run-guile
  :config
  (require 'console-buffer)
  (setf (alist-get 'scheme-mode luna-console-buffer-alist)
        "* Guile REPL *"))

;; C/C++
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local company-transformers nil)
                   (setq-local comment-multi-line t)
                   (eglot-ensure))))

;; XML
(defun setup-xml ()
  "Setup hideshow for XML file."
  (require 'hideshow)
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))
  (hs-minor-mode))
(add-hook 'nxml-mode-hook #'setup-xml)
(add-hook 'sgml-mode-hook #'setup-xml)

;; Rust
(load-package rust-mode
  :mode "\\.rs$"
  :config
  (add-hook 'rust-mode-hook #'eglot-ensure))

;;; General package

(load-package aggressive-indent
  :commands aggressive-indent-mode
  :hook ((emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
          lisp-mode-hook)
         . aggressive-indent-mode))

(load-package citre
  :init
  (setq citre-ctags-program "uctags"
        citre-readtags-program "ureadtags")
  (add-hook 'haskell-mode-hook #'citre-mode)
  :commands citre-mode)

(load-package eglot
  ;; Note: setting `eldoc-echo-area-use-multiline-p' keeps eldoc slim.
  :commands eglot eglot-ensure
  :config
  (setq read-process-output-max (* 1024 1024))
  ;; Otherwise eglot highlights documentations, which is annoying.
  (push :documentHighlightProvider eglot-ignored-server-capabilites)
  ;; Has to be here because needs ‘eglot-server-programs’ loaded.
  (luna-on "Brown"
    (add-to-list 'eglot-server-programs
                 '(c-mode . ("~/attic/ccls/Release/ccls")))
    (add-to-list 'eglot-server-programs
                 '(c++-mode . ("~/attic/ccls/Release/ccls")))
    (add-to-list 'eglot-server-programs
                 '(rust-mode . ("rust-analyzer")))))

;; (load-package quickrun
;;   :commands
;;   quickrun
;;   quickrun-region
;;   quickrun-with-arg
;;   quickrun-shell
;;   quickrun-compile-only
;;   quickrun-replace-region
;;   quickrun-autorun-mode)


;; (load-package lsp-mode
;;   :init (setq lsp-keymap-prefix "C-SPC l")
;;   :config (setq lsp-auto-guess-root t)
;;   :commands (lsp))

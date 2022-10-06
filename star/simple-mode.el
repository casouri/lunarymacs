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
 "<S-return>" #'eval-print-sexp-at-point
 :---
 :keymaps 'web-mode-map
 "C-M-f" #'web-mode-element-next
 "C-M-b" #'web-mode-element-previous)

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
  (luna-on "Brown"
    (setq inferior-lisp-program "ros run")
    (luna-safe-load (expand-file-name "~/.roswell/helper.el")))
  (add-to-list 'display-buffer-alist
               (cons "sly-xref for"
                     (cons 'display-buffer-in-side-window
                           '((side . bottom))))))

(load-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")

;; Shell
(defun shell-chmod ()
  "Make this shell script executable."
  (interactive)
  (if (derived-mode-p 'sh-mode 'shell-script-mode)
      (message "%s" (shell-command-to-string
                     (format "chmod +x %s" (buffer-file-name))))
    (user-error "Not in shell-script-mode")))

(defalias 'make-executable 'shell-chmod)

;; Javascript
(setq js-indent-level 2
      typescript-indent-level 2)
(load-package tide
  :autoload-hook (typescript-mode-hook . luna-setup-tide))
(defun luna-setup-tide ()
  "Setup for tide."
  (tide-setup)
  (flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq company-tooltip-align-annotations t))

;; Makefile
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local whitespace-style '(tab-mark))
            (whitespace-mode)))

;; JSON
(load-package flymake-json
  :extern "jsonlint"
  :autoload-hook
  (js-mode-hook . flymake-json-maybe-load)
  (json-mode-hook . flymake-json-load))

(luna-note-extern "jsonlint"
  "First install npm:
    sudo port install nodejs15
Then jslint:
    npm install jsonlint -g")

;; Scheme
;; Note: C-c C-a to activate a #lang operation in a racket file.
(load-package geiser
  :extern "racket-language-server"
  :commands run-geiser
  :config
  (add-hook 'geiser-repl-mode
            (lambda ()
              (setq-local company-idle-delay nil)))
  (luna-def-key
   :keymaps 'geiser-mode-map
   "C-." nil
   "M-." nil))

(luna-note-extern "racket-language-server"
  "raco pkg install racket-langserver")

(load-package geiser-racket
  :commands run-geiser run-racket)

(load-package geiser-guile
  :commands run-geiser run-guile
  :config
  (require 'console-buffer)
  (setf (alist-get 'scheme-mode luna-console-buffer-alist)
        "* Guile REPL *"))

;; C/C++
(defun setup-c ()
  "Setup for ‘c-mode’ and ‘c++-mode’."
  (setq-local company-transformers nil)
  (setq-local comment-multi-line t)
  (eglot-ensure))
(add-hook 'c-mode-hook #'setup-c)
(add-hook 'c++-mode-hook #'setup-c)

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
  :hook (rust-mode-hook . setup-rust)
  :config
  ;; If I format buffer before save, rustfmt won’t format in my back.
  ;; Eat this rustfmt!!
  (setq rust-format-on-save t)
  (defun setup-rust ()
    "Setup for ‘rust-mode’."
    (eglot-ensure)
    ;; For some reason format-on-save is turned on and I can’t turn it
    ;; off. And I don’t want to manually revert buffer after every
    ;; fucking save.
    (auto-revert-mode)
    ;; rustfmt’s line limit is too large.
    (toggle-word-wrap)
    (electric-quote-local-mode -1)))

;; Go
(load-package go-mode
  :mode "\\.go$"
  :hook (go-mode-hook . setup-go)
  :config
  (defun setup-go ()
    "Setup for ‘go-mode’."
    (eglot-ensure)))

(load-package protobuf-mode :mode "\\.proto")

;;; General package

(load-package aggressive-indent
  :commands aggressive-indent-mode
  :autoload-hook ((emacs-lisp-mode-hook
                   lisp-interaction-mode-hook
                   scheme-mode-hook
                   lisp-mode-hook)
                  . aggressive-indent-mode))

(load-package eglot
  ;; Note: setting `eldoc-echo-area-use-multiline-p' keeps eldoc slim.
  :commands eglot eglot-ensure
  :config
  (setq read-process-output-max (* 1024 1024))
  ;; Otherwise eglot highlights symbols, which is annoying.
  (add-to-list 'eglot-ignored-server-capabilities
               :documentHighlightProvider)
  (add-to-list 'eglot-ignored-server-capabilities
               :documentFormattingProvider)
  (add-to-list 'eglot-ignored-server-capabilities
               :documentRangeFormattingProvider)
  ;; Has to be here because needs ‘eglot-server-programs’ loaded.
  (luna-on "Brown"
    (add-to-list 'eglot-server-programs
                 '((c-mode c++-mode) . ("ccls-clang-10")))
    (add-to-list 'eglot-server-programs
                 '(rust-mode . ("rust-analyzer"))))
  ;; Show error message when hovering by point. (By default error
  ;; messages are only shown when hovering by cursor).
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function
                                  eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy
                    #'eldoc-documentation-compose))))

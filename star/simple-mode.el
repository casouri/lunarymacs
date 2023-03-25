;; -*- lexical-binding: t -*-
;;
;; Simple config for programming modes.

(require 'utility)

;; Emacs Lisp
(luna-key-def
 :leader
 "eg" #'eglot
 "lc" #'lspce-mode
 :---
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "<S-return>" #'eval-defun
 "<C-S-return>" #'eval-defun-and-next
 :---
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 :leader-prefix
 "eb" #'eval-buffer
 :---
 :keymaps 'lisp-interaction-mode-map
 "<S-return>" #'eval-print-sexp-at-point
 :---
 :keymaps 'web-mode-map
 "C-M-f" #'web-mode-tag-next
 "C-M-b" #'web-mode-tag-previous
 "C-s-f" #'web-mode-tag-next
 "C-s-b" #'web-mode-tag-previous
 :leader
 "ob" #'browse-url-of-file
 :---
 :keymaps 'eglot-mode-map
 "C-h C-h" #'eldoc-box-help-at-point)

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
  (luna-key-def
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

(load-package emmet-mode :defer t)

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
  :hook (web-mode-hook . setup-web)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-auto-close-style 2)
  (defun setup-web ()
    "Setup for ‘web-mode’."
    ;; This way expreg can use tree-sitter for expansion.
    (ignore-errors (treesit-parser-create 'html))
    (emmet-mode)))

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

;; Javascript/Typescript
(setq-default js-indent-level 2)
(add-hook 'tsx-ts-mode-hook #'setup-tsx)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))
(with-eval-after-load 'tsx-ts-mode
  (set-face-attribute 'typescript-ts-jsx-tag-face nil :inherit 'shadow)
  (set-face-attribute 'typescript-ts-jsx-attribute-face nil
                      :inherit 'font-lock-type-face))

(defun setup-tsx ()
  "Setup for ‘tsx-ts-mode’."
  ;; typescript-language-server sens immense amounts of noise over the
  ;; wire, which leads to bad lagging.
  (setq-local eglot-events-buffer-size 0)
  ;; Too many completions.
  (setq-local company-prefix 3)
  (add-hook 'post-command-hook #'tsx-tag-complete 0 t))

(defvar tsx-tag--void-elements
  ;; From ‘web-mode-void-elements’.
  '("area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr" "tmpl_var"))

(defun tsx-tag-complete (&rest _)
  "Complete HTML tags."
  (cond ((and (eq this-command 'self-insert-command)
              (eq (char-before) ?>))
         ;; <div| --(type ">")--> <div>|</div>
         (let* ((node (treesit-node-prev-sibling (treesit-node-at (point))))
                (type (treesit-node-text
                       (treesit-node-child-by-field-name node "name"))))
           (when (equal (treesit-node-type node) "jsx_opening_element")
             (save-excursion
               (if (member type tsx-tag--void-elements)
                   (progn (backward-char)
                          (insert "/"))
                 (save-excursion
                   (insert (format "</%s>" type))))))))
        ;; <div>|</div> --(type "RET")-->
        ;; <div>
        ;;   |
        ;; </div>
        ((eq this-command 'newline)
         (let ((node (treesit-node-parent (treesit-node-at (point)))))
           (when (and (eq (point) (treesit-node-start node))
                      (equal (treesit-node-type node) "jsx_closing_element"))
             (newline)
             (indent-for-tab-command)
             (forward-line -1)
             (indent-for-tab-command))))))

;; Makefile
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local whitespace-style '(tab-mark))
            (whitespace-mode)))

;; JSON
(load-package flymake-json
  :extern "jsonlint"
  :autoload-hook
  (( js-mode-hook js-ts-mode-hook
     tsx-ts-mode-hook typescript-ts-mode-hook)
   . flymake-json-maybe-load)
  (json-mode-hook . flymake-json-load))

(luna-note-extern "jsonlint"
  "First install npm:
    sudo port install nodejs15
Then jslint:
    npm install jsonlint -g")

;; Scheme
;; Note: C-c C-a to activate a #lang operation in a racket file.
;; (load-package geiser
;;   :commands run-geiser
;;   :config
;;   (luna-key-def
;;    :keymaps 'geiser-mode-map
;;    "C-." nil
;;    "M-." nil))

;; (load-package geiser-guile
;;   :commands run-geiser run-guile
;;   :config
;;   (require 'console-buffer)
;;   (setf (alist-get 'scheme-mode luna-console-buffer-alist)
;;         "* Guile REPL *"))

(load-package racket-mode
  :mode "\\.rkt\\'"
  :extern "racket-langserver"
  :hook (racket-mode-hook . setup-racket))

(defun setup-racket ()
  "Setup racket."
  nil)

(luna-note-extern "racket-langserver"
  "raco pkg install racket-langserver")

;; C/C++
(defun setup-c ()
  "Setup for ‘c-mode’ and ‘c++-mode’."
  (setq-local company-transformers nil)
  (setq-local comment-multi-line t))
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
;; (load-package rust-mode
;;   :mode "\\.rs$"
;;   :hook (rust-mode-hook . setup-rust)
;;   :config
;;   ;; If I format buffer before save, rustfmt won’t format in my back.
;;   ;; Eat this rustfmt!!
;;   (setq rust-format-on-save t)
;;   (defun setup-rust ()
;;     "Setup for ‘rust-mode’."
;;     ;; For some reason format-on-save is turned on and I can’t turn it
;;     ;; off. And I don’t want to manually revert buffer after every
;;     ;; fucking save.
;;     (auto-revert-mode)
;;     ;; rustfmt’s line limit is too large.
;;     (toggle-word-wrap)
;;     (electric-quote-local-mode -1)))

(load-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook
  (rust-ts-mode-hook . toggle-truncate-lines)
  (rust-ts-mode-hook . setup-rust)
  :init
  (defvar rust-analyzer-config
    ;; https://rust-analyzer.github.io/manual.html#configuration
    '(:rust-analyzer
      ( :procMacro ( :attributes (:enable t)
                     :enable t)
        :cargo (:buildScripts (:enable t))
        :diagnostics (:disabled ["unresolved-proc-macro"
                                 "unresolved-macro-call"])))
    "Configuration for rust-analyzer as :initializationOption.")
  :config
  (defun setup-rust ()
    "Setup for ‘rust-ts-mode’."
    (add-hook 'rust-ts-mode-hook #'eglot-format-buffer 0 t)
    (electric-quote-local-mode -1)))

;; Go
(load-package go-mode
  :mode "\\.go$"
  :hook (go-mode-hook . setup-go)
  :config
  (defun setup-go ()
    "Setup for ‘go-mode’."
    nil))

(load-package protobuf-mode :mode "\\.proto")

;;; General package

(load-package aggressive-indent
  :commands aggressive-indent-mode
  :autoload-hook ((emacs-lisp-mode-hook
                   lisp-interaction-mode-hook
                   scheme-mode-hook
                   racket-mode-hook
                   lisp-mode-hook)
                  . aggressive-indent-mode))

(load-package eglot
  ;; Note: setting `eldoc-echo-area-use-multiline-p' keeps eldoc slim.
  :hook (eglot-managed-mode-hook . setup-eglot)
  :config
  (setq-default read-process-output-max (* 1024 1024)
                eglot-events-buffer-size 0)

  ;; Otherwise eglot highlights symbols, which is annoying.
  (add-to-list 'eglot-ignored-server-capabilities
               :documentHighlightProvider)
  (add-to-list 'eglot-ignored-server-capabilities
               :inlayHintProvider)

  (luna-on "Brown"
    (add-to-list 'eglot-server-programs
                 '((c-mode c++-mode) . ("ccls-clang-10")))
    (add-to-list 'eglot-server-programs
                 `(rust-ts-mode . ("rust-analyzer"
                                   :initializationOptions
                                   ,rust-analyzer-config))))

  (defun setup-eglot ()
    "Setup for eglot."
    ;; Show error message when hovering by point. (By default error
    ;; messages are only shown when hovering by cursor). Show flymake
    ;; diagnostics first.
    (setq eldoc-documentation-functions
          (cons #'flymake-eldoc-function
                (remove #'flymake-eldoc-function
                        eldoc-documentation-functions)))
    ;; Show all eldoc feedback.
    (setq eldoc-documentation-strategy
          #'eldoc-documentation-compose)
    ;; (eglot-inlay-hints-mode)
    ))

(load-package eldoc-box
  :commands
  eldoc-box-hover-mode
  eldoc-box-help-at-point
  :config
  (set-face-attribute 'eldoc-box-body nil
                      :family "IBM Plex Sans" :height 140)
  (setq eldoc-doc-buffer-separator
        (propertize "-" 'display '(space :align-to right)
                    'face '(:strike-through t)
                    'font-lock-face '(:strike-through t))))

(load-package apheleia
  :extern "prettier"
  :autoload-hooks
  ((tsx-ts-mode typescript-ts-mode js-ts-mode js-mode)
   . apheleia-mode))

(luna-note-extern "prettier"
  "npm install prettier")

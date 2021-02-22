;; -*- lexical-binding: t -*-

;; Emacs Lisp
(luna-def-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "<S-return>" #'eval-defun
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

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)


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


(luna-on "Brown"
  (defun load-agda ()
    (interactive)
    (let ((coding-system-for-read 'utf-8))
      (load-file
       (shell-command-to-string "~/.cabal/bin/agda-mode locate"))
      (agda2-mode)))
  (add-to-list 'auto-mode-alist '("\\.l?agda\\'" . load-agda)))

(luna-on "Brown"
  (load-package matlab
    :init
    (setq matlab-shell-command
          "/Applications/MATLAB_R2018b.app/Contents/MacOS/MATLAB")
    (setq matlab-shell-command-switches (list "-nodesktop"))
    ;; don’t enable company in matlab-shell-mode
    :commands matlab-shell))


(load-package mips-mode
  :mode "\\.mips$")


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


;; common lisp
(load-package sly
  :commands sly
  :init
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'common-lisp-mode-hook #'sly-mode)
  (setq inferior-lisp-program "sbcl"))



(load-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")



;; Javascript
(setq js-indent-level 2)

(load-package tide
  :hook ((js-mode-hook. typescript-mode-hook) . setup-tide-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-local company-tooltip-align-annotations t)
  (tide-hl-identifier-mode +1)
  (add-to-list 'luna-smart-format-alist '(typescript-mode . tide-format-before-save))
  (add-to-list 'luna-smart-format-alist '(js-mode . tide-format-before-save)))

;; Makefile
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local whitespace-style '(tab-mark))
            (whitespace-mode)))

;; Scheme
;;
;; Note: C-c C-a to activate a #lang operation in a racket file.
(load-package geiser
  :commands run-geiser
  :config (add-hook 'geiser-repl-mode
                    (lambda ()
                      (setq-local company-idle-delay nil)))
  (require 'console-buffer)
  (setf (alist-get 'scheme-mode luna-console-buffer-alist)
        "* Guile REPL *"))

;; Racket
(load-package racket-mode
  :commands racket-mode
  :mode "\\.rkt$")


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

;;; General package

(load-package aggressive-indent
  :commands aggressive-indent-mode
  :hook ((emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
          lisp-mode-hook)
         . aggressive-indent-mode))


(load-package quickrun
  :commands
  quickrun
  quickrun-region
  quickrun-with-arg
  quickrun-shell
  quickrun-compile-only
  quickrun-replace-region
  quickrun-autorun-mode)


;; (load-package lsp-mode
;;   :init (setq lsp-keymap-prefix "C-SPC l")
;;   :config (setq lsp-auto-guess-root t)
;;   :commands (lsp))

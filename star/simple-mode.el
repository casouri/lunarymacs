;;; -*- lexical-binding: t -*-

(load-package markdown-mode
  :mode ("\\.md$" "\\.markdown$" "\\.mk$"))

(load-package yaml-mode
  :mode "\\.yaml$")

(load-package haskell-mode
  :mode "\\.hs$"
  :config
  ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html#Interactive-Haskell
  (add-to-list 'luna-console-buffer-alist '(haskell-mode . "*haskell*"))
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c `") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (with-eval-after-load 'haskell-interactive-mode
    (define-key haskell-interactive-mode-map (kbd "C-a") #'haskell-interactive-mode-beginning)))



(load-package matlab-emacs
  :init
  (setq matlab-shell-command "/Applications/MATLAB_R2018b.app/Contents/MacOS/MATLAB")
  (setq matlab-shell-command-switches (list "-nodesktop"))
  (add-hook 'matlab-shell-mode-hook (lambda () (company-mode -1)))
  :config (require 'matlab)
  :commands matlab-shell)

(load-package mips-mode
  :mode "\\.mips$")

(load-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             quickrun-autorun-mode)
  ;; from evil-collection
  :config (when (featurep 'evil)
            (evil-define-key 'normal quickrun--mode-map
                             "q" 'quit-window)))

(load-package web-mode
  :mode
  "\\.phtml\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.html?\\'"
  :config
  (when (featurep 'flycheck )
    (flycheck-add-mode 'html-tidy 'web-mode))
  (setq web-mode-markup-indent-offset 2
        web-mode-auto-close-style 2))

;; common lisp
(load-package sly
  :commands sly
  :init
  (add-hook 'common-lisp-mode-hook #'sly-mode)
  (setq inferior-lisp-program "ccl64"))

(load-package aggressive-indent
  :commands aggressive-indent-mode
  :init
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode))

(load-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")

(load-package pdf-tools
  :mode "\\.pdf%"
  :init (setq doc-view-resolution 320)
  :commands pdf-view-mode)

;; Javascript
(load-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)
                company-tooltip-align-annotations t)
    (tide-hl-identifier-mode +1)
    (add-to-list 'luna-smart-format-alist '(typescript-mode . tide-format-before-save))
    (add-to-list 'luna-smart-format-alist '(js-mode . tide-format-before-save)))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'js-mode-hook #'setup-tide-mode)
  :commands (setup-tide-mode))

(setq js-indent-level 2)

;; C/C++
(with-eval-after-load 'c-mode
  (eglot-ensure)
  ;; ccls has a fuzzy matching algorithm to order candidates according to your query.
  (setq-local company-transformers nil))

(with-eval-after-load 'c++-mode
  (eglot-ensure)
  ;; ccls has a fuzzy matching algorithm to order candidates according to your query.
  (setq-local company-transformers nil))

;; Debugger
(add-hook 'gud-mode-hook (lambda () (company-mode -1)))

(load-package realgud
  :commands (realgud:gdb realgud:lldb))

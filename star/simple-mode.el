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
  :config (when (featurep 'flycheck )
            (flycheck-add-mode 'html-tidy 'web-mode)))

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
(luna-lsp/eglot
 (progn
   (add-hook 'js-mode-hook #'lsp t)
   (add-hook 'typescript-mode-hook #'lsp t)
   (push '(js-mode . lsp-format-buffer) luna-smart-format-alist)
   (push '(typescript-mode . lsp-format-buffer) luna-smart-format-alist))
 (progn
   (add-hook 'js-mode-hook #'eglot-ensure)
   (add-hook 'typescript-mode #'eglot-ensure)
   (push '(js-mode . eglot-format-buffer) luna-smart-format-alist)
   (push '(typescript-mode . eglot-format-buffer) luna-smart-format-alist)))

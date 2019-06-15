;;; -*- lexical-binding: t -*-

(load-package markdown-mode
  :mode ("\\.md$" "\\.markdown$" "\\.mk$"))

(load-package yaml-mode
  :mode "\\.yaml$")

(load-package haskell-mode
  :mode "\\.hs$")

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

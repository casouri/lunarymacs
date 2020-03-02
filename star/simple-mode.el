;; -*- lexical-binding: t -*-

(load-package markdown-mode
  :mode ("\\.md$" "\\.markdown$" "\\.mk$")
  :init (add-hook 'markdown-mode-hook #'company-mode))


(load-package yaml-mode
  :mode "\\.yaml$"
  :init (add-hook 'yaml-mode-hook #'company-mode))


(load-package haskell-mode
  :mode "\\.hs$"
  :init (add-hook 'haskell-mode-hook #'company-mode)
  :config
  ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html#Interactive-Haskell
  (require 'console-buffer)
  (add-to-list 'luna-console-buffer-alist '(haskell-mode . "*haskell*"))
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
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
  (add-hook 'matlab-mode-hook #'company-mode)
  (setq matlab-shell-command "/Applications/MATLAB_R2018b.app/Contents/MacOS/MATLAB")
  (setq matlab-shell-command-switches (list "-nodesktop"))
  ;; don’t enable company in matlab-shell-mode
  :commands (matlab-shell))


(load-package mips-mode
  :mode "\\.mips$"
  :init (with-eval-after-load 'company
          (add-hook 'mips-mode-hook #'company-mode)))


(load-package web-mode
  :init
  (add-to-list 'luna-package-list 'flycheck t)
  (add-hook 'web-mode-hook #'company-mode)
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
  (add-hook 'common-lisp-mode-hook #'company-mode)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'common-lisp-mode-hook #'sly-mode)
  (setq inferior-lisp-program "ccl64"))



(load-package lua-mode
  :init (add-hook 'lua-mode-hook #'company-mode)
  :mode "\\.lua$"
  :interpreter "lua")



(load-package pdf-tools
  :mode "\\.pdf%"
  :init (setq doc-view-resolution 320)
  :commands pdf-view-mode)


;; Javascript
(setq js-indent-level 2)

(load-package tide
  :hook ((typescript-mode-hook . setup-tide-mode)
         (js-mode-hook. setup-tide-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (company-mode)
  (flycheck-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-local company-tooltip-align-annotations t)
  (tide-hl-identifier-mode +1)
  (add-to-list 'luna-smart-format-alist '(typescript-mode . tide-format-before-save))
  (add-to-list 'luna-smart-format-alist '(js-mode . tide-format-before-save)))


;; C/C++
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook (lambda ()
                   (company-mode)
                   (eglot-ensure)
                   (dash-underscore-mode)
                   ;; ccls has a fuzzy matching algorithm to order
                   ;; candidates according to your query.
                   (setq-local company-transformers nil)
                   (setq-local comment-multi-line t))))


;; Makefile
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local whitespace-style '(tab-mark))
            (whitespace-mode)))

;; Scheme
(load-package geiser
  :commands (run-geiser)
  :config (add-hook 'geiser-repl-mode
                    (lambda ()
                      (setq-local company-idle-delay nil)))
  (require 'console-buffer)
  (setf (alist-get 'scheme-mode luna-console-buffer-alist)
        "* Guile REPL *"))

;;; Genarl package
(load-package aggressive-indent
  :commands (aggressive-indent-mode))


(load-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             quickrun-autorun-mode))


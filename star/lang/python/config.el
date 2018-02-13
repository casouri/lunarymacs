;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar python-enable-format-on-save t)

;;
;; Config
;;

(delay-use-package| lsp-python
  :hook (python-mode . lsp-python-enable)
  :config
  (when python-enable-format-on-save
    (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook 'lsp-format-buffer))))
  )

(delay-use-package| pyenv
  :hook (python-mode . pyenv-mode)
  )

(delay-use-package| quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             quickrun-autorun-mode
             ))

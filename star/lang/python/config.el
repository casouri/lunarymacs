;;
;; Var
;;

(defvar python-enable-format-on-save t)

;;
;; Config
;;

(use-package| lsp-python
  :hook (python-mode . lsp-python-enable)
  :config
  (when python-enable-format-on-save
    (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook 'lsp-format-buffer))))
  )

(use-package| pyenv
  :hook (python-mode . pyenv-mode)
  )

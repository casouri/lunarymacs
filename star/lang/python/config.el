;;; -*- lexical-binding: t -*-

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

(use-package| pyvenv
  :commands pyvenv-activate
  )

(use-package| quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             quickrun-autorun-mode
             ))

(setq python-shell-interpreter "/usr/local/bin/python3")

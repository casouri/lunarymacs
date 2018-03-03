;;; -*- lexical-binding: t -*-


;;
;; Config
;;

(use-package| lsp-python
  :hook (python-mode . lsp-python-enable)
  :config (add-hook 'python-mode-hook #'lsp-format-on-save-mode)
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

(use-package| ein
  :commands (ein:notebooklist-open
             ein:notebooklist-new-notebook
             ein:notebooklist-open-notebook-global
             ein:notebooklist-login
             ein:junk-new))


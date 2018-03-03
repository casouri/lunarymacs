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
             ein:junk-new)
  :config
  (setq ein:worksheet-enable-undo 'yes)
  (load (concat
         moon-emacs-d-dir
         "star/lang/python/jpnb-state.el")))

(post-config| general
  (general-define-key
   :keymaps '(ein:notebook-mode-map
              ein:notebook-multilang-mode-map
              ein:notebook-python-mode-map
              ein:notebook-plain-mode-map
              override)
   "gj" #'evil-jpnb-state
   ))

;; (define-key ein:notebook-multilang-mode-map "gj" #'evil-jpnb-state)

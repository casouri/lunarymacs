;;; -*- lexical-binding: t -*-


;;
;; Config
;;

(defvar moon-enable-format-on-save-python nil
  "Whether to enabel lsp-python format on save.")

(use-package| lsp-python
  ;; :hook (python-mode . lsp-python-enable)
  :init
  (add-hook 'python-mode-hook #'lsp-python-enable)
  (add-hook 'python-mode-hook #'setup-lsp)
  :config
  (when moon-enable-format-on-save-python
    (add-hook 'python-mode-hook #'lsp-format-on-save-mode)))


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
             )
  ;; from evil-collection
  :config (when (featurep 'evil)
            (evil-define-key 'normal quickrun--mode-map
              "q" 'quit-window)))

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
         "star/lang/python/jpnb-state/jpnb-state.el")))

(post-config| general
  (local-leader
    :keymaps '(ein:notebook-mode-map
              ein:notebook-multilang-mode-map
              ein:notebook-python-mode-map
              ein:notebook-plain-mode-map
              )
   "j" #'evil-jpnb-state
   ))

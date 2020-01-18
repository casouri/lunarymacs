;;; -*- lexical-binding: t -*-

;;; Key
(with-eval-after-load 'luna-general-config
  (general-define-key
   :prefix "C-x"
   :keymaps 'emacs-lisp-mode-map
   ;; eval
   "w" '(:ignore t :which-key "eval")
   "w e" #'eval-last-sexp
   "w r" #'eval-region
   "w f" #'eval-defun
   "w b" #'eval-buffer))

;;; Package
;; (load-package lispyville
;;   :hook (emacs-lisp-mode . lispyville-mode)
;;   :config
;;   (lispyville-set-key-theme
;;    '(operators
;;      prettify
;;      text-objects
;;      additional-motions
;;      slurp/barf-cp)))

;;; Config

(dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook #'flymake-mode)
  (add-hook hook #'company-mode)
  (add-hook hook #'aggressive-indent-mode))

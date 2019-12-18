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

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(add-hook 'emacs-lisp-mode-hook #'company-mode)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

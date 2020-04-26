;; -*- lexical-binding: t -*-

;;; Key

(with-eval-after-load 'luna-general-config
  (general-define-key
   :prefix "C-x"
   :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   ;; eval
   "w" '(:ignore t :which-key "eval")
   "we" #'eval-last-sexp
   "wr" #'eval-region
   "wf" #'eval-defun
   "wb" #'eval-buffer))

;;; Package

;; interfere with Emacs development
;; (load-package sly-el-indent
;;   :commands sly-el-indent-setup
;;   :hook (emacs-lisp-mode-hook . sly-el-indent-setup))

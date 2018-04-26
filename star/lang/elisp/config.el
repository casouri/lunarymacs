;;; -*- lexical-binding: t -*-

(use-package| lispyville
  :hook (emacs-lisp-mode . lispyville-mode))


(post-config| general
  (default-leader
   :keymaps 'emacs-lisp-mode-map
   ;; eval
   "ee" '(:ignore t :which-key "eval")
   "eee" #'eval-last-sexp
   "eer" #'eval-region
   "eef" #'eval-defun
   "eeb" #'eval-buffer))

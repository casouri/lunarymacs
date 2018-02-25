;;; -*- lexical-binding: t -*-

(use-package| lispyville
  :hook (emacs-lisp-mode . lispyville-mode))


(post-config| general
  (local-leader
   :keymaps 'emacs-lisp-mode-map
   ;; eval
   "er" #'eval-region
   "ef" #'eval-function
   "eb" #'eval-buffer))

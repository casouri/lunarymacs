;; -*- lexical-binding: t -*-

;;; Key

(with-eval-after-load 'luna-general-config
  (general-define-key
   :prefix "C-x"
   :keymaps 'emacs-lisp-mode-map
   ;; eval
   "w" '(:ignore t :which-key "eval")
   "we" #'eval-last-sexp
   "wr" #'eval-region
   "wf" #'eval-defun
   "wb" #'eval-buffer))


;;; Config

(dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook #'flymake-mode)
  (add-hook hook #'company-mode)
  (add-hook hook #'aggressive-indent-mode))

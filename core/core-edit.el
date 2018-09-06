;; -*- lexical-binding: t -*-

(defvar moon-smart-format-alist ()
  "Alist of format functions of each major mode.
Each element should be a con cell of major mode symbol and function symbol.
For example, '(python-mode . format-python)")

(defvar moon-format-on-save nil
  "Whether to format on save.")

(defun moon/smart-format-buffer ()
  "Only format buffer when `moon-format-on-save' is non-nil."
  (interactive)
  (when moon-format-on-save
    (eval (cdr (assoc major-mode moon-smart-format-alist)))))

(defun strip-text-properties(text)
  "Return TEXT without any properties."
  (set-text-properties 0 (length text) nil text)
  text)

(defvar moon-evil nil
  "whether to use evil.")

(defmacro moon-vim-emacs (vim emacs)
  "If `moon-evil' is true:
Evaluate VIM;
otherwise evalueate EMACS."
    `(if moon-evil
         ,vim
       ,emacs))

(defalias 'mve #'moon-vim-emacs)

;;
;;; Config
;;

(add-hook 'after-save-hook #'moon/smart-format-buffer)



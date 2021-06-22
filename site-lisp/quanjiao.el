;;; quanjiao.el --- 全角引号      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(defface quanjiao-quote '((t . (:family "Source Han Serif SC")))
  "Face for full width quotation marks."
  :group 'convenience)

(defvar quanjiao-font-family "Source Han Serif SC"
  "Font family used for quanjiao quotes.")

(defvar-local quanjiao--quote-face `((t . (:family ,quanjiao-font-family)))
  ;; Defined as local variable because `quanjiao-mode' is local.
  "The face used for quanjiao quotes.
This variable is automatically set when `quanjiao-mode' is enabled.
Instead of changing this variable, customize `quanjiao-font-family'
and restart `quanjiao-mode'.")

(defun quanjiao-matcher (limit)
  "Matcher for font-lock."
  (and (re-search-forward "[‘’“”]" limit t)
       (or (memq (aref char-script-table (char-after))
                 '(han cjk-misc))
           (memq (aref char-script-table (char-before (1- (point))))
                 '(han cjk-misc)))))

(define-minor-mode quanjiao-mode
  "Display full width quotation marks."
  :lighter ""
  (if quanjiao-mode
      (progn
        (setq quanjiao--quote-face
              `((t . (:family ,quanjiao-font-family))))
        (font-lock-add-keywords
         nil '((quanjiao-matcher . quanjiao--quote-face))))
    (font-lock-remove-keywords
     nil '((quanjiao-matcher . quanjiao--quote-face))))
  (jit-lock-refontify))

(provide 'quanjiao)

;;; quanjiao.el ends here

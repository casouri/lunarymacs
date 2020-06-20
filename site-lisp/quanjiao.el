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

;; (defun quanjiao-fontify (beg end)
;;   "Fontify functions for jit-lock."
;;   (save-excursion
;;     (goto-char beg)
;;     (while (re-search-forward "[‘’“”]" end t)
;;       (when (or (memq (aref char-script-table (char-after))
;;                       '(han cjk-misc))
;;                 (memq (aref char-script-table (char-before (1- (point))))
;;                       '(han cjk-misc)))
;;         (put-text-property (1- (point)) (point) 'font-lock-face 'quanjiao-quote)
;;         (put-text-property (1- (point)) (point) 'face 'quanjiao-quote)))
;;     (cons 'jit-lock-bounds (cons beg end))))

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
      (font-lock-add-keywords nil '((quanjiao-matcher . 'quanjiao-quote)))
    (font-lock-remove-keywords nil '((quanjiao-matcher . 'quanjiao-quote))))
  (jit-lock-refontify))

(provide 'quanjiao)

;;; quanjiao.el ends here

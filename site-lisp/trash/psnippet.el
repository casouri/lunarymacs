;;; psnippet.el --- Poor man's support for LSP snippet      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(defface placeholder (let ((display t)
                           (attributs '(:inherit highlight)))
                       (list (cons display attributs)))
  "Face for argument placeholders."
  :group 'company)

(defvar placeholder-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'next-placeholder)
    (define-key map (kbd "<backtab>") #'previous-placeholder)
    map)
  "Keymap applies when point is on a placeholder.")

(defun make-placeholder (text tooltip)
  "Return a  placeholder TEXT with TOOLTIP."
  (propertize "*"
              'display text
              'help-echo tooltip
              'insert-in-front-hooks (list (lambda (_ end)
                                             ;; self-remove of placeholder
                                             (save-excursion
                                               (goto-char end)
                                               (delete-char 1))))
              'face 'placeholder
              'font-lock-face 'placeholder
              'rear-nonsticky t
              'placeholder t
              'keymap 'placeholder-map))

(defun next-placeholder ()
  "Jump to next placeholder."
  (interactive)
  (when-let ((next (save-excursion
                     (forward-char)
                     (next-single-char-property-change
                      (point) 'placeholder)
                     nil (min (point-max) (+ (point) 1000)))))
    (goto-char (1- next))))

(defun previous-placeholder ()
  "Jump to previous placeholder."
  (interactive)
  (when-let ((prev (save-excursion
                     (backward-char)
                     (previous-single-char-property-change
                      (point) 'placeholder
                      nil (max (point-min) (- (point) 1000))))))
    (goto-char (1- prev))))

(setq ph (make-placeholder "arg" "help"))

(provide 'psnippet)

;;; psnippet.el ends here

;;; pause.el --- Pause      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'pcase)

(defvar pause--stack nil
  "A stack of continue and quit functions.
Each element is an alist that looks like ((continue . fn) (quit . fn)).")

(define-minor-mode pause-minor-mode
  "Minor mode for pause facility."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'pause-continue)
            (define-key map (kbd "C-g") #'pause-quit)
            map))

(defun pause--control-minor-mode ()
  "Turn on/off `pause-minor-mode'.
Call after modified ‘pause--stack’."
  (when (eql (length pause--stack) 0)
    (pause-minor-mode -1))
  (when (and (> (length pause--stack) 0)
             (not pause-minor-mode))
    (pause-minor-mode)))

(defun pause-continue ()
  "Continue pause."
  (interactive)
  (unwind-protect
      (funcall (alist-get 'continue (pop pause--stack)))
    (pause--control-minor-mode)))

(defun pause-quit ()
  "Quit pause."
  (interactive)
  (unwind-protect
      (funcall (alist-get 'quit (pop pause--stack)))
    (pause--control-minor-mode)
    (keyboard-quit)))

(defmacro pause (continue &optional quit finally)
  "”Pause” the execution.
So the user can do arbitrary actions and continue from where one left.
Return immediately, run CONTINUE & FINALLY if user press C-c C-c,
run QUIT & FINALLY if user press C-g."
  (declare (indent 0))
  `(progn
     ;; save lexical environment
     (push (list (cons 'continue (lambda () (unwind-protect ,continue ,finally)))
                 (cons 'quit (lambda () (unwind-protect ,quit ,finally))))
           pause--stack)
     (pause--control-minor-mode)))

(provide 'pause)

;;; pause.el ends here

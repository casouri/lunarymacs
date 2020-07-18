;;; pause.el --- Pause      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'pcase)
(require 'cl-lib)

(define-minor-mode pause-minor-mode
  "Minor mode for pause facility."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'exit-recursive-edit)
            (define-key map (kbd "C-g") #'abort-recursive-edit)
            map))

(defvar pause--level 0
  "Level of nested pause calls.")

(defmacro pause (continue &optional quit finally)
  "”Pause” the execution.
So the user can do arbitrary actions and continue from where they left.
Return immediately, run CONTINUE & FINALLY if user press C-c C-c,
run QUIT & FINALLY if user press C-g."
  (declare (indent 0))
  `(progn
     (pause-minor-mode)
     (cl-incf pause--level)
     (condition-case nil
         (progn (recursive-edit)
                ,continue)
       (quit ,quit))
     ,finally
     (cl-decf pause--level)
     (if (eq pause--level 0)
         (pause-minor-mode -1))))

(provide 'pause)

;;; pause.el ends here

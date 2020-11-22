;;; measure-repeat.el --- Measure how fast can command loop handle inputs      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(defvar measure-repeat-mean nil)

(defvar measure-repeat-last-time nil)

(defun measure-repeat ()
  (interactive)
  (when (eq last-command #'measure-repeat)
    (let ((elapsed-time
           (time-convert
            (time-subtract (current-time) measure-repeat-last-time)
            'integer)))
      (setq measure-repeat-mean
            (if measure-repeat-mean
                (/ (+ elapsed-time measure-repeat-mean) 2.0)
              elapsed-time)
            measure-repeat-last-time (current-time)))))

(defun measure-repeat-clear ()
  (interactive)
  (setq measure-repeat-mean nil))

(when nil
  (global-set-key (kbd "C-o") #'measure-repeat))

(provide 'measure-repeat)

;;; measure-repeat.el ends here

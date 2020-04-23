;;; luna-local.el --- Local Persistent Storage      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-f)

(defvar luna-local-file (expand-file-name "local.el" user-emacs-directory)
  "File for local storage.")

(defvar luna-local--pending-alist nil
  "An alist of (VAR . VAL) pending to be stored into ‘luna-local-file’.")

(defvar luna-local--save-timer nil
  "Timer that runs ‘luna-local-save’ which saves variables set by
  ‘luna-local-set’.")

(defun luna-local-set (var val)
  "Store (VAR . VAL) in local storage.

Of course, you can only save printable objects like string.
Non-printable object includes buffers, window configurations,
frames, windows, etc."
  (push (cons var val) luna-local--pending-alist)
  (unless luna-local--save-timer
    (run-with-idle-timer 1 nil #'luna-local-save)))

(defun luna-local-save ()
  "Save variables set by ‘luna-local-set’."
  (when (timerp luna-local--save-timer)
    (cancel-timer luna-local--save-timer))
  (unwind-protect
      (progn
        (luna-f-touch luna-local-file)
        (let* ((val-list (luna-f-read luna-local-file))
               ;; Remove elements in ‘val-list’ that’s also in
               ;; ‘luna-local--pending-alist’.
               (cleaned-list (cl-reduce
                              (lambda (lst b)
                                (assq-delete-all b lst))
                              (mapcar #'car luna-local--pending-alist)
                              :initial-value val-list))
               (save-list (append luna-local--pending-alist cleaned-list)))
          (luna-f-write-obj luna-local-file save-list)))
    (setq luna-local--pending-alist nil)))

(defun luna-local-load ()
  "Load ‘luna-local-file’."
  (when (file-exists-p luna-local-file)
    (let ((var-list (luna-f-read luna-local-file)))
      (dolist (elt var-list)
        (set (car elt) (cdr elt))))))

(provide 'luna-local)

;;; luna-local.el ends here

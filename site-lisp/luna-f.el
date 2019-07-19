;;; luna-f.el --- File system functions      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'seq)

(defun luna-f-list-directory (dir &optional absolute)
  "Return a list of directories in DIR.
Return absolute path if ABSOLUTE is t."
  ;; FULL argument in ‘directory-files’ must be t,
  ;; otherwise ‘file-directory-p’ doesn’t work
  (mapcar (lambda (path)
            (if absolute
                path
              (file-name-nondirectory path)))
          (seq-filter (lambda (file)
                        (and (not (string-match (rx "/" (** 1 2 ".") eol)
                                                file))
                             (file-directory-p file)))
                      (directory-files dir t))))

(defun luna-f-directory-files (dir &optional absolute)
  "Return a list of regular files in DIR.
Return absolute path if ABSOLUTE is t."
  (mapcar (lambda (path)
            (if absolute
                path
              (file-name-nondirectory path)))
          (seq-filter (lambda (file)
                        (and (not (string-match (rx "/" (** 1 2 ".") eol)
                                                file))
                             (file-regular-p file)))
                      (directory-files dir t))))

(defun luna-f-join (&rest path-list)
  "Join paths in PATH-LIST."
  (if (eql (length path-list) 1)
      (car path-list)
    (expand-file-name (car (last path-list))
                      (apply #'luna-publish--join-path (butlast path-list)))))

(provide 'luna-f)

;;; luna-f.el ends here

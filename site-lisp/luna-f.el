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
                      (apply #'luna-f-join (butlast path-list)))))

(defun luna-f-content (path)
  "Read text of file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun luna-f-write (path string)
  "Write STRING into file at PATH."
  (with-temp-buffer
    (insert string)
    (write-file path)))

(defun luna-f-read (path)
  "Read file at PATH as a Lisp object."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))

(defun luna-f-write-obj (path obj)
  "Write OBJ to file at PATH."
  (with-temp-buffer
    (prin1 obj (current-buffer))
    (write-file path)))

(defun luna-f-content-literally (path)
  "Read text of file at PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun luna-this-file-directory ()
  "Return the directory of the file at where the code is."
  (file-name-directory (or load-file-name buffer-file-name)))

(provide 'luna-f)

;;; luna-f.el ends here

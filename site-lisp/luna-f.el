;;; luna-f.el --- File system functions      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This is not used anymore, vanilla functions can do most of what I
;; want and there is no point requiring an extra package.

;;; Code:

(require 'seq)

(defun luna-f-list-directory (dir &optional full)
  "Return a list of directories in DIR.
Return full path if FULL is non-nil."
  (let ((default-directory dir))
    (seq-filter #'file-directory-p
                (directory-files
                 dir full directory-files-no-dot-files-regexp))))

(defun luna-f-directory-files (dir &optional full)
  "Return a list of regular files in DIR.
Return full path if FULL is non-nil."
  (let ((default-directory dir))
    (seq-filter #'file-regular-p
                (directory-files
                 dir full directory-files-no-dot-files-regexp))))

(defun luna-f-join (base file)
  "Join together BASE and FILE."
  (expand-file-name file base))

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

(defun luna-f-read (path &optional default)
  "Read file at PATH as a Lisp object.
If the file is empty, return DEFAULT."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (condition-case nil
        (read (current-buffer))
      (end-of-file default))))

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

(defun luna-f-this-file-directory ()
  "Return the directory of the file at where the code is."
  (file-name-directory (or load-file-name buffer-file-name)))

(defmacro luna-f-with-file (file &rest form)
  "Open a temp buffer, insert FILE’s content, eval FORM."
  (declare (debug (sexp &rest sexp)) (indent 1))
  `(with-temp-buffer
     (insert-file-contents ,file)
     (goto-char (point-min))
     ,@form))

(defmacro luna-f-with-edit-file (file &rest form)
  "Open a temp buffer, insert FILE’s content, eval FORM.
Finally write visible region to file."
  (declare (debug (sexp &rest sexp)) (indent 1))
  (let ((buf-sym (gensym)))
    `(with-temp-buffer
       (let ((,buf-sym (current-buffer)))
         (insert-file-contents ,file)
         (goto-char (point-min))
         ,@form
         (with-current-buffer ,buf-sym
           (write-region (point-min) (point-max) ,file))))))

(defalias 'luna-f-with-write-file #'with-temp-file)

(defun luna-f-touch (path)
  "Touch PATH."
  (if (file-exists-p path)
      (set-file-times path)
    (with-temp-buffer
      (write-file path ))))

(defun luna-f-subtract (base path)
  "Subtract BASE from PATH.
Example:

    (luna-f-subtract \"~\" \"~/.emacs\")
-> \".emacs\"
"
  (file-relative-name path base))

(defun luna-f-trim (path suffix)
  "Trim SUFFIX from PATH.

For example, if PATH is “~/p/casouri/”, SUFFIX is “/casouri”,
return “~/p/”. If SUFFIX has a preceding “/”, then the returned
path doesn’t have a trailing “/” and vice versa. Return nil if
PATH doesn’t have SUFFIX as proper suffix."
  (let ((path (file-name-as-directory path))
        (suffix (file-name-as-directory suffix)))
    (when (string-suffix-p suffix path)
      (string-remove-suffix suffix path))))

(defun luna-f-change-extension (file extension)
  "Change the extension of FILE to EXTENSION."
  (let ((base (file-name-base file))
        (dir (or (file-name-directory file) "")))
    (format "%s%s.%s" dir base extension)))

(provide 'luna-f)

;;; luna-f.el ends here

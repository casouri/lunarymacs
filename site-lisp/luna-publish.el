;;; luna-publish.el --- Publish blog      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-f)

(defun luna-publish-html-export (dir &optional force)
  "Export index.org to index.html in DIR if the latter is older.
If FORCE is non-nil, only export when org file is newer than html file."
  (let ((org-file (expand-file-name "index.org" dir))
        (html-file (expand-file-name "index.html" dir)))
    (when (and (file-exists-p org-file)
               (or force (file-newer-than-file-p org-file html-file)))
      (let ((buffer (find-file org-file)))
        (with-current-buffer buffer
          (org-html-export-to-html))))))

(defmacro luna-publish-with-tmp-buffers (&rest body)
  "Evaluate BODY, but don’t introduce new buffers."
  (let ((old-buffer-list-sym (gensym)))
    `(let ((,old-buffer-list-sym (buffer-list)))
       ,@body
       (dolist (buf (buffer-list))
         (unless (member buf ,old-buffer-list-sym)
           (kill-buffer buf))))))

(defmacro luna-publish-with-theme (theme &rest body)
  "Use THEME during BODY."
  (declare (indent 1))
  (let ((old-theme-sym (gensym)))
    `(let ((,old-theme-sym luna-current-theme))
       (luna-load-theme ,theme)
       ,@body
       (luna-load-theme ,old-theme-sym))))


(provide 'luna-publish)

;;; luna-publish.el ends here

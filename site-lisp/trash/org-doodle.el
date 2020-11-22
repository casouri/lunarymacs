;;; org-doodle.el --- Doodles in Org Mode      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'org)
(require 'subr-x)

(require 'org-download)

(defvar org-doodle-canvas-path
  (concat "file://" (expand-file-name "doodle.png")))

(defun org-doodle-insert ()
  (interactive)
  (org-download-image org-doodle-canvas-path))

(defun org-doodle-edit ()
  (interactive)
  (if-let ((link (plist-get
                  (plist-get (text-properties-at (point))
                             'htmlize-link)
                  :uri)))
      (shell-command-to-string
       (format "open %s" (string-remove-prefix "file:" link)))))

(provide 'org-doodle)

;;; org-doodle.el ends here

;; rss-export.el --- My RSS export library      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'ox-html)
(require 'ox-cjk-html)

(defvar rss-export-html-relative-dir ""
  "E.g., “/2020/insert-math-symbol”. 
This is used to fix relative link under a post. So “./file” becomes
“/note/2020/insert-math-symbol/file”.

Always use absolute paths from site root, don’t use relative
links (although the name of the variable is “relative”), because
offline RSS readers can’t interpret relative image paths.")

(org-export-define-derived-backend 'rss-html 'cjk-html
  :translate-alist '((link . rss-export-link)
                     (export-block . rss-export-block))
  ;;                                       keyword option default
  :options-alist '((:rss-html-relative-dir nil nil rss-export-html-relative-dir)))

(defun rss-export-fix-relative-link (text relative-dir root-dir)
  "Fix the relative links in TEXT and return.

RELATIVE-DIR is the absolute path to the post directory. E.g.,
“/.../2020/insert-match-symbol”.

ROOT-DIR is the absolute path to the project root."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward (rx (seq "\""
                                       (group (or "./" "../")
                                              (+? nonl))
                                       "\""))
                              nil t)
      (let* ((path (match-string 1))
             (full-path (expand-file-name path relative-dir)))
        ;; If the file doesn’t exist, we probably hit a wrong match.
        (when (file-exists-p full-path)
          (let ((url-path (luna-f-subtract root-dir full-path)))
            (replace-match (format "\"/%s\"" url-path))))))
    (buffer-string)))

(defun rss-export-link (link desc info)
  (rss-export-fix-relative-link (org-html-link link desc info)
                                (plist-get info :rss-html-relative-dir)
                                (plist-get info :rss-html-root-dir)))

(defun rss-export-block (export-block _contents info)
  (when (string= (org-element-property :type export-block) "HTML")
    (rss-export-fix-relative-link
     (org-remove-indentation (org-element-property :value export-block))
     (plist-get info :rss-html-relative-dir)
     (plist-get info :rss-html-root-dir))))

(provide 'rss-export)

;;; rss-export.el ends here

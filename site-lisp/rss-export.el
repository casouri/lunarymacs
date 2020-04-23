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
  :translate-alist '((link . rss-export-link))
  ;;                                       keyword option default
  :options-alist '((:rss-html-relative-dir nil nil rss-export-html-relative-dir)))

(defun rss-export-fix-relative-link (text relative-dir)
  "Fix the relative links in TEXT and return.
RELATIVE-DIR is the relative path to the post directory. E.g.,
“/2020/insert-match-symbol”. It has to have a beginning slash."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward (rx (seq "\"./" (group (+? nonl))
                                       "\""))
                              nil t)
      (replace-match
       ;; Absolute path, no dot, see ‘rss-export-html-relative-dir’
       ;; for more info.
       (format "\"%s\"" (luna-f-join relative-dir (match-string 1)))))
    (buffer-string)))

(defun rss-export-link (link desc info)
  (rss-export-fix-relative-link (org-html-link link desc info)
                                (plist-get info :rss-html-relative-dir)))



(provide 'rss-export)

;;; rss-export.el ends here

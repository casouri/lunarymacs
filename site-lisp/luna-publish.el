;;; luna-publish.el --- Publish blog      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-f)
(require 'subr-x)

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

;;; RSS

(defun luna-publish-rss-export (link category-list dir &optional force)
  "Export index.html to css-item.xml in DIR if the latter is older.
If FORCE is non-nil, only export when org file is newer than html file.

LINK is the web link for the post in dir.
Categories in CATEGORY-LIST are strings."
  (let ((org-file (expand-file-name "index.org" dir))
        (rss-file (expand-file-name "css-item.xml" dir)))
    (when (file-exists-p org-file)
      (if (or force ; force export
              (not (file-exists-p rss-file)) ; rss doesn’t exist
              (file-newer-than-file-p org-file rss-file)) ; org newer
          (let* ((intro (or (string-remove-suffix
                             "\n"
                             (with-current-buffer (find-file org-file)
                               (org-element-map
                                   (org-element-parse-buffer)
                                   'plain-text #'identity
                                   nil t)))
                            "No intro."))
                 ;; (html (with-current-buffer (find-file org-file)
                 ;;         (let ((org-html-home/up-format ""))
                 ;;           (org-export-as 'html))))
                 (env (with-current-buffer (find-file org-file)
                        (org-export-get-environment)))
                 (date (format-time-string
                        "%a, %d %b %Y %H:%M:%S %z"))
                 (title (or (car (plist-get env :title))
                            "No title"))
                 (link (url-encode-url link)))
            (with-current-buffer (find-file rss-file)
              (erase-buffer)
              (insert (string-join '("<item>"
                                     (format "<title>%s</title>" title)
                                     (format "<link>%s</link>" link)
                                     (format "<guid>%s</guid>" link)
                                     (format "<description>%s</description>" intro)
                                     ;; (format "<description>![CDATA[%s]]</description>\n" html)
                                     (format "<pubDate>%s</pubDate>" date)
                                     "</item>\n")
                                   "\n"))
              (save-buffer)
              (buffer-string)))
        ;; if not newer
        (luna-f-content rss-file)))))

(provide 'luna-publish)

;;; luna-publish.el ends here

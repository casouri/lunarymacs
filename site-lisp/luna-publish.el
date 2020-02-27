;;; luna-publish.el --- Publish blog      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-f)
(require 'subr-x)
;; remove this and ‘org-html-home/up-format’ won’t work right
(require 'ox-html)

(defun luna-publish-html-export (dir &optional force)
  "Export index.org to index.html in DIR if the latter is older.
If FORCE is non-nil, only export when org file is newer than html file."
  (let ((org-file (expand-file-name "index.org" dir))
        (html-file (expand-file-name "index.html" dir)))
    (when (and (file-exists-p org-file)
               (or force (file-newer-than-file-p org-file html-file)))
      (luna-f-with-file org-file
        (org-mode)
        (let ((org-html-head-include-scripts nil)
              (org-export-use-babel nil)
              ;; for relative links in org file
              (default-directory dir)
              (org-export-coding-system org-html-coding-system))
          (org-export-to-file 'html html-file))))))

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
                             (luna-f-with-file org-file
                               (org-element-map
                                   (org-element-parse-buffer)
                                   'plain-text #'identity
                                   nil t)))
                            "No intro."))
                 (html (luna-f-with-file org-file
                         (let ((org-html-style-default "")
                               (org-html-head "")
                               (org-html-head-extra "")
                               (org-html-postamble nil)
                               (org-html-head-include-scripts nil)
                               (org-export-with-toc nil)
                               (org-export-use-babel nil)
                               (org-html-htmlize-output-type nil))
                           ;; org-export-as doesn’t seem to respect above settings
                           ;; body only
                           (org-export-to-buffer 'html (current-buffer)
                             nil nil nil t)
                           (buffer-substring-no-properties
                            1 (1+ (buffer-size))))))
                 (env (luna-f-with-file org-file
                        (org-export-get-environment)))
                 (date (let ((time (cadar (plist-get env :date))))
                         (format-time-string
                          "%a, %d %b %Y %H:%M:%S %z"
                          (encode-time
                           0
                           (or (plist-get time :minute-start) 0)
                           (or (plist-get time :hour-start)   0)
                           (or (plist-get time :day-start)    0)
                           (or (plist-get time :month-start)  0)
                           (or (plist-get time :year-start)   0)))))
                 (title (or (car (plist-get env :title))
                            "No title"))
                 (link (url-encode-url link)))
            (luna-f-with-write-file rss-file
              (insert (string-join
                       (list "<item>"
                             (format "<title>%s</title>" title)
                             (format "<link>%s</link>" link)
                             (format "<guid>%s</guid>" link)
                             ;; (format "<description>%s</description>" intro)
                             (format "<description><![CDATA[%s]]></description>\n" html)
                             (format "<pubDate>%s</pubDate>" date)
                             "</item>\n")
                       "\n"))
              (buffer-substring-no-properties (point-min) (point-max))))
        ;; if not newer
        (luna-f-content rss-file)))))

(provide 'luna-publish)

;;; luna-publish.el ends here

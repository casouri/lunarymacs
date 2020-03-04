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
(require 'rss-export)

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

(defun luna-publish-rss-export (link category-list dir root &optional force)
  "Export index.html to css-item.xml in DIR if the latter is older.
If FORCE is non-nil, only export when org file is newer than html file.

LINK is the web link for the post in dir.
DIR is the absolute path to the directory containing the post (index.org).
ROOT is the root directory of the site, where the rss file resides.
Categories in CATEGORY-LIST are strings."
  (let ((org-file (expand-file-name "index.org" dir))
        (rss-file (expand-file-name "rss-item.xml" dir)))
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
                 (html (with-temp-buffer
                         (let ((buf (current-buffer)))
                           (luna-f-with-file org-file
                             (let ((org-export-use-babel nil)
                                   ;; construct the relative path
                                   ;; from root to dir for relative
                                   ;; links in the post
                                   (path (concat "/"
                                                 (luna-f-subtract root
                                                                  dir))))
                               ;; org-export-as doesn’t seem to respect
                               ;; above settings.
                               ;; use my rss-export backend
                               (org-export-to-buffer 'rss-html buf
                                 nil nil nil t
                                 `(:html-style-default
                                   ""
                                   :html-head ""
                                   :html-head-extra ""
                                   :html-postamble nil
                                   :html-head-include-scripts ""
                                   :with-toc nil
                                   :html-htmlize-output-type nil
                                   :rss-html-relative-dir ,path)))))
                         (buffer-string)))
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
              (buffer-string)))
        ;; if not newer
        (luna-f-content rss-file)))))

(provide 'luna-publish)

;;; luna-publish.el ends here

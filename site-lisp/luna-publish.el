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

(defun luna-publish-html-export (dir option-list &optional force before-export-fn)
  "Export index.org to index.html in DIR if the latter is older.
If FORCE is non-nil, only export when org file is newer than html file.
OPTION-LIST is passed to export function.

Run hooks in BEFORE-EXPORT-FN before export in the temp buffer.

Set use-babel and include-scripts to nil when export."
  (let ((org-file (expand-file-name "index.org" dir))
        (html-file (expand-file-name "index.html" dir)))
    (when (and (file-exists-p org-file)
               (or force (file-newer-than-file-p org-file html-file)))
      (luna-f-with-file org-file
        (org-mode)
        (org-macro-replace-all (org-macro-initialize-templates))
        (let ((org-export-use-babel nil)
              ;; for relative links in org file
              (default-directory dir)
              (org-export-coding-system org-html-coding-system))
          (run-hooks before-export-fn)
          (luna-publish-populate-header-id)
          (org-export-to-file 'cjk-html html-file nil nil nil nil
                              (append '(:html-head-include-scripts nil)
                                      option-list)))))))

(defun luna-publish-populate-header-id ()
  "Add CUSTOM_ID property to each header in current buffer."
  (let (id-list)
    (cl-labels ((get-id ()
                        (let ((id (url-encode-url
                                   (replace-regexp-in-string
                                    " " "-"
                                    (org-get-heading t t t t))))
                              (dup-counter 1))
                          (while (member id id-list)
                            (setq id (format "%s-%d" id dup-counter))
                            (cl-incf dup-counter))
                          (push id id-list)
                          id)))
      (org-map-entries
       (lambda ()
         (org-entry-put (point) "CUSTOM_ID" (get-id)))))))

;;; RSS

(defun luna-publish-rss-export
    (link category-list path root &optional force)
  "Export index.html to css-item.xml in PATH if the latter is older.
If FORCE is nil, only export when org file is newer than html file.

LINK is the web link for the post in path.
PATH is the absolute path to the directory containing the post (index.org).
ROOT is the root directory of the site, where the rss file resides.
Categories in CATEGORY-LIST are strings."
  (ignore category-list)
  (let ((org-file (expand-file-name "index.org" path))
        (rss-file (expand-file-name "rss-item.xml" path)))
    (when (file-exists-p org-file)
      (if (or force ; force export
              (not (file-exists-p rss-file)) ; rss doesn’t exist
              (file-newer-than-file-p org-file rss-file)) ; org newer
          (let* ((html
                  (with-temp-buffer
                    (let ((buf (current-buffer)))
                      (luna-f-with-file org-file
                        (let ((org-export-use-babel nil)
                              (default-directory path))
                          ;; org-export-as doesn’t seem to respect
                          ;; above settings.
                          (org-mode)
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
                              :rss-html-relative-dir ,path
                              :rss-html-root-dir ,root)))))
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
                 (title (let ((title (car (plist-get env :title))))
                          (pcase title
                            (`(macro . ,_)
                             (org-macro-expand
                              title (org-macro-initialize-templates)))
                            ('nil "Untitled")
                            (_ title))))
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

(defun luna-publish-rss
    (out-file post-list template root host &optional force)
  "Export RSS to OUT-FILE.

POST-LIST is a list of plists. Each plist contains the information
of a post. Each plist should have the form:

    (:date DATE :path PATH)

DATE should be comparable by `time-less-p'.
PATH is the absolute path to the directory containing the Org file.
You can get such a list from `luna-publish-post-info'.


ROOT is the root directory of the static site.
HOST is the root url of the website.

TEMPLATE is the RSS template. See `luna-blog-note-rss-template'.

Normally only export outdated RSS, if FORCE non-nil, always export."
  (with-temp-file out-file
    (insert
     (format
      template
      (format-time-string "%a, %d %b %Y %H:%M:%S %z")
      (string-join
       (mapcar (lambda (post)
                 (let* ((path (plist-get post :path))
                        (link (concat host (luna-f-subtract root path))))
                   (luna-publish-rss-export link nil path root force)))
               post-list))))))

;;; Post info

(defun luna-publish-post-info (dir-list)
  "Return a list of info plists for each post.

DIR-LIST is a list of absolute paths to post directory.

Each plist is of form

    (:title TITLE :date DATE :tags TAGS :path PATH).

TAGS is a colon-separated string.
PATH is the absolute path of each post directory.

We look at these environment variables:

    #+TITLE:
    #+DATE: (org timestamp)
    #+TAGS: (space separated)
    #+HIDE: (true or false)

The list is sorted by date, hidden posts are ignored."
  (let (header-list)
    (dolist (dir dir-list)
      (let ((org-file (luna-f-join dir "index.org"))
            ;; Our custom options.
            (org-export-options-alist
             (append '((:tags . ("TAGS" "tags" "" space))
                       (:hide . ("HIDE" "hide" "" nil)))
                     org-export-options-alist)))
        (luna-f-with-file org-file
          ;; Some document level information.
          (let* ((env (org-export-get-environment))
                 (tag-list (split-string (plist-get env :tags)))
                 (title (plist-get env :title))
                 (date (plist-get env :date))
                 (hide (plist-get env :hide)))
            ;; Collect those information.
            (unless (equal hide "true")
              (push (list
                     :title (car title)
                     :date (org-timestamp-to-time (car date))
                     :tags (concat ":" (string-join tag-list ":") ":")
                     :path dir)
                    header-list))))))
    (seq-sort-by (lambda (x) (plist-get x :date))
                 ;; greater than
                 (lambda (a b) (time-less-p b a))
                 header-list)))

(provide 'luna-publish)

;;; luna-publish.el ends here

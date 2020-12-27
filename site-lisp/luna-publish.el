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


;;; Post

(defun luna-publish-dir (dir project-info &optional force)
  "Export file in DIR to HTML if the file is newer than the HTML file.
If FORCE non-nil, export regardless. PROJECT-INFO carries project
options that can affect Org export."
  (let ((org-file (expand-file-name "index.org" dir))
        (html-file (expand-file-name "index.html" dir)))
    (when (and (file-exists-p org-file)
               (or force (file-newer-than-file-p org-file html-file)))
      (luna-f-with-file org-file
        (org-mode)
        ;; For relative links in org file.
        (let ((default-directory dir))
          (org-export-to-file 'post html-file nil nil nil nil
                              project-info))))))

;;; RSS

(defun luna-publish-rss-export (path info-plist &optional force)
  "Export index.org to css-item.xml in PATH if the former is newer.
If FORCE is non-nil, export regardless.

INFO-PLIST should include

    (:blog-rss-link LINK :blog-site-root ROOT)

LINK is the url link for the post in path. PATH is the absolute
path to the directory containing the post (index.org). ROOT is
the root directory of the site, where the rss file resides."
  (let* ((org-file (expand-file-name "index.org" path))
         (rss-file (expand-file-name "rss-item.xml" path))
         (default-directory path))
    (when (file-exists-p org-file)
      (when (or force ; force export
                (not (file-exists-p rss-file)) ; rss doesn’t exist
                (file-newer-than-file-p org-file rss-file)) ; org newer
        (luna-f-with-file org-file
          (org-mode)
          (org-export-to-file 'rss-item rss-file nil nil nil nil
                              ;; This list contains :blog-rss-link and
                              ;; :blog-site-root.
                              info-plist)))
      ;; Return file content.
      (luna-f-content rss-file))))

(defvar luna-publish-rss-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>%s</title>
    <link>%s</link>
    <description>%s</description>
    <lastBuildDate>%s</lastBuildDate>
%s
  </channel>
</rss>"
  "Used for RSS export.
First %s substitutes to :blog-rss-title,
Second %s substitutes to :blog-url-base
Third %s substitutes to :blog-rss-desc.")

(defun luna-publish-rss (out-file post-list project-info &optional force)
  "Export RSS to OUT-FILE.

POST-LIST is a list of plists. Each plist contains the information
of a post. Each plist should have the form:

    (:date DATE :path PATH)

DATE should be comparable by `time-less-p'.
PATH is the absolute path to the directory containing the Org file.
You can get such a list from `luna-publish-post-info'.

PROJECT-INFO should contain

    (:blog-rss-title TITLE :blog-url-base HOST 
     :blog-rss-desc DESC   :blog-site-root ROOT)

See `luna-publish-rss-template' for the purpose of TITLE, HOST
and DESC. ROOT is passed to rss-item backend to produce absolute
url.

Normally only export outdated RSS, if FORCE non-nil, always export."
  (with-temp-file out-file
    (insert
     (format
      luna-publish-rss-template
      (plist-get project-info :blog-rss-title)
      (plist-get project-info :blog-url-base)
      (plist-get project-info :blog-rss-desc)
      (format-time-string "%a, %d %b %Y %H:%M:%S %z")
      (string-join
       (mapcar
        (lambda (post)
          ;; We calculate the url for each rss post.
          (let* ((path (plist-get post :path))
                 (host (plist-get project-info :blog-url-base))
                 (base (plist-get project-info :blog-site-base))
                 (link (concat host (luna-f-subtract base path))))
            (luna-publish-rss-export
             path (append `(:blog-rss-link ,link) project-info) force)))
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

;;; Project info

(defvar luna-publish-project-alist nil
  "An alist of (PROJECT-NAME . PROJECT-DEFINITION).")

(defun luna-publish-post-info-list (project-info)
  "Return a list of post-info plists depending on PROJECT-INFO."
  (luna-publish-post-info
   (funcall (plist-get project-info :blog-dir-list-fn)
            project-info)))

(defun luna-publish (&optional force)
  "Publish blog defined by PROJECT-INFO.
If FORCE non-nil, force publish the whole blog. When called
interactively, prefix argument indicates force publish."
  (interactive "p")
  (let* ((force (if (eq force 1) nil force))
         (project-info (alist-get (intern
                                   (completing-read
                                    "Project: "
                                    luna-publish-project-alist))
                                  luna-publish-project-alist))
         (pre-process-fn (plist-get project-info :blog-preprocess)))
    ;; Pre-process
    (when pre-process-fn
      (funcall pre-process-fn project-info))
    (let ((post-list (luna-publish-post-info-list project-info))
          (site-base (plist-get project-info :blog-site-base)))
      ;; Posts.
      (dolist (post post-list)
        (luna-publish-dir (plist-get post :path) project-info force))
      ;; Index page.
      (luna-publish-dir site-base project-info t)
      ;; RSS
      (luna-publish-rss (luna-f-join site-base "rss.xml")
                        post-list project-info force))))

(provide 'luna-publish)

;;; luna-publish.el ends here

;;; blogs.el --- Config for each blog site      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-publish)
(require 'cl-lib)
(require 'subr-x)

;;; Common

(defvar luna-org-html-postamble-format
  '(("en" "<p class=\"author\">Written by %a <%e></p>
<p class=\"first-publish\">First Published on %d</p>
<p class-\"last-modified\">Last modified on %C</p>")))

(defvar luna-org-html-home/up-format
  "<div id=\"org-div-home-and-up-index-page\">
<div>
<a accesskey=\"h\" href=\"%s\"> UP </a> |
<a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>
<div>
<a href=\"../rss.xml\"> RSS </a> |
<a href=\"https://github.com/casouri/casouri.github.io\"> Source </a> |
<a href=\"https://creativecommons.org/licenses/by-sa/4.0/\"> License </a>
</div>
</div>")

(defvar luna-org-html-home/up-format-for-note-index
  "<div id=\"org-div-home-and-up-index-page\">
<div>
<a accesskey=\"h\" href=\"%s\"> UP </a> |
<a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>
<div>
<a href=\"./rss.xml\"> RSS </a> |
<a href=\"https://github.com/casouri/casouri.github.io\"> Source </a> |
<a href=\"https://creativecommons.org/licenses/by-sa/4.0/\"> License </a>
</div>
</div>"
  "RSS url is different.")

;;; Note

(defvar luna-publish-note-dir "~/p/casouri/note/"
  "Make sure the path follow the convention of adding slash and the end of directory.")

(defun luna-blog-get-note-date (post-file-name)
  (with-current-buffer (find-file-noselect post-file-name)
    (plist-get (car (cdr (car (plist-get (org-export-get-environment) :date)))) :raw-value)))

(defun luna-blog-get-note-content (post-file-name)
  (with-current-buffer (find-file-noselect post-file-name)
    (buffer-string)))

(defun luna-publish-note (&optional force)
  "Publish my blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  ;; so the syntax color is good for light background
  (save-excursion
    (luna-publish-with-theme 'doom-one-light
      (luna-publish-with-tmp-buffers
       (let ((org-html-postamble-format luna-org-html-postamble-format)
             (org-html-postamble t)
             (org-html-home/up-format luna-org-html-home/up-format))
         ;; export posts
         ;; for each year
         (dolist (dir (luna-f-list-directory luna-publish-note-dir t))
           ;; for each post
           (dolist (post-dir (luna-f-list-directory dir t))
             ;; publish each post
             (luna-publish-html-export post-dir force)))

         ;; publish index page
         (let ((org-html-postamble nil)
               (org-html-home/up-format luna-org-html-home/up-format-for-note-index))
           (luna-publish-html-export luna-publish-note-dir force)))))))

(defun luna-new-note-blog (title)
  "Make a new blog post with TITLE."
  (interactive "MTitle: ")
  (let* ((year (substring (current-time-string) 20))
         (dir-file-name (downcase (replace-regexp-in-string " " "-" title)))
         (dir-path (luna-f-join luna-publish-note-dir
                                year
                                dir-file-name))
         (file-path (luna-f-join dir-path
                                 "index.org")))
    ;; create post’s dir and org file,
    ;; insert basic information
    (mkdir dir-path)
    (find-file file-path)
    (insert (format "#+SETUPFILE: ../../setup.org
#+TITLE: %s
#+DATE:
"
                    title))
    (save-buffer)
    ;; edit index page, add new post’s header to it.
    (let ((post-header-info (format "{{{post(%s,%s/%s/)}}}"
                                    title
                                    year
                                    dir-file-name)))
      (find-file (luna-f-join luna-publish-note-dir "index.org"))
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote "# post-insert-anchor") nil t)
          (insert (concat "\n\n" post-header-info))
        (kill-new post-header-info)
        (message "Cannot find post header anchor, please yank header information manually")))))

(defvar luna-note-rss-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>Notes</title>
    <link>http://archive.casouri.co.uk/note</link>
    <description>RSS feed for my notes</description>
    <lastBuildDate>%s</lastBuildDate>
%s
  </channel>
</rss>")

(defun luna-note-export-rss (force)
  "Export RSS for current buffer.
If FORCE non-nil, re-export every post."
  (interactive "P")
  (let* ((default-directory luna-publish-note-dir)
         (auto-save-interval 999999999999)
         (inhibit-file-name-handlers t)
         (undo-inhibit-record-point t)
         (str (with-current-buffer (find-file (luna-f-join luna-publish-note-dir "index.org"))
                (buffer-string)))
         (rss (with-temp-buffer
                (insert str)
                (org-mode)
                (org-macro-initialize-templates)
                (org-macro-replace-all org-macro-templates)
                (string-join
                 (org-element-map
                     (org-element-parse-buffer)
                     'headline (lambda (hl)
                                 (luna-publish-rss-export
                                  (org-element-property :RSS_LINK hl)
                                  nil
                                  (message (org-element-property :RSS_DIR hl))
                                  force)))))))
    (with-current-buffer (find-file "./rss.xml")
      (erase-buffer)
      (insert (format luna-note-rss-template
                      (format-time-string "%a, %d %b %Y %H:%M:%S %z")
                      rss))
      (save-buffer))))

;;; Rock/day

(defvar luna-publish-rock/day-dir "~/p/casouri/rock/day/"
  "Make sure the path follow the convention of adding slash and the end of directory.")

(defun luna-blog-rock/day-generate-titles ()
  "Generate titles for index page."
  (let ((day-num (length (luna-f-directory-files (luna-f-join luna-publish-rock/day-dir "src") t))))
    (string-join (cl-loop for day-idx downfrom day-num to 1
                          collect (format "* [[./day-%d/index.html][Day %d]]" day-idx day-idx))
                 "\n")))

(defun luna-publish-rock/day (&optional force)
  "Publish rock/day blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  ;; so the syntax color is good for light background
  (save-excursion
    (luna-publish-with-theme 'doom-one-light
      (luna-publish-with-tmp-buffers
       ;; publish each post
       (let ((org-html-postamble-format luna-org-html-postamble-format)
             (org-html-postamble t)
             (day-num (length (luna-f-directory-files (luna-f-join luna-publish-rock/day-dir "src") t))))
         (defvar --luna-blog-rock-day-- nil)
         (cl-loop for day-idx from 1 to day-num
                  do (let* ((file-path (luna-f-join luna-publish-rock/day-dir "src" (format "day-%d.org" day-idx)))
                            ;; (eval-env `((luna-blog-rock-day . ,day-idx))) ; used by macros in org files
                            (html-dir (luna-f-join luna-publish-rock/day-dir
                                                   (format "day-%d" day-idx)))
                            (html-path (luna-f-join html-dir "index.html")))
                       (unless (file-exists-p html-dir)
                         (mkdir html-dir))
                       (with-current-buffer (find-file file-path)
                         (when (or force (file-newer-than-file-p file-path html-path))
                           (setq --luna-blog-rock-day-- day-idx)
                           (org-export-to-file 'html html-path))))))
       ;; publish index page
       (let ((org-html-postamble nil))
         (luna-publish-html-export luna-publish-rock/day-dir force))))))

(defun luna-new-rock/day ()
  "Make a new blog post of rock/day of DAY."
  (interactive)
  (let ((day (1+ (length (luna-f-directory-files (luna-f-join luna-publish-rock/day-dir "src"))))))
    (mkdir (format "~/p/casouri/rock/day/day-%d" day))
    (find-file (format "~/p/casouri/rock/day/src/day-%d.org" day))
    (goto-char (point-min))
    (insert "#+SETUPFILE: ../setup.org
#+TITLE: {{{day_title}}}
#+DATE:

{{{day_link}}}

{{{img()}}}

* - *

#+BEGIN_QUOTE
#+END_QUOTE
")
    (save-buffer)
    (kill-new (format "* [[./day-%d/index.html][Day %d]]" day day))
    (find-file "~/p/casouri/rock/day/index.org")
    (if (re-search-forward "# day-anchor" nil t)
        (progn (end-of-line)
               (insert (format "\n* [[./day-%d/index.html][Day %d]]" day day)))
      (message "Cannot find anchor, please paste manually")
      )))

;;; blogs.el ends here

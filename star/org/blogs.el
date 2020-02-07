;;; blogs.el --- Config for each blog site      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-publish)
(require 'luna-f)
(require 'cl-lib)
(require 'subr-x)

;;; Common

(defvar luna-org-html-footnote-format
  "<sup>%s</sup>")

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

(defun luna-publish-note-process ()
  "Run another Emacs and publish blog."
  (interactive)
  (start-process "Publish Notes"
                 "*publish*"
                 "~/bin/emacs"
                 "-q"
                 "-l" "~/.emacs.d/star/org/blog-init.el"
                 "-f" "luna-publish-note"
                 "-f" "save-buffers-kill-terminal"))

(defun luna-publish-note (&optional force)
  "Publish my blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  ;; so the syntax color is good for light background
  (save-excursion
    (let ((org-html-head-include-scripts nil)
          (org-export-use-babel nil))
      ;; export posts
      ;; for each year
      (dolist (dir (luna-f-list-directory luna-publish-note-dir t))
        ;; for each post
        (dolist (post-dir (luna-f-list-directory dir t))
          ;; publish each post
          ;; for some reason the let bindings only work here
          ;; move it up there and it doesn’t work anymore...
          (let ((org-html-postamble-format luna-org-html-postamble-format)
                (org-html-postamble t)
                (org-html-home/up-format luna-org-html-home/up-format)
                (org-html-footnote-format luna-org-html-footnote-format))
            (luna-publish-html-export post-dir force))))
      ;; publish index page
      (let ((org-html-postamble nil)
            (org-html-home/up-format
              luna-org-html-home/up-format-for-note-index))
        ;; like rock/day, we are automatically generating headers now
        ;; force update every time
        (luna-publish-html-export luna-publish-note-dir t)))))

(defun luna-new-note-blog (title)
  "Make a new blog post with TITLE."
  (interactive "MTitle: ")
  (let* ((year (substring (current-time-string) 20))
         (dir-file-name (downcase (replace-regexp-in-string " " "-" title)))
         (year-path (luna-f-join luna-publish-note-dir year))
         (dir-path (luna-f-join year-path
                                dir-file-name))
         (file-path (luna-f-join dir-path
                                 "index.org")))
    ;; create post’s dir and org file,
    ;; insert basic information
    (unless (file-exists-p year-path)
      (mkdir year-path))
    (mkdir dir-path)
    (find-file file-path)
    (insert (format "#+SETUPFILE: ../../setup.org
#+TITLE: %s
#+DATE:
#+TAGS:
"
                    title))
    (save-buffer)))

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
         (auto-save-default nil)
         (undo-inhibit-record-point t)
         (rss (luna-f-with-file (luna-f-join luna-publish-note-dir "index.org")
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
                                  (org-element-property :RSS_DIR hl)
                                  force)))))))
    (with-temp-file (luna-f-join luna-publish-note-dir "rss.xml")
      (insert (format luna-note-rss-template
                      (format-time-string "%a, %d %b %Y %H:%M:%S %z")
                      rss)))))


(defun luna-note-export-headers ()
  "Generate org headers for each post."
  (let (header-list)
    (dolist (year-dir (luna-f-list-directory luna-publish-note-dir t))
      (dolist (post-dir (luna-f-list-directory year-dir t))
        (let ((org-file (luna-f-join post-dir "index.org"))
              (org-export-options-alist (append '((:tags . ("TAGS" "tags" "" space))
                                                  (:hide . ("HIDE" "hide" "" nil)))
                                                org-export-options-alist)))
          (luna-f-with-file org-file
            (let* ((env (org-export-get-environment))
                   (tag-list (split-string (plist-get env :tags)))
                   (title (plist-get env :title))
                   (date (plist-get env :date))
                   (hide (plist-get env :hide)))
              (unless (equal hide "true")
                (push (list :title (car title)
                            :date date
                            :tags (concat ":" (string-join tag-list ":") ":")
                            :path (format "%s/%s"
                                          (file-name-base year-dir)
                                          (file-name-base post-dir)))
                      header-list)))))))
    (substring-no-properties
     (string-join
      (mapcar (lambda (header)
                (format "* [[./%s][%s]] %s\n :PROPERTIES:\n :RSS_LINK: %s\n :RSS_DIR: %s\n :END:\n\n"
                        (concat (plist-get header :path) "/index.html")
                        (plist-get header :title)
                        (plist-get header :tags)
                        (concat "https://archive.casouri.cat/note/" (plist-get header :path))
                        (luna-f-join luna-publish-note-dir
                                     (plist-get header :path))))
              (seq-sort-by (lambda (x) (plist-get x :date))
                           #'org-time>=
                           header-list))))))

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
                         (html-path (luna-f-join html-dir "index.html"))
                         (org-html-head-include-scripts nil)
                         (org-export-with-toc nil)
                         (org-export-use-babel nil))
                    (unless (file-exists-p html-dir)
                      (mkdir html-dir))
                    (luna-f-with-file (find-file file-path)
                      (when (or force (file-newer-than-file-p file-path html-path))
                        (org-mode)
                        (setq --luna-blog-rock-day-- day-idx)
                        (org-export-to-file 'html html-path))))))
    ;; publish index page
    (let ((org-html-postamble nil))
      ;; always force because the index is automatically generated now
      ;; so there normally won’t be changes
      (luna-publish-html-export luna-publish-rock/day-dir t))))

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

#+BEGIN_EXAMPLE
#+END_EXAMPLE
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

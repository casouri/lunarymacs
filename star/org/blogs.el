;;; blogs.el --- Config for each blog site      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-publish)

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
<a href=\"../index.xml\"> RSS </a> |
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
<a href=\"./index.xml\"> RSS </a> |
<a href=\"https://github.com/casouri/casouri.github.io\"> Source </a> |
<a href=\"https://creativecommons.org/licenses/by-sa/4.0/\"> License </a>
</div>
</div>"
  "RSS url is different.")

;;; Note

(defvar luna-publish-note-dir "~/p/casouri/note/"
  "Make sure the path follow the convention of adding slash and the end of directory.")

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
         (let ((org-html-postamble-format luna-org-html-postamble-format)
               (org-html-postamble t)
               (org-html-home/up-format luna-org-html-home/up-format-for-note-index))
           (luna-publish-html-export luna-publish-note-dir force))

         ;; export RSS
         (require 'ox-rss)
         (let ((buffer (find-file (luna-f-join luna-publish-note-dir "index.org"))))
           (with-current-buffer buffer
             (org-rss-export-to-rss))
           (kill-buffer buffer)))))))

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

;;; Rock/day

(defvar luna-publish-rock/day-dir "~/p/casouri/rock/day/"
  "Make sure the path follow the convention of adding slash and the end of directory.")

(defun luna-publish-rock/day (&optional force)
  "Publish rock/day blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  ;; so the syntax color is good for light background
  (save-excursion
    (luna-publish-with-theme 'doom-one-light
      (luna-publish-with-tmp-buffers
       (let ((environment '((org-html-postamble-format luna-org-html-postamble-format)
                            (org-html-postamble t))))
         (dolist (post-dir (luna-f-list-directory luna-publish-rock/day-dir t))
           ;; publish each post
           (luna-publish-html-export post-dir environment force))
         ;; publish index page
         (luna-publish-html-export luna-publish-rock/day-dir environment force))))))

(defun luna-new-rock/day (day)
  "Make a new blog post of rock/day of DAY."
  (interactive "n")
  (mkdir (format "~/p/casouri/rock/day/day-%d" day))
  (find-file (format "~/p/casouri/rock/day/day-%d/index.org" day))
  (insert (format "#+SETUPFILE: ../setup.org
#+TITLE: Day %d
#+DATE:

#+HTML: <div style=\"display: flex; justify-content: space-between;\"><a href=\"../day-%d/index.html\"><< Yesterday <<</a><a href=\"../day-%d/index.html\">>> Tommorrow >></a></div>


[[../album/]]

* - *

#+BEGIN_SRC
#+END_SRC
" day (1- day) (1+ day)))
  (save-buffer)
  (kill-new (format "- [[./day-%d/index.html][Day %d]]" day day))
  (find-file "~/p/casouri/rock/day/index.org"))

;;; blogs.el ends here

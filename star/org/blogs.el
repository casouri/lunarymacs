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

(defvar luna-blog-root (expand-file-name "~/p/casouri/")
  "Make sure there is a final slash.")

(defvar luna-blog-url "https://archive.casouri.cat/"
  "Make sure there is a final slash.")

(defvar luna-blog-footnote-format
  "<sup>%s</sup>")

(defvar luna-blog-postamble-format
  '(("en" "<p class=\"author\">Written by %a</p>
<p class=\"first-publish\">First Published on %d</p>
<p class-\"last-modified\">Last modified on %C</p>
<p>Send your comment to 
<a href=\"mailto:archive.casouri.cat@gmail.com\">
archive.casouri.cat@gmail.com</a></p>")))

(defvar luna-blog-home/up-format
  "<div id=\"org-div-home-and-up\">
<div>
<a accesskey=\"h\" href=\"%s\"> UP </a> |
<a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>
<div>
<a href=\"../../rss.xml\"> RSS </a> |
<a href=\"https://github.com/casouri/casouri.github.io\"> Source </a> |
<a href=\"https://creativecommons.org/licenses/by-sa/4.0/\"> License </a>
</div>
</div>")

(defvar luna-blog-index-home/up-format
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

(defvar luna-blog-note-dir (concat luna-blog-root "note/")
  "Make sure there is a final slash.")

(defvar luna-blog-note-url (concat luna-blog-url "note/")
  "Make sure there is a final slash.")

;;;; Backstage


(defun luna-note-get-post-info ()
  "Return a list of info plists for each post.
Return value see `luna-publish-post-info'."
  (let (dir-list)
    (dolist (year-dir (luna-f-list-directory luna-blog-note-dir t))
      (dolist (post-dir (luna-f-list-directory year-dir t))
        (push post-dir dir-list)))
    (luna-publish-post-info dir-list)))

;;;; Headers

(defun luna-note-export-headers ()
  "Generate org headers for each post.
This is used as a local macro in index page of the note blog."
  (let ((header-list (luna-note-get-post-info)))
    ;; In each header we have (:title :date :tags :path).
    ;; Now we have a list of document-info, format them into headers.
    (substring-no-properties
     (string-join
      (mapcar
       (lambda (header)
         (let* ((abs-path (plist-get header :path))
                (relative-path (luna-f-subtract
                                luna-blog-note-dir abs-path)))
           (format "* [[./%s][%s]] %s\n\n"
                   (concat relative-path "/index.html")
                   (plist-get header :title)
                   (plist-get header :tags))))
       header-list)))))

;;;; RSS

(defvar luna-blog-note-rss-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>Notes</title>
    <link>http://archive.casouri.co.uk/note</link>
    <description>RSS feed for my notes</description>
    <lastBuildDate>%s</lastBuildDate>
%s
  </channel>
</rss>")

(defun luna-note-export-rss (&optional force)
  "Export RSS for notes.
If FORCE non-nil, re-export every post."
  (interactive "P")
  (let* ((post-list (luna-note-get-post-info)))
    (luna-publish-rss (luna-f-join luna-blog-note-dir "rss.xml")
                      post-list
                      luna-blog-note-rss-template
                      luna-blog-root
                      luna-blog-url
                      force)))

;;;; Commands

(defun luna-publish-note-process (arg)
  "Run another Emacs and publish blog.
Use prefix argument (ARG) to force publish."
  (interactive "p")
  (if (eq arg 4)
      (start-process "Publish Notes"
                     "*publish*"
                     "emacs"
                     ;; "--batch"
                     "-q"
                     "-l" "~/.emacs.d/star/org/blog-init.el"
                     "--eval" "(luna-publish-note t)"
                     "-f" "save-buffers-kill-terminal")
    (start-process "Publish Notes"
                   "*publish*"
                   "emacs"
                   ;; "--batch"
                   "-q"
                   "-l" "~/.emacs.d/star/org/blog-init.el"
                   "-f" "luna-publish-note"
                   "-f" "save-buffers-kill-terminal")))

(defun luna-publish-note (&optional force)
  "Publish my blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  ;; so the syntax color is good for light background
  (save-excursion
    ;; export posts
    ;; for each year
    (dolist (dir (luna-f-list-directory luna-blog-note-dir t))
      ;; for each post
      (dolist (post-dir (luna-f-list-directory dir t))
        ;; publish each post
        ;; for some reason the let bindings only work here
        ;; move it up there and it doesn’t work anymore...
        (let ((option-list `(:html-postamble-format
                             ,luna-blog-postamble-format
                             :html-postamble t
                             :html-home/up-format
                             ,luna-blog-home/up-format
                             :html-footnote-format
                             ,luna-blog-footnote-format)))
          (luna-publish-html-export post-dir option-list force))))
    ;; publish index page
    (let ((option-list `(:html-postamble
                         nil
                         :html-home/up-format
                         ,luna-blog-index-home/up-format)))
      ;; like rock/day, we are automatically generating headers now,
      ;; force update every time
      (luna-publish-html-export luna-blog-note-dir option-list t))
    ;; rss
    (luna-note-export-rss)))

(defun luna-new-note-blog (title)
  "Make a new blog post with TITLE."
  (interactive "MTitle: ")
  (let* ((year (substring (current-time-string) 20))
         (dir-file-name (downcase (replace-regexp-in-string " " "-" title)))
         (year-path (luna-f-join luna-blog-note-dir year))
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
" title))
    (save-buffer)))

;;; Rock/day

(defvar luna-blog-rock/day-dir (concat luna-blog-root "rock/day/")
  "Make sure to add a slash at the end of directory.")

(defun luna-blog-rock/day-generate-titles ()
  "Generate titles for index page."
  (let ((day-num (length (luna-f-directory-files
                          (luna-f-join luna-blog-rock/day-dir "src")
                          t))))
    (string-join
     (cl-loop for day-idx downfrom day-num to 1
              collect (format "* [[./day-%d/index.html][%d]]"
                              day-idx day-idx))
     "\n")))

;;;; RSS

(defvar luna-blog-rock/day-rss-template
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>余日摇滚</title>
    <link>http://archive.casouri.co.uk/rock/day</link>
    <description>音乐推荐☆DAZE☆</description>
    <lastBuildDate>%s</lastBuildDate>
%s
  </channel>
</rss>")

(defun luna-blog-rock/day-export-rss (&optional force)
  "Export RSS for rock/day.
FORCE as usual."
  (interactive)
  (let* ((dir-list (cl-remove-if-not
                    (lambda (d) (string-match-p "day" (file-name-base d)))
                    (luna-f-list-directory luna-blog-rock/day-dir t)))
         (post-list (luna-publish-post-info dir-list)))
    (luna-publish-rss (luna-f-join luna-blog-rock/day-dir "rss.xml")
                      post-list
                      luna-blog-rock/day-rss-template
                      luna-blog-root
                      luna-blog-url
                      force)))

;;;; Commands

(defun luna-publish-rock/day (&optional force)
  "Publish rock/day blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  (save-excursion
    ;; publish each post
    (let ((org-html-postamble-format luna-blog-postamble-format)
          (org-html-postamble t)
          (day-num (length (luna-f-directory-files
                            (luna-f-join luna-blog-rock/day-dir "src")
                            t))))
      (defvar --luna-blog-rock-day-- nil)
      (cl-loop
       for day-idx from 1 to day-num
       do
       (let* ((file-path (luna-f-join
                          luna-blog-rock/day-dir
                          "src" (format "day-%d.org" day-idx)))
              (html-dir (luna-f-join luna-blog-rock/day-dir
                                     (format "day-%d" day-idx)))
              (html-path (luna-f-join html-dir "index.html"))
              (duplicate-org-path (luna-f-join html-dir "index.org"))
              (org-export-use-babel nil)
              (option-list `(:html-home/up-format
                             ,luna-blog-home/up-format
                             :with-toc
                             nil
                             :html-head-include-scripts nil)))
         (unless (file-exists-p html-dir)
           (mkdir html-dir))
         ;; Export HTML.
         (luna-f-with-file file-path
           ;; set default-directory so the relative link to setupfile works
           (let ((default-directory (luna-f-join
                                     luna-blog-rock/day-dir "src"))
                 (title (format "Day %d" day-idx)))
             (when (or force (file-newer-than-file-p file-path html-path))
               (org-mode)
               (setq --luna-blog-rock-day-- day-idx)
               (org-export-to-file 'html html-path nil nil nil nil
                                   option-list)
               ;; Expand macros and write to index.org for RSS export.
               (goto-char (point-min))
               ;; Title macro is not properly expanded...
               (when (search-forward "{{{day_title}}}" nil t)
                 (replace-match title))
               (org-macro-replace-all (org-macro-initialize-templates))
               (write-file duplicate-org-path)))))))
    ;; publish index page
    (let ((option-list `(:html-home/up-format
                         ,luna-blog-index-home/up-format
                         :html-postamble nil)))
      ;; always force because the index is automatically generated now
      ;; so there normally won’t be changes
      (luna-publish-html-export luna-blog-rock/day-dir option-list t))
    ;; export rss
    (luna-blog-rock/day-export-rss force)))

(defun luna-new-rock/day ()
  "Make a new blog post of rock/day of DAY."
  (interactive)
  (let ((day (1+ (length (luna-f-directory-files
                          (luna-f-join
                           luna-blog-rock/day-dir "src"))))))
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
    (save-buffer)))

(defun luna-open-album-dir ()
  "Open ~/p/casouri/rock/day/album/."
  (interactive)
  (shell-command-to-string (format "open ~/p/casouri/rock/day/album/")))

(defun luna-insert-album ()
  "Insert a album image name."
  (interactive)
  (insert (completing-read
           "Album: "
           (luna-f-directory-files
            (luna-f-join luna-blog-rock/day-dir "album")))))

;;; goldfish

(defvar luna-publish-goldfish-dir "~/p/casouri/goldfish")

(defun luna-publish-goldfish (&optional force)
  "Publish goldfish blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  (save-excursion
    ;; publish each post
    (dolist (file (directory-files luna-publish-goldfish-dir t ".org"))
      (luna-f-with-file file
        (org-mode)
        (let ((org-export-use-babel nil)
              (html-file (luna-f-change-extension file "html"))
              ;; for relative links in org file
              (default-directory luna-publish-goldfish-dir)
              (org-export-coding-system org-html-coding-system)
              (org-html-postamble-format luna-blog-postamble-format)
              (org-html-postamble nil)
              (org-export-use-babel nil)
              (org-html-head-include-scripts nil))
          (if (or force (file-newer-than-file-p file html-file))
              (org-export-to-file 'cjk-html html-file nil nil nil nil)))))))

;;; blogs.el ends here

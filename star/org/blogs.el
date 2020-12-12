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

(defvar luna-blog-note-info
  `(:blog-site-base
    ,(concat luna-blog-root "note/")
    :blog-url-base ,(concat luna-blog-url "note/")
    :blog-rss-title "Notes"
    :blog-rss-desc "RSS feed for my notes"
    :blog-link-source "https://github.com/casouri/casouri.github.io"
    :blog-link-license "https://creativecommons.org/licenses/by-sa/4.0/"
    :blog-dir-list-fn
    (lambda (info)
      (let (dir-list)
        (dolist (year-dir (luna-f-list-directory
                           (plist-get info :blog-site-base) t))
          (dolist (post-dir (luna-f-list-directory year-dir t))
            (push post-dir dir-list)))
        dir-list))))

(defvar luna-blog-rock-info
  `(:blog-site-base
    ,(concat luna-blog-root "rock/day/")
    :blog-url-base ,(concat luna-blog-url "rock/day/")
    :blog-rss-title "余日摇滚"
    :blog-rss-desc "音乐推荐☆DAZE☆"
    :blog-link-source "https://github.com/casouri/casouri.github.io"
    :blog-link-license "https://creativecommons.org/licenses/by-sa/4.0/"
    :blog-dir-list-fn
    (lambda (info)
      (directory-files (plist-get info :blog-site-base) t
                       "day-"))
    :blog-preprocess luna-blog-rock-distribute))

(setq luna-publish-project-alist
      `((note . ,luna-blog-note-info)
        (rock . ,luna-blog-rock-info)))

;;; Notes

;;;; Macros

(defun luna-note-export-headers ()
  "Generate org headers for each post.
This is used as a local macro in index page of the note blog."
  (let ((header-list (luna-publish-post-info-list luna-blog-note-info)))
    ;; In each header we have (:title :date :tags :path).
    ;; Now we have a list of document-info, format them into headers.
    (substring-no-properties
     (string-join
      (mapcar
       (lambda (header)
         (let* ((abs-path (plist-get header :path))
                (relative-path (luna-f-subtract
                                (plist-get luna-blog-note-info
                                           :blog-site-base)
                                abs-path)))
           (format "* [[./%s][%s]] %s\n\n"
                   (concat relative-path "/index.html")
                   (plist-get header :title)
                   (plist-get header :tags))))
       header-list)))))

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
  (interactive)
  (luna-publish luna-blog-note-info force))

(defun luna-new-note-blog (title)
  "Make a new blog post with TITLE."
  (interactive "MTitle: ")
  (let* ((year (substring (current-time-string) 20))
         (dir-file-name
          (downcase (replace-regexp-in-string " " "-" title)))
         (year-path (luna-f-join (plist-get luna-blog-note-info
                                            :blog-site-base)
                                 year))
         (dir-path (luna-f-join year-path
                                dir-file-name))
         (file-path (luna-f-join dir-path
                                 "index.org")))
    ;; Create the post’s dir and org file and insert basic information.
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

;;;; Macros

(defun luna-blog-rock-day-count ()
  (let* ((site-base (plist-get luna-blog-rock-info :blog-site-base)))
    (length (luna-f-directory-files (luna-f-join site-base "src")))))

(defun luna-blog-rock-this-day ()
  "Get the number of day of this buffer file.
The `default-directory' should be `day-xxx'."
  (let ((text (file-name-base (directory-file-name default-directory))))
    (string-match "^day-\\(.+\\)$" text)
    (string-to-number (match-string 1 text))))

(defun luna-blog-rock-generate-titles ()
  "Generate titles for index page."
  (let* ((days-count (luna-blog-rock-day-count)))
    (string-join
     (cl-loop for day-idx downfrom days-count to 1
              collect (format "* [[./day-%d/index.html][%d]]"
                              day-idx day-idx))
     "\n")))

;;;; Commands

(defun luna-blog-rock-distribute (info)
  "Distribute files src/day-x.org to day-x/index.org.
INFO is not used."
  (ignore info)
  (let* ((site-base (plist-get luna-blog-rock-info :blog-site-base))
         (days-count (luna-blog-rock-day-count)))
    (cl-loop
     for day-idx from 1 to days-count
     do (let* ((source (luna-f-join
                        site-base (format "src/day-%d.org" day-idx)))
               (dest-dir (luna-f-join site-base (format "day-%d" day-idx)))
               (dest-path (luna-f-join dest-dir "index.org")))
          ;; Copy src/day-x.org to day-x/index.org.
          (unless (file-exists-p dest-dir)
            (mkdir dest-dir))
          (when (file-newer-than-file-p source dest-path)
            (luna-f-with-file source
              (write-file dest-path)))))))

(defun luna-new-rock ()
  "Make a new blog post of rock/day of DAY."
  (interactive)
  (let ((site-base (plist-get luna-blog-rock-info :blog-site-base))
        (day (1+ (luna-blog-rock-day-count))))
    (mkdir (luna-f-join site-base (format "day-%d" day)))
    (find-file (luna-f-join site-base (format "src/day-%d.org" day)))
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

;;; blogs.el ends here

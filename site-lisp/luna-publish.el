;;; luna-publish.el --- Publish blog      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-f)

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

(defvar luna-publish-note-dir "~/p/casouri/note/"
  "Make sure the path follow the convention of adding slash and the end of directory.")

(defvar luna-publish-rock/day-dir "~/p/casouri/rock/day/"
  "Make sure the path follow the convention of adding slash and the end of directory.")

;;; Backstage

(defun luna-publish--html-export (dir &optional force)
  "Export index.org to index.html in DIR if the latter is older.
If FORCE is non-nil, only export when org file is newer than html file."
  (let ((org-file (expand-file-name "index.org" dir))
        (html-file (expand-file-name "index.html" dir)))
    (when (and (file-exists-p org-file)
               (or force (file-newer-than-file-p org-file html-file)))
      (let ((buffer (find-file org-file)))
        (with-current-buffer buffer
          (org-html-export-to-html))))))

(defmacro luna-publish--with-tmp-buffers (&rest body)
  "Evaluate BODY, but don’t introduce new buffers."
  (let ((old-buffer-list-sym (gensym)))
    `(let ((,old-buffer-list-sym (buffer-list)))
       ,@body
       (mapc (lambda (buf) (unless (member buf ,old-buffer-list-sym) (kill-buffer buf)))
             (buffer-list)))))

(defmacro luna-publish--with-theme (theme &rest body)
  "Use THEME during BODY."
  (declare (indent 1))
  (let ((old-theme-sym (gensym)))
    `(let ((,old-theme-sym luna-current-theme))
       (luna-load-theme ,theme)
       ,@body
       (luna-load-theme ,old-theme-sym))))

;;; Userland

(defun luna-publish-note (&optional force)
  "Publish my blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  ;; so the syntax color is good for light background
  (luna-publish--with-theme 'doom-one-light
    (luna-publish--with-tmp-buffers
     ;; export posts
     (let ((org-html-postamble-format luna-org-html-postamble-format)
           (org-html-postamble t)
           (org-html-home/up-format luna-org-html-home/up-format))
       ;; for each year
       (dolist (dir (luna-f-list-directory luna-publish-note-dir))
         ;; for each post
         (dolist (post-dir (luna-f-list-directory dir))
           ;; publish each post
           (luna-publish--html-export post-dir force)))

       ;; publish index page
       (let ((org-html-postamble-format luna-org-html-postamble-format)
             (org-html-postamble t)
             (org-html-home/up-format luna-org-html-home/up-format-for-note-index))
         (luna-publish--html-export luna-publish-note-dir force))

       ;; export RSS
       (require 'ox-rss)
       (let ((buffer (find-file (expand-file-name "index.org" luna-publish-note-dir))))
         (with-current-buffer buffer
           (org-rss-export-to-rss))
         (kill-buffer buffer)))
     ;; kill buffers opened in the process
     (mapc (lambda (buf) (unless (member buf old-buffer-list) (kill-buffer buf)))
           (buffer-list))
     ;; recover theme
     (luna-load-theme original-theme))))


(defun luna-publish-rock/day (&optional force)
  "Publish rock/day blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  ;; so the syntax color is good for light background
  (luna-publish--with-theme 'doom-one-light
    (luna-publish--with-tmp-buffers
     (let ((environment '((org-html-postamble-format luna-org-html-postamble-format)
                          (org-html-postamble t))))
       (dolist (post-dir (luna-f-list-directory luna-publish-rock/day-dir))
         ;; publish each post
         (luna-publish--html-export post-dir environment force))
       ;; publish index page
       (luna-publish--html-export luna-publish-rock/day-dir environment force)))))

(defun luna-new-blog (title)
  "Make a new blog post with TITLE."
  (interactive "M")
  (let* ((year (substring (current-time-string) 20))
         (dir-file-name (downcase (replace-regexp-in-string " " "-" title)))
         (dir-path (luna-publish--join-path luna-publish-note-dir
                                            year
                                            dir-file-name))
         (file-path (luna-publish--join-path dir-file-name
                                             "index.org")))
    (mkdir dir-path)
    (find-file file-path)
    (insert (format "#+SETUPFILE: ../../setup.org
#+TITLE: %s
#+DATE:
"
                    title))
    (kill-new (format "{{{post(%s,%s/%s/)}}}"
                      title
                      year
                      dir-file-name))
    (save-buffer)
    (find-file "~/p/casouri/note/index.org")))

(provide 'luna-publish)

;;; luna-publish.el ends here

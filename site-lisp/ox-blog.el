;;; ox-blog.el --- Export backend for my blog      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; AUTHOR is derived from user-full-name
;; EMAIL is derived from user-mail-address
;; Disable code evaluation:
;; https://orgmode.org/manual/Exporting-Code-Blocks.html


;;; Code:
;;

(require 'ox-cjk-html)
(require 'subr-x)
(require 'luna-f)

(defvar org-blog-postamble-format
  '(("en" "<p class=\"author\">Written by %a</p>
<p class=\"first-publish\">First Published in %d</p>
<p class-\"last-modified\">Last modified in %C</p>
<p>Send your comment to 
<a href=\"mailto:archive.casouri.cat@gmail.com\">
archive.casouri.cat@gmail.com</a></p>")))


(defun org-blog-preamble (info)
  "Generate the UP|HOME    RSS|Source|License line."
  (let ((up (plist-get info :blog-link-up))
        (home (plist-get info :blog-link-home))
        (rss (plist-get info :blog-link-rss))
        (source (plist-get info :blog-link-source))
        (license (plist-get info :blog-link-license)))
    (string-join
     (list
      "<div id=\"org-page-header\">"
      (when (and up home)
        (format "<div>
<a accesskey=\"h\" href=\"%s\"> UP </a> |
<a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>" up home))
      "<div>"
      (when rss
        (format "<a href=\"%s\"> RSS </a> |" rss))
      (when (and source license)
        (format "<a href=\"%s\"> Source </a> | 
<a href=\"%s\"> License </a>" source license))
      "</div>
</div>")
     "\n")))

(defun org-blog-headline (headline contents info)
  "Human-readable anchor."
  (let* ((text (org-export-data
                (org-element-property :title headline) info))
         (id (url-encode-url (replace-regexp-in-string
                              " " "-" text)))
         (headline (org-element-put-property headline :CUSTOM_ID id)))
    (org-html-headline headline contents info)))


;;; Post

(org-export-define-derived-backend 'post 'cjk-html
  ;; Overwrite html backend defaults.
  :options-alist '((:html-postamble-format
                    nil nil org-blog-postamble-format)
                   (:html-postamble nil "html-postamble" t)
                   (:html-preamble nil nil 'org-blog-preamble)
                   (:html-head-include-scripts nil "html-scripts" nil)
                   (:html-head-include-default-style nil "html-style" nil)
                   (:html-html5-fancy nil nil t)
                   ;; Blog custom options.
                   (:blog-link-home "BLOG_LINK_HOME" nil nil)
                   (:blog-link-up "BLOG_LINK_UP" nil nil)
                   (:blog-link-rss "BLOG_LINK_RSS" nil nil)
                   (:blog-link-source "BLOG_LINK_SOURCE" nil nil)
                   (:blog-link-license "BLOG_LINK_LICENSE" nil nil))
  :menu-entry '(?p "Export to blog post"
                   ((?h "As HTML file" org-blog-export-to-post)))
  :translate-alist '((headline . org-blog-headline)))

(defun org-blog-export-to-post
    (&optional async subtreep visible-only body-only ext-plist)
  "Export to HTML with post backend."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
				    org-html-extension
				    "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'post file
      async subtreep visible-only body-only ext-plist)))

;;; RSS

(defun org-blog-link (link desc info)
  "Change relative links to absolute ones."
  ;; https://orgmode.org/worg/dev/org-element-api.html#org85c642d
  (let ((path (org-element-property :path link))
        (type (org-element-property :type link)))
    (when (and (equal type "file")
               (string-prefix-p "." path))
      (let* ((abs-path (expand-file-name path))
             ;; We want the final path to have a “/” at the beginning.
             (site-root (directory-file-name
                         (plist-get info :blog-site-root)))
             (relative-path (luna-f-subtract site-root abs-path)))
        (setq link (org-element-put-property link :path relative-path))))
    ;; We bypass Org’s link dispatch. This way we get the desired
    ;; image format.
    (if (and (plist-get info :html-inline-images)
	     (org-export-inline-image-p
	      link (plist-get info :html-inline-image-rules)))
        ;; We don’t deal with attributes list (pass a nil), since RSS
        ;; probably doesn’t need them, hopefully.
        (org-html--format-image
         (org-element-property :path link) nil info)
      (org-html-link link desc info))))

(defun org-blog-rss-item-template (contents info)
  "Export RSS item."
  (let* ((date (let ((time (cadar (plist-get info :date))))
                 (format-time-string
                  "%a, %d %b %Y %H:%M:%S %z"
                  (encode-time
                   0
                   (or (plist-get time :minute-start) 0)
                   (or (plist-get time :hour-start)   0)
                   (or (plist-get time :day-start)    0)
                   (or (plist-get time :month-start)  0)
                   (or (plist-get time :year-start)   0)))))
         (title (let ((title (and (plist-get info :with-title)
		                  (plist-get info :title))))
                  (org-export-data title info)))
         (link (url-encode-url (plist-get info :blog-rss-link))))
    (string-join
     (list "<item>"
           (format "<title>%s</title>" title)
           (format "<link>%s</link>" link)
           (format "<guid>%s</guid>" link)
           ;; (format "<description>%s</description>" intro)
           (format "<description><![CDATA[%s]]></description>"
                   contents)
           (format "<pubDate>%s</pubDate>" date)
           "</item>\n")
     "\n")))

(org-export-define-derived-backend 'rss-item 'post
  :options-alist '((:blog-site-base "BLOG_SITE_BASE" nil "")
                   (:blog-rss-link "BLOG_RSS_LINK" nil ""))
  :translate-alist '((link . org-blog-link)
                     (template . org-blog-rss-item-template)))


(provide 'ox-blog)

;;; ox-blog.el ends here

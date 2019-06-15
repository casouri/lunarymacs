;;; Keys

(with-eval-after-load 'general
  (general-define-key
   :keymaps 'org-mode-map
   "C-c i" #'luna-insert-heading
   "C-c <tab>" #'outline-toggle-children))

(defvar luna-todo-file "~/note/todo.org")

;;; Packages

(load-package toc-org
  :commands (toc-org-enable
             toc-org-insert-toc))

(load-package htmlize
  :commands
  org-html-export-to-html
  org-html-export-as-html)

(load-package olivetti
  :init
  (setq olivetti-body-width 80)
  :commands olivetti-mode)

(load-package org-download
  :defer t
  :init (add-hook 'org-mode-hook #'org-download-enable))

;;; Function

(defun luna-open-album-dir ()
  "Open ~/p/casouri/rock/day/album/."
  (interactive)
  (shell-command-to-string (format "open ~/p/casouri/rock/day/album/")))

(defun luna-new-blog (title)
  "Make a new blog post with TITLE."
  (interactive "M")
  (let* ((year (substring (current-time-string) 20))
         (dir-file-name (downcase (replace-regexp-in-string " " "-" title)))
         (dir-path (concat (format  "~/p/casouri/note/%s/"
                                    year)
                           dir-file-name))
         (file-path (concat dir-path
                            "/index.org")))
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

;;; Config

(define-minor-mode luna-prose-mode
  "A mode that optimizes for prose editing."
  :lighter " PROSE"
  (if luna-prose-mode
      (progn
        ;; This should be used with `doom-cyberpunk-theme' or `doom-one-light-theme'(modified)
        ;; see casouri/doom-themes repo for more
        (variable-pitch-mode)
        (setq-local cursor-type 'bar)
        ;; (setq-local blink-cursor-interval 0.6)
        ;; (blink-cursor-mode)
        (setq-local line-spacing 0.2)
        (electric-pair-local-mode -1)
        (ignore-errors (flyspell-mode 1))
        (olivetti-mode))
    (message "Sorry, alea iacta est.")))

(add-hook 'org-mode-hook #'luna-org-hook)

(defun luna-org-hook ()
  "Configuration for Org Mode."
  (luna-prose-mode)
  (electric-quote-local-mode)
  (setq-local whitespace-style '(tab-mark))
  (whitespace-mode))

;;;; 中文换行空格问题
;;
;; https://github.com/hick/emacs-chinese#中文断行

;; (defun clear-single-linebreak-in-cjk-string (string)
;;   "clear single line-break between cjk characters that is usually soft line-breaks"
;;   (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
;;          (start (string-match regexp string)))
;;     (while start
;;       (setq string (replace-match "\\1\\2" nil nil string)
;;             start (string-match regexp string start))))
;;   string)

;; (defun ox-html-clear-single-linebreak-for-cjk (string backend info)
;;   (when (org-export-derived-backend-p backend 'html)
;;     (clear-single-linebreak-in-cjk-string string)))

;; (add-to-list 'org-export-filter-final-output-functions
;;              #'ox-html-clear-single-linebreak-for-cjk)

;;;; Org Agenda

(setq org-agenda-files (list luna-todo-file))
(setq org-todo-keywords
      '((sequence "TODO"
                  "NEXT"
                  "START"
                  "WAIT"
                  "DEFER"
                  "|"
                  "DONE"
                  "CANCEL")))
(setq org-agenda-custom-commands
      '(("d" "Default Agenda View"
         ((agenda "")
          (todo ""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                 (org-agenda-overriding-header "Unscheduled/deadline tasks:")))))))

(setq org-priority-faces
      '((?A . (:inherit font-lock-warning-face))
        (?B . (:inherit default))
        (?C . (:inherit font-lock-comment-face))))

(setq org-todo-keyword-faces
      '(("DEFER" . (:inherit default :weight bold))))


;;;; Org Capture

(with-eval-after-load 'org-capture
  (setq org-default-notes-file "~/note/index.org")
  (setq org-capture-templates
        (append org-capture-templates
                `(("t" "TODOs")
                  ("te" "Emacs" entry (file+olp "~/note/todo.org" "Emacs") "*** TODO %?")
                  ("th" "Homework" entry (file+olp "~/note/todo.org" "Homework") "*** TODO %?")
                  ("to" "Other" entry (file+olp "~/note/todo.org" "Other") "*** TODO %?")
                  ("ts" "School" entry (file+olp "~/note/todo.org" "School") "*** TODO %?")
                  ("tr" "Readlist" entry (file+olp "~/note/todo.org" "Readlist") "*** TODO %?")
                  ))))

;;; Blog

(add-to-list 'luna-package-list 'f)

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

(defvar luna-publish-root-dir "~/p/casouri/note/"
  "Make sure the path follow the convention of adding slash and the end of directory.")
(defvar luna-publish-rock/day-dir "~/p/casouri/rock/day/"
  "Make sure the path follow the convention of adding slash and the end of directory.")

(defun luna-publish (&optional force)
  "Publish my blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  (require 'f)
  ;; so the syntax color is good for light background
  (luna-load-theme 'doom-one-light)
  (let ((environment '((org-html-postamble-format luna-org-html-postamble-format)
                       (org-html-postamble t)
                       (org-html-home/up-format luna-org-html-home/up-format))))
    (dolist (dir (f-directories luna-publish-root-dir))
      (dolist (post-dir (f-directories dir))
        ;; publish each post
        (luna-html-export post-dir environment force)))
    (require 'ox-rss)
    ;; publish index page
    (let ((environment '((org-html-postamble-format luna-org-html-postamble-format)
                         (org-html-postamble t)
                         (org-html-home/up-format luna-org-html-home/up-format-for-note-index))))
      (luna-html-export luna-publish-root-dir environment force))
    ;; export RSS
    (let ((buffer (find-file (expand-file-name "index.org" luna-publish-root-dir))))
      (with-current-buffer buffer
        (org-rss-export-to-rss))
      (kill-buffer buffer)))
  (luna-load-theme 'doom-cyberpunk))

(defun luna-html-export (dir &optional environment force)
  "Export index.org to index.html in DIR if the latter is older.
If FORCE is non-nil, only export when org file is newer than html file.

ENVIRONMENT is passed to `let' to setup environments."
  (eval `(let ,environment
           (let ((org-file (expand-file-name "index.org" dir))
                 (html-file (expand-file-name "index.html" dir)))
             (when (and (file-exists-p org-file)
                        (or force (file-newer-than-file-p org-file html-file)))
               (let ((buffer (find-file org-file)))
                 (with-current-buffer buffer
                   (org-html-export-to-html))
                 (kill-buffer)))))))

(defun luna-publish-rock/day (&optional force)
  "Publish rock/day blog.
If FORCE is non-nil, only export when org file is newer than html file."
  (interactive)
  (require 'f)
  ;; so the syntax color is good for light background
  (luna-load-theme 'doom-one-light)
  (let ((environment '((org-html-postamble-format luna-org-html-postamble-format)
                       (org-html-postamble t))))
    (dolist (post-dir (f-directories luna-publish-rock/day-dir))
      ;; publish each post
      (luna-html-export post-dir environment force))
    ;; publish index page
    (luna-html-export luna-publish-rock/day-dir environment force))
  (luna-load-theme 'doom-cyberpunk))

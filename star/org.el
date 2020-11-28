;; -*- lexical-binding: t; -*-

;;; Keys

(luna-def-key
 :leader
 "rf" #'org-roam-find-file
 "ri" #'org-roam-insert
 "rb" #'org-roam-buffer-toggle-display
 "df" #'deft

 :---
 :keymaps 'org-mode-map
 "C-c <tab>" #'outline-toggle-children)

;;; Packages

(load-package toc-org
  :commands toc-org-enable toc-org-insert-toc)

(load-package htmlize
  :commands
  org-html-export-to-html
  org-html-export-as-html)

(load-package olivetti
  :init (setq-default olivetti-body-width 74)
  :commands olivetti-mode)

(load-package org-download
  :hook (org-mode-hook . org-download-enable))

(load-package valign
  :hook (org-mode-hook . valign-mode))

;; (load-package org-backtick
;;   :hook (org-mode-hook . org-backtick-mode))

(luna-on "Brown"
  (load-package orgmark
    :load-path "~/p/OrgMark"
    :commands orgmark-insert orgmark-edit orgmark-abort))

(with-eval-after-load 'org-latex
  (add-to-list 'org-latex-packages-alist '("" "listings" nil))
  (setq org-latex-listings t)
  (setq org-latex-listings-options '(("breaklines" "true")))
  (setq org-latex-compiler "xelatex"))


(load-package org
  :defer
  :config (setq org-edit-src-content-indentation 0
                org-adapt-indentation nil
                ;; This program generates crisp images for latex
                ;; preview.
                org-preview-latex-default-process 'dvisvgm))

;;; Org config

(define-minor-mode luna-prose-mode
  "A mode that optimizes for prose editing."
  :lighter " PROSE"
  (if luna-prose-mode
      (progn
        (variable-pitch-mode)
        (olivetti-mode)
        ;; (luna-scale-cjk-mode)
        (electric-pair-local-mode -1)
        (setq-local cursor-type 'bar)
        ;; This is a global mode!
        ;; (require 'delicate-click)
        ;; (delicate-click-mode)
        ;; (setq-local blink-cursor-interval 0.6)
        ;; (blink-cursor-mode)
        (setq-local line-spacing 0.15))
    (variable-pitch-mode -1)
    (olivetti-mode -1)
    ;; (luna-scale-cjk-mode -1)
    (electric-pair-local-mode)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'cursor-type)))

(defun luna-org-hook ()
  "Configuration for Org Mode."
  (company-mode -1)
  (luna-prose-mode)
  (electric-quote-local-mode)
  (setq-local whitespace-style '(tab-mark))
  (whitespace-mode)
  (require 'org-tempo))

(add-hook 'org-mode-hook #'luna-org-hook)
(font-lock-add-keywords 'org-mode '(("^ *- " 0 'fixed-pitch)))

;;; Org calendar config

(add-to-list 'display-buffer-alist
             '((lambda (buf _)
                 (with-current-buffer buf
                   (and (derived-mode-p 'calendar-mode)
                        (eq this-command 'org-time-stamp))))
               . (display-buffer-in-child-frame
                  . ((child-frame-parameters
                      . ((desktop-dont-save . t)
                         (menu-bar-lines . 0)
                         (tool-bar-lines . 0)
                         (minibuffer-exit . delete-frame)
                         (height . 10)
                         (ns-transparent-titlebar . t)))))))

;;; Org Agenda config

(defvar luna-todo-file "~/note/todo.org")
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

;;; Org Capture config

(with-eval-after-load 'org-capture
  (setq org-default-notes-file "~/note/index.org")
  (setq org-capture-templates
        (append org-capture-templates
                `(("t" "TODOs")
                  ("te" "Emacs" entry
                   (file+olp "~/note/todo.org" "Emacs") "*** TODO %?")
                  ("th" "Homework" entry
                   (file+olp "~/note/todo.org" "Homework") "*** TODO %?")
                  ("to" "Other" entry
                   (file+olp "~/note/todo.org" "Other") "*** TODO %?")
                  ("ts" "School" entry
                   (file+olp "~/note/todo.org" "School") "*** TODO %?")
                  ("tr" "Readlist" entry
                   (file+olp "~/note/todo.org" "Readlist") "*** TODO %?")
                  ))))

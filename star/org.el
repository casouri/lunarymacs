;; -*- lexical-binding: t; -*-

;;; Keys

(with-eval-after-load 'luna-general-config
  (general-define-key
   :keymaps 'org-mode-map
   "C-c i" #'luna-insert-heading
   "C-c <tab>" #'outline-toggle-children)
  (luna-default-leader
    "rf" #'org-roam-find-file
    "ri" #'org-roam-insert
    "rb" #'org-roam-buffer-toggle-display))

;;; Packages

(load-package toc-org
  :commands toc-org-enable toc-org-insert-toc)

(load-package htmlize
  :commands
  org-html-export-to-html
  org-html-export-as-html)

(load-package olivetti
  :init (setq olivetti-body-width 80)
  :commands olivetti-mode)

(load-package org-download
  :hook (org-mode-hook . org-download-enable))

(load-package valign
  :hook (org-mode-hook . valign-mode))

(load-package quanjiao
  :hook (org-mode-hook . quanjiao-mode))

(add-to-list 'luna-package-list 'wucuo)

(luna-on "Brown"
  (load-package orgmark
    :load-path "~/p/OrgMark"
    :commands orgmark-insert orgmark-edit orgmark-abort))

;;; Org config

(define-minor-mode luna-prose-mode
  "A mode that optimizes for prose editing."
  :lighter " PROSE"
  (if luna-prose-mode
      (progn
        (variable-pitch-mode)
        (olivetti-mode)
        (luna-scale-cjk-mode)
        (wucuo-start)
        (electric-pair-local-mode -1)
        (setq-local cursor-type 'bar)
        (require 'delicate-click)
        (delicate-click-mode)
        ;; (setq-local blink-cursor-interval 0.6)
        ;; (blink-cursor-mode)
        (setq-local line-spacing 0.2))
    (variable-pitch-mode -1)
    (olivetti-mode -1)
    (luna-scale-cjk-mode -1)
    (wucuo-stop)
    (electric-pair-local-mode)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'cursor-type)))

(with-eval-after-load 'org-latex
  (add-to-list 'org-latex-packages-alist '("" "listings" nil))
  (setq org-latex-listings t)
  (setq org-latex-listings-options '(("breaklines" "true")))
  (setq org-latex-compiler "xelatex"))

(defun luna-org-hook ()
  "Configuration for Org Mode."
  (company-mode -1)
  (luna-prose-mode)
  (electric-quote-local-mode)
  (setq-local whitespace-style '(tab-mark))
  (whitespace-mode)
  (require 'org-tempo))

(add-hook 'org-mode-hook #'luna-org-hook)

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
                  ("te" "Emacs" entry (file+olp "~/note/todo.org" "Emacs") "*** TODO %?")
                  ("th" "Homework" entry (file+olp "~/note/todo.org" "Homework") "*** TODO %?")
                  ("to" "Other" entry (file+olp "~/note/todo.org" "Other") "*** TODO %?")
                  ("ts" "School" entry (file+olp "~/note/todo.org" "School") "*** TODO %?")
                  ("tr" "Readlist" entry (file+olp "~/note/todo.org" "Readlist") "*** TODO %?")
                  ))))

;;; Patch

(luna-load-relative "star/org/org-patch.el")

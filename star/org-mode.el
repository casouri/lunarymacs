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

;;; Org calendar

(add-to-list 'display-buffer-alist
             '((lambda (buf _)
                 (with-current-buffer buf
                   (and (derived-mode-p 'calendar-mode)
                        (eq this-command 'org-time-stamp))))
               . (display-buffer-in-side-window
                  . ((side . bottom)))))

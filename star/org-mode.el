;; -*- lexical-binding: t; -*-

;;; Keys

(luna-def-key
 :leader
 "oc" #'org-capture

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

(defun beginning-of-sentence (arg)
  "Go to the beginning of the current sentence.
ARG is the numerical argument."
  (interactive "p")
  (backward-char)
  (dotimes (_ arg)
    (goto-char (beginning-of-thing 'sentence))))

(defun end-of-sentence (arg)
  "Go to the beginning of the current sentence.
ARG is the numerical argument."
  (interactive "p")
  (forward-char)
  (dotimes (_ arg)
    (goto-char (end-of-thing 'sentence))))

(defvar luna-prose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") #'beginning-of-visual-line)
    (define-key map (kbd "C-e") #'end-of-visual-line)
    map)
  "Mode map for ‘luna-prose-mode’.")

(define-minor-mode luna-prose-mode
  "A mode that optimizes for prose editing."
  :lighter " PROSE"
  :keymap luna-prose-mode-map
  (if luna-prose-mode
      (progn
        (variable-pitch-mode)
        (olivetti-mode)
        ;; (luna-scale-cjk-mode)
        (electric-pair-local-mode -1)
        (electric-quote-local-mode)
        (setq-local cursor-type 'bar)
        ;; This is a global mode!
        ;; (require 'delicate-click)
        ;; (delicate-click-mode)
        (setq-local line-spacing 0.15)
        (company-mode -1)
        (setq-local whitespace-style '(tab-mark))
        (whitespace-mode))
    (whitespace-mode -1)
    (company-mode)
    (variable-pitch-mode -1)
    (olivetti-mode -1)
    ;; (luna-scale-cjk-mode -1)
    (electric-pair-local-mode)
    (electric-quote-local-mode -1)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'cursor-type)))

(defun luna-org-hook ()
  "Configuration for Org Mode."
  (luna-prose-mode)
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

;;; Org capture

(load-package org-web-tools :defer t)

(defun luna-capture-title ()
  "Insert title and url."
  (require 'org-web-tools)
  (concat (org-web-tools--html-title
           (org-web-tools--get-url (org-web-tools--get-first-url)))
          "\n"
          (org-web-tools--get-first-url)))

(with-eval-after-load 'org-capture
  (add-to-list
   'org-capture-templates
   '("w" "Web archive" plain
     (file+function "~/deft/articles.org"
                    (lambda ()
                      (goto-char (point-min))
                      (search-forward "articles")))
     "%t\n%?%(luna-capture-title)"
     :prepend t
     :empty-lines 1)))

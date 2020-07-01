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
  :commands (toc-org-enable
             toc-org-insert-toc))

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

;;; Org

(define-minor-mode luna-prose-mode
  "A mode that optimizes for prose editing."
  :lighter " PROSE"
  (if luna-prose-mode
      (progn
        ;; This should be used with `doom-cyberpunk-theme' or `doom-one-light-theme'(modified)
        ;; see casouri/doom-themes repo for more
        (variable-pitch-mode)
        (olivetti-mode)
        (luna-scale-cjk-mode)
        (ignore-errors (flyspell-mode 1))
        (electric-pair-local-mode -1)
        (setq-local cursor-type 'bar)
        ;; (setq-local blink-cursor-interval 0.6)
        ;; (blink-cursor-mode)
        (setq-local line-spacing 0.2))
    (variable-pitch-mode -1)
    (olivetti-mode -1)
    (luna-scale-cjk-mode -1)
    (flyspell-mode -1)
    (electric-pair-local-mode)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'cursor-type)))

;; export code with line wrap
;; https://emacs.stackexchange.com/questions/33010/how-to-word-wrap-within-code-blocks
;;
(with-eval-after-load 'org-mode
  (require 'org-tempo))

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
  (whitespace-mode))

(add-hook 'org-mode-hook #'luna-org-hook)

;;; Org Agenda

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


;;; Org Capture

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

;;; Function

(defun luna-open-album-dir ()
  "Open ~/p/casouri/rock/day/album/."
  (interactive)
  (shell-command-to-string (format "open ~/p/casouri/rock/day/album/")))

;;; Hack
;;
;; Display ~ and = as `

(with-eval-after-load 'org
  (setq org-emphasis-alist
        '(("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim (display "`"))
          ("~" org-code verbatim (display "`"))
          ("+"
           (:strike-through t))))

  (defun org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
			    (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
	  (let* ((marker (match-string 2))
	         (verbatim? (member marker '("~" "="))))
	    (when (save-excursion
		    (goto-char (match-beginning 0))
		    (and
		     ;; Do not match table hlines.
		     (not (and (equal marker "+")
			       (org-match-line
			        "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
		     ;; Do not match headline stars.  Do not consider
		     ;; stars of a headline as closing marker for bold
		     ;; markup either.
		     (not (and (equal marker "*")
			       (save-excursion
			         (forward-char)
			         (skip-chars-backward "*")
			         (looking-at-p org-outline-regexp-bol))))
		     ;; Match full emphasis markup regexp.
		     (looking-at (if verbatim? org-verbatim-re org-emph-re))
		     ;; Do not span over paragraph boundaries.
		     (not (string-match-p org-element-paragraph-separate
					  (match-string 2)))
		     ;; Do not span over cells in table rows.
		     (not (and (save-match-data (org-match-line "[ \t]*|"))
			       (string-match-p "|" (match-string 4))))))
              ;; beg
	      (pcase-let ((`(,_ ,face ,_ ,props) (assoc marker org-emphasis-alist)))
                ;; end
	        (font-lock-prepend-text-property
	         (match-beginning 2) (match-end 2) 'face face)
	        (when verbatim?
		  (org-remove-flyspell-overlays-in
		   (match-beginning 0) (match-end 0))
		  (remove-text-properties (match-beginning 2) (match-end 2)
					  '(display t invisible t intangible t)))
	        (add-text-properties (match-beginning 2) (match-end 2)
				     '(font-lock-multiline t org-emphasis t))
                ;; beg
                (when props
                  (add-text-properties (match-end 4) (match-beginning 5)
				       props)
                  (add-text-properties (match-beginning 3) (match-end 3)
				       props))
                ;; end
	        (when org-hide-emphasis-markers
		  (add-text-properties (match-end 4) (match-beginning 5)
				       '(invisible org-link))
		  (add-text-properties (match-beginning 3) (match-end 3)
				       '(invisible org-link)))
	        (throw :exit t)))))))))

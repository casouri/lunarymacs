;; color-outline.el --- Outline w/ color      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package provides a basic version of outshine.el, providing:
;; 
;;   1) Highlight each header level
;;   2) outline folding
;;
;; Usage: M-x color-outline-mode RET
;;
;; Header level is determined by the number of comment characters.
;; The first level header starts from 3 comment characters.
;; For example, in ‘emacs-lisp-mode’:
;;
;;     ;;; Header 1
;;     ;;;; Header 2
;;     ;;;;; Header 3
;;
;; In ‘python-mode’:
;;
;;     ### Header 1
;;     #### Header 2
;;     ##### Header 3
;;
;; To toggle evah header, use outline commands. We also provide a
;; command to toggle the whole buffer: ‘color-outline-toggle-all’.
;;
;; Add support for new major modes by
;;
;;     (color-outline-define-header MODE COMMENT-CHAR)
;;
;; COMMENT-CHAR for ‘python-mode’ is "#", for example.

;;; Code:
;;

(require 'cl-lib)
(require 'subr-x)
(require 'hi-lock)

(defvar color-outline-comment-char-alist '((c-mode . "/")
                               (python-mode . "#")
                               (javascript-mode . "/")
                               (css-mode . "/"))
  "Stores custom comment character each major mode.
For most major modes ‘comment-start’ is enough.")

(defvar color-outline-face-list '(info-title-1 info-title-2 info-title-3)
  "Face for each level.")

(defvar color-outline-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "<C-tab>") #'outline-toggle-children)
    ;; (define-key map (kbd "C-M-i") #'color-outline-toggle-hide-show)
    map)
  "Mode map for ‘color-outline-mode’.")

(defvar-local color-outline--folded nil
  "Hide show state. Internal use.")

(defun color-outline-toggle-all ()
  "Toggle hide/show outline."
  (interactive)
  (if color-outline--folded
      (progn (outline-show-all)
             (setq color-outline--folded nil))
    (outline-hide-body)
    (setq color-outline--folded t)))

(defun color-outine--has-children ()
  "Return t if this header has children.
Assumes point is at the beginning of a header."
  (save-excursion
    (let ((count 0))
      (outline-map-region (lambda () (setq count (1+ count)))
                          (point)
                          (condition-case nil
                              (progn (outline-forward-same-level
                                      (outline-level))
                                     (point))
                            (error (point-max))))
      (> count 1))))

(defun color-outline-toggle-heading ()
  "Toggle visibility of children.
Toggle between “only header”, “header and sub-header”, “everything”."
  (interactive)
  ;; go to a header
  (save-excursion
    (outline-back-to-heading)
    (let ((prop (plist-get (text-properties-at (point))
                           'color-outline-state)))
      (when (and (eq prop 'header) (not (color-outine--has-children)))
        (put-text-property (point) (1+ (point))
                           'color-outline-state
                           'sub-header))
      (pcase prop
        ('header (outline-show-children)
                 (put-text-property (point) (1+ (point))
                                    'color-outline-state
                                    'sub-header)
                 (message "Sub-headers"))
        ('sub-header (outline-show-subtree)
                     (put-text-property (point) (1+ (point))
                                        'color-outline-state
                                        'all)
                     (message "All"))
        (_ (outline-hide-subtree)
           (put-text-property (point) (1+ (point))
                              'color-outline-state
                              'header)
           (message "Header"))))))

(defun color-outline-* (number string)
  "Return NUMBER of STRINGs sticked together."
  (string-join (cl-loop for i from 1 to number
                        collect string)))

(defun color-outline--create-pattern (comment-char)
  "Return the header pattern for major mode MODE.
COMMENT-CHAR (string) is the comment character of this mode."
  (let* ((header-level (length color-outline-face-list))
         (outline-re (format "%s[%s]* [^\t\n]"
                             (color-outline-* header-level comment-char)
                             comment-char))
         (hi-re-list (cl-loop
                      for level from 0 to (1- header-level)
                      collect
                      (format "^%s%s .*"
                              ;; base (3)
                              (color-outline-* 3 comment-char)
                              ;; determined level
                              (color-outline-* level comment-char))))
         (hi-pattern-list (cl-loop for re in hi-re-list
                                   for face in color-outline-face-list
                                   collect `(,re (0 ',face t)))))
    (list outline-re hi-pattern-list)))

(defun color-outline-define-header (mode comment-char)
  "Define the header pattern for major mode MODE.
COMMENT-CHAR (char) is the comment character of this mode."
  (setf (alist-get mode color-outline-mode-alist)
        (color-outline--create-pattern comment-char)))

(define-minor-mode color-outline-mode
  "Color outline."
  :lighter ""
  :keymap 'color-outline-mode-map
  (if color-outline-mode
      (if-let* ((comment-char (or (alist-get major-mode
                                             color-outline-comment-char-alist)
                                  comment-start))
                (config (color-outline--create-pattern comment-char)))
          (progn (setq outline-regexp (car config))
                 (hi-lock-set-file-patterns (cadr config))
                 (outline-minor-mode)
                 (hi-lock-mode))
        (user-error "No color-outline pattern configured for %s"
                    major-mode))
    (outline-minor-mode -1)
    (hi-lock-mode -1)))

(provide 'color-outline)

;;; color-outline.el ends here

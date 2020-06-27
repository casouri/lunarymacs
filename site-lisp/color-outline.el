;;; color-outline.el --- Outline w/ color      -*- lexical-binding: t; -*-

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
;; To toggle each header, use outline commands. Outline+
;; provides two nice ones.
;;
;; Add support for new major modes by
;;
;;     (color-outline-define-header MODE COMMENT-CHAR COMMENT-BEGIN)
;;
;; COMMENT-CHAR for ‘python-mode’ is “#”, for example. It can be more
;; than one character. COMMENT-BEGIN is the (possibly empty) beginning
;; of the header. For example, in OCaml, comments are (* ... *). Then
;; COMMENT-BEGIN is “(” and COMMENT-CHAR is “*”.
;;
;; You can also just edit ‘color-outline-comment-char-alist’.

;;; Code:
;;

(require 'cl-lib)
(require 'subr-x)
(require 'hi-lock)

(defvar color-outline-comment-char-alist '((c-mode "/" nil)
                                           (python-mode "#" nil)
                                           (javascript-mode "/" nil)
                                           (css-mode "/" nil)
                                           (tuareg-mode "*" "("))
  "Stores custom comment character each major mode.
For most major modes ‘comment-start’ is enough.")

(defvar color-outline-face-list '(outline-1 outline-2 outline-3 outline-4)
  "Face for each level.")

(defvar color-outline-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "<C-tab>") #'outline-toggle-children)
    ;; (define-key map (kbd "C-M-i") #'color-outline-toggle-hide-show)
    map)
  "Mode map for ‘color-outline-mode’.")

(defun color-outline--create-pattern (comment-char comment-begin)
  "Return the header pattern for major mode MODE.
COMMENT-CHAR (string) is the comment character of this mode.
COMMENT-BEGIN is string pattern starting a comment.
The result pattern is “COMMENT-START(COMMENT-CHAR){3}”."
  (let* ((header-level (length color-outline-face-list))
         (outline-re (rx-to-string `(seq ,comment-begin
                                         (= 3 ,comment-char)
                                         (group (* ,comment-char)))))
         (hi-re-list (cl-loop
                      for level from 0 to (1- header-level)
                      collect
                      (format "^%s%s%s [^\t\n]*"
                              (regexp-quote comment-begin)
                              ;; base (3)
                              (regexp-quote
                               (color-outline-* 3 comment-char))
                              ;; determined level
                              (regexp-quote
                               (color-outline-* level comment-char)))))
         (hi-pattern-list (cl-loop for re in hi-re-list
                                   for face in color-outline-face-list
                                   collect `(,re (0 ',face t)))))
    (list outline-re hi-pattern-list)))

(defun color-outline-define-header (mode comment-char comment-begin)
  "Define the header pattern for major mode MODE.
COMMENT-CHAR (char) is the comment character of this mode.
COMMENT-BEGIN is string pattern starting a comment."
  (setf (alist-get mode color-outline-comment-char-alist)
        (color-outline--create-pattern comment-char comment-begin)))

(define-minor-mode color-outline-mode
  "Color outline."
  :lighter ""
  :keymap 'color-outline-mode-map
  (if color-outline-mode
      (if-let* ((rule (or (alist-get major-mode
                                     color-outline-comment-char-alist)
                          (list comment-start "")))
                (comment-char (or (car rule) comment-start))
                (comment-begin (or (cadr rule) ""))
                (config (color-outline--create-pattern
                         comment-char comment-begin)))
          (progn (setq outline-regexp (car config))
                 (setq outline-level
                       (lambda () (1+ (/ (length (match-string 1))
                                         (length comment-char)))))
                 (hi-lock-set-file-patterns (cadr config))
                 (outline-minor-mode)
                 (hi-lock-mode))
        (user-error "No color-outline pattern configured for %s"
                    major-mode))
    (outline-minor-mode -1)
    (hi-lock-mode -1)))

(provide 'color-outline)

;;; color-outline.el ends here

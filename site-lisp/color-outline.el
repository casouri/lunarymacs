;;; color-outline.el --- Outline w/ color      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package is a basic version of outshine.el, it provides a quick
;; and easy way of setting up headline patterns and integration with
;; outline commands, highlighting of headlines and imenu support.
;;
;; Usage:
;;
;; M-x color-outline-mode RET
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
;; To toggle each header, use outline commands.
;;
;; Ideally, ‘comment-start’ defined by major modes are enough for
;; setting up color-outline, however, sometimes ‘comment-char’ is not
;; sufficient. Then you can add support for new major modes by
;;
;;     (color-outline-define-header MODE COMMENT-CHAR COMMENT-BEGIN)
;;
;; For example, COMMENT-CHAR for ‘python-mode’ is “#”. It can be more
;; than one character. COMMENT-BEGIN is the (possibly empty) beginning
;; of the header. For example, in OCaml, comments are (* ... *). Then
;; COMMENT-BEGIN is “(” and COMMENT-CHAR is “*”.
;;
;; Instead of using ‘color-outline-define-header’, you can also modify
;; ‘color-outline-comment-char-alist’ directly.
;;
;; If you want buffer-local setting for color-outline, you can add a
;; file-local variable ‘color-outline-local-comment-char’.
;;
;; Color-outline looks for comment char settings in the following order:
;; 1. Buffer-local ‘color-outline-local-comment-char’
;; 2. ‘color-outline-comment-char-alist’
;; 3. ‘comment-start’ defined by the major mode.

;;; Code:
;;

(require 'cl-lib)
(require 'subr-x)
(require 'rx)

(defgroup color-outline
  '((color-outline-comment-char-alist custom-variable)
    (color-outline-disable-list custom-variable)
    (color-outline-face-list custom-variable))
  "Easy programming mode outline with visual highlight."
  :group 'outline)

(defcustom color-outline-comment-char-alist
  '((c-mode "*" "/")
    (c++-mode "*" "/")
    (c-ts-mode "*" "/")
    (c++-ts-mode "*" "/")
    (python-mode "#")
    (python-ts-mode "#")
    (javascript-mode "/")
    (javascript-ts-mode "/")
    (css-mode "*" "/")
    (css-ts-mode "*" "/")
    (tuareg-mode "*" "(")
    (shell-script-mode "#")
    (web-mode "-" "<!")
    (sh-mode "#")
    (rust-ts-mode "*" "// "))
  "Stores custom comment character for each major mode.
An alist of (MAJOR-MODE . (COMMENT-CHAR COMMENT-BEGIN))
or (MAJOR-MODE . (COMMENT-CHAR)). For other major modes,
‘comment-start’ is enough."
  :type '(alist :key-type symbol
                :value-type sexp)
  :group 'color-outline)

(defcustom color-outline-disable-list '(org-mode outline-mode)
  "Color-outline mode is not enabled in these modes."
  :type '(repeat symbol)
  :group 'color-outline)

(defcustom color-outline-face-list
  '(outline-1 outline-2 outline-3 outline-4)
  "Face for each level."
  :type '(repeat symbol)
  :group 'color-outline)

(defvar-local color-outline-local-comment-char nil
  "Overriding buffer local setting for color-outline comment char.
This value takes the same form as the values in alist
‘color-outline-comment-char-alist’. I.e., a list (COMMENT-CHAR
COMMENT-BEGIN).")

(defvar-local color-outline--keywords nil
  "We store font-lock keywords in this variable.
This is used to remove font-lock rules when ‘color-outline-mode’
is turned off.")

(defvar-local color-outline--imenu-expression nil
  "We store imenu expressions in this variable.
This is used to remove imenu expressions when ‘color-outline-mode’
is turned off.")

(defun color-outline--create-pattern (comment-char comment-begin)
  "Return the header pattern for major mode MODE.
COMMENT-CHAR (string) is the comment character of this mode.
COMMENT-BEGIN is string pattern starting a comment.
The result pattern is

<COMMENT-START><COMMENT-CHAR>{3}<COMMENT-CHAR>*<SPACE><ANYCHAR>*

The first group is the second group of COMMENT-CHARS, the second
group is <ANYCHAR>*.

Return a plist

    (:outline PATTERN :font-lock PATTERN-LIST)

where PATTERN is suitable for `outline-regepx', PATTERN-LIST is suitable
for `font-lock-add-keywords' (a list of specs)."
  (let* ((header-level (length color-outline-face-list))
         (outline-re (rx-to-string `(seq ,comment-begin
                                         (= 3 ,comment-char)
                                         (group (* ,comment-char))
                                         " "
                                         (group (* (not (any ?\t ?\n)))))))
         (re-list (cl-loop
                   for level from 0 to (1- header-level)
                   collect
                   (rx-to-string `(seq bol
                                       ,comment-begin
                                       (= 3 ,comment-char)
                                       (= ,level ,comment-char)
                                       " "
                                       (* (not (any ?\t ?\n)))))))
         (font-lock-list (cl-loop for re in re-list
                                  for face in color-outline-face-list
                                  collect `(,re (0 ',face t t)))))
    (list :outline outline-re :font-lock font-lock-list)))

(defun color-outline-define-header (mode comment-char comment-begin)
  "Define the header pattern for major mode MODE.
COMMENT-CHAR (char) is the comment character of this mode.
COMMENT-BEGIN is string pattern starting a comment."
  (setf (alist-get mode color-outline-comment-char-alist)
        (color-outline--create-pattern comment-char comment-begin)))

(define-minor-mode color-outline-mode
  "Color outline."
  :lighter "Co"
  (if (and color-outline-mode
           (not (apply #'derived-mode-p color-outline-disable-list)))
      (if-let* ((rule (or color-outline-local-comment-char
                          (alist-get major-mode
                                     color-outline-comment-char-alist)
                          (list comment-start "")))
                (comment-char (or (car rule) comment-start))
                (comment-begin (or (cadr rule) ""))
                (config (color-outline--create-pattern
                         comment-char comment-begin))
                (outline-re (plist-get config :outline))
                (imenu-expression `("Section" ,outline-re 2))
                (font-lock-keyword-list (plist-get config :font-lock)))
          (progn (setq-local outline-regexp outline-re)
                 (setq-local outline-level
                             (lambda () (1+ (/ (length (match-string 1))
                                               (length comment-char)))))
                 (setq-local imenu-generic-expression
                             (cons imenu-expression
                                   imenu-generic-expression))
                 (when (not (bound-and-true-p
                             outline-minor-mode-highlight))
                   (font-lock-add-keywords nil font-lock-keyword-list))
                 (setq color-outline--keywords font-lock-keyword-list)
                 (setq color-outline--imenu-expression imenu-expression)
                 (outline-minor-mode))
        ;; Don’t error in major mode hooks.
        (message "No color-outline pattern configured for %s"
                 major-mode))
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)
    (font-lock-remove-keywords nil color-outline--keywords)
    (setq-local imenu-generic-expression
                (remove color-outline--imenu-expression
                        imenu-generic-expression))
    (outline-minor-mode -1))
  (jit-lock-refontify))

(provide 'color-outline)

;;; color-outline.el ends here


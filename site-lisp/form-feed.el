;;; form-feed.el --- Display ^L glyphs as horizontal lines

;; Copyright (C) 2014 Vasilij Schneidermann <mail@vasilij.de>

;; Author: Vasilij Schneidermann <mail@vasilij.de>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; URL: https://depp.brause.cc/form-feed
;; Keywords: faces
;; Version: 0.2.3

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This minor mode displays page delimiters which usually appear as ^L
;; glyphs on a single line as horizontal lines spanning the entire
;; window.  It is suitable for inclusion into mode hooks and is
;; intended to be used that way.  The following snippet would enable
;; it for Emacs Lisp files for instance:
;;
;;     (add-hook 'emacs-lisp-mode-hook 'form-feed-mode)

;;; Note:
;;
;; Yuan: I removed a bunch of functions and variables and changed
;; font-lock to jit-lock.


;;; Code:


;;; variables

(defgroup form-feed nil
  "Turn ^L glyphs into horizontal lines."
  :prefix "form-feed-"
  :group 'faces)

(defface form-feed-line
  '((((type graphic)
      (background light)) :strike-through "black")
    (((type graphic)
      (background dark)) :strike-through "white")
    (((type tty)) :inherit font-lock-comment-face :underline t))
  "Face for form-feed-mode lines."
  :group 'form-feed)

(defcustom form-feed-line-width t
  "Width of the form feed line.
It may be one of the following values:

t: Full width.

floating point number: Ratio of full width.  A value of 0.5 would
use half the width.

positive integer number: Width as measured in columns.  A value
of 80 would use a 80 characters wide line.

negative integer number: Full width minus specified number of
columns.  A value of -1 would leave the last column empty."
  :type '(choice (const :tag "Full width" t)
                 (float :tag "Ratio")
                 (integer :tag "Columns"))
  :group 'form-feed)

(defvar form-feed--line-width
  (cond
   ((integerp form-feed-line-width)
    (if (>= form-feed-line-width 0)
        form-feed-line-width
      `(- text ,(abs form-feed-line-width))))
   ((floatp form-feed-line-width)
    `(,form-feed-line-width . text))
   (t 'text)))

(defvar form-feed--font-lock-keywords
  ;; NOTE see (info "(elisp) Search-based fontification") and the
  ;; `(MATCHER . SUBEXP-HIGHLIGHTER)' section
  `((,page-delimiter 0 form-feed--font-lock-face t)))

(defcustom form-feed-lighter " ^L"
  "Lighter for `form-feed-mode'."
  :type 'string
  :group 'form-feed
  :risky t)


;;; Functions

(defun form-feed-fontify (beg end)
  "Fonfity page breaks at beginning of line between BEG and END."
  (goto-char beg)
  (while (re-search-forward "^" end t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         `( font-lock-face form-feed-line
                            display (space :width ,form-feed--line-width)
                            rear-non-sticky t))))

;;;###autoload
(define-minor-mode form-feed-mode
  "Toggle special rendering of form feed characters.

This minor mode displays page delimiters which usually appear as ^L
glyphs on a single line as horizontal lines spanning the entire
window."
  :lighter form-feed-lighter
  (if form-feed-mode
      (jit-lock-register #'form-feed-fontify)
    (jit-lock-unregister #'form-feed-fontify))
  (jit-lock-refontify))

(provide 'form-feed)
;;; form-feed.el ends here

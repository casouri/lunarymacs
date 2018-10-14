;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : soar-misc.el
;;;; Author          : Michael Hucka
;;;; Created On      : Sun Jun 10 22:13:21 1990
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Fri Mar 20 19:03:31 1992
;;;; Update Count    : 28
;;;; 
;;;; PURPOSE
;;;; 	Definitions of some missing CL functions from Elisp.
;;;; Table of contents
;;;; 	I.	line-not-commented 
;;;;
;;;; Copyright 1990, Mike Hucka.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  changed names of index functions to match trim functions in name order -fer

;;;
;;;	i.	Initializations and variables
;;;

(require 'cl)
(provide 'soar-misc)

;; (if (fboundp 'proclaim-inline)
;;   (proclaim-inline
;;     string-trim
;;     string-left-trim
;;     string-right-trim
;;     string-left-trim-index
;;     string-right-trim-index
;;     schar
;; ))


;;;
;;; 	I.	line-not-commented 
;;;

;; (defun line-not-commented ()
;;   "Returns t if line does not start with a ;"
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (not (looking-at ";"))))

;;; stolen from the net

(defun edit-string (s &optional bufname temp bindings)
  "Pops up a buffer to recursively edit STRING.  If terminated using
abort-recursive-edit, the original string is returned.  If terminated with
exit-recursive-edit, the edited string is returned.  Optional 2nd arg
BUFNAME is name of buffer to use.  Optional 3rd arg TEMP non-nil means
kill buffer when done.  Optional last arg BINDINGS is a keymap of
bindings to use in the edit buffer."
  (let ((buf (get-buffer-create (or bufname "*Edit*"))))
        (if bindings (use-local-map bindings))
        (save-window-excursion
          (pop-to-buffer buf)
          (erase-buffer)
          (insert s)
          (beginning-of-buffer)
          (prog1
                  (condition-case e
                          (progn
                                (recursive-edit)
                                (buffer-string))
                        (quit s))
                (if temp (kill-buffer (current-buffer))
                  (bury-buffer))))))

;;; String trimming functions
;;;---------------------------------------------------------------------------
;;; Pieces stolen from PSL/PCLS 3.2 (Stan Shebs/Sandra Loosemore, U. of Utah) 

(defun string-trim (bag s)
  "Returns a substring of the string specified by S that has had every
character in BAG removed from the beginning and end.  S must be a string or a
symbol.  If S is a symbol, its print name is used as the string.  The BAG
argument may be any sequence of characters.  Characters are trimmed from the
beginning and from the end of S until the first character not in BAG is
found."
  (let* ((len (length s))
	 (i1  (string-left-trim-index bag s 0 len))
	 (i2  (string-right-trim-index bag s len)))
    (if (<= i2 i1) "" (substring s i1 i2))))

; (string-trim '(?\ ?T) " Tasddf asdf af  ")
; (string-trim '(?\ ?\t) "  tBype asddf asdf af")

(defun string-left-trim (bag s)
  "Returns a substring of the string specified by S that has had every
character in BAG removed from the beginning.  S must be a string or a symbol.
The BAG argument may be any sequence of characters.  Characters are trimmed
from the beginning of S until the first character not in BAG is found."
  (let* ((len (length s))
	 (i1  (string-left-trim-index bag s 0 len)))
    (if (<= len i1) "" (substring s i1 len))))


(defun string-right-trim (bag s) 
  "Returns a substring of the string specified by S that has had every
character in BAG removed from the end.  S must be a string or a symbol.
The BAG argument may be any sequence of characters.  Characters are trimmed
from the end of S until the first character not in BAG is found."
  (let ((i2 (string-right-trim-index bag s (length s))))
    (if (<= i2 0) "" (substring s 0 i2))))


(defun string-left-trim-index (bag s i uplim)
  (if (or (eql i uplim)
	  (not (member (schar s i) bag)))
    i
    (string-left-trim-index bag s (1+ i) uplim)))


(defun string-right-trim-index (bag s i)
  (if (or (eql i 0)
	  (not (member (schar s (1- i)) bag)))
    i
    (string-right-trim-index bag s (1- i))))


(defun schar (s i)
  "Returns the ITH character of string S as a character object.  S must be
a simple string or a symbol.  If S is a symbol, its print name is used
as the string to operate on.  I must be a non-negative integer less than the
length of the string (indexing is zero-origin).  The function schar applied
to simple strings behaves identically to aref or char, but it may be faster
than either in many implementations."

  (string-to-char (substring s i)))

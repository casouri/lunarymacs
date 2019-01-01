;;; notmuch-parser.el --- streaming S-expression parser
;;
;; Copyright © Austin Clements
;;
;; This file is part of Notmuch.
;;
;; Notmuch is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Notmuch is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Notmuch.  If not, see <https://www.gnu.org/licenses/>.
;;
;; Authors: Austin Clements <aclements@csail.mit.edu>

;;; Code:

(require 'cl)

(defun notmuch-sexp-create-parser ()
  "Return a new streaming S-expression parser.

This parser is designed to incrementally read an S-expression
whose structure is known to the caller.  Like a typical
S-expression parsing interface, it provides a function to read a
complete S-expression from the input.  However, it extends this
with an additional function that requires the next value in the
input to be a list and descends into it, allowing its elements to
be read one at a time or further descended into.  Both functions
can return 'retry to indicate that not enough input is available.

The parser always consumes input from point in the current
buffer.  Hence, the caller is allowed to delete any data before
point and may resynchronize after an error by moving point."

  (vector 'notmuch-sexp-parser
	  ;; List depth
	  0
	  ;; Partial parse position marker
	  nil
	  ;; Partial parse state
	  nil))

(defmacro notmuch-sexp--depth (sp)         `(aref ,sp 1))
(defmacro notmuch-sexp--partial-pos (sp)   `(aref ,sp 2))
(defmacro notmuch-sexp--partial-state (sp) `(aref ,sp 3))

(defun notmuch-sexp-read (sp)
  "Consume and return the value at point in the current buffer.

Returns 'retry if there is insufficient input to parse a complete
value (though it may still move point over whitespace).  If the
parser is currently inside a list and the next token ends the
list, this moves point just past the terminator and returns 'end.
Otherwise, this moves point to just past the end of the value and
returns the value."

  (skip-chars-forward " \n\r\t")
  (cond ((eobp) 'retry)
	((= (char-after) ?\))
	 ;; We've reached the end of a list
	 (if (= (notmuch-sexp--depth sp) 0)
	     ;; .. but we weren't in a list.  Let read signal the
	     ;; error to be consistent with all other code paths.
	     (read (current-buffer))
	   ;; Go up a level and return an end token
	   (decf (notmuch-sexp--depth sp))
	   (forward-char)
	   'end))
	((= (char-after) ?\()
	 ;; We're at the beginning of a list.  If we haven't started
	 ;; a partial parse yet, attempt to read the list in its
	 ;; entirety.  If this fails, or we've started a partial
	 ;; parse, extend the partial parse to figure out when we
	 ;; have a complete list.
	 (catch 'return
	   (when (null (notmuch-sexp--partial-state sp))
	     (let ((start (point)))
	       (condition-case nil
		   (throw 'return (read (current-buffer)))
		 (end-of-file (goto-char start)))))
	   ;; Extend the partial parse
	   (let (is-complete)
	     (save-excursion
	       (let* ((new-state (parse-partial-sexp
				  (or (notmuch-sexp--partial-pos sp) (point))
				  (point-max) 0 nil
				  (notmuch-sexp--partial-state sp)))
		      ;; A complete value is available if we've
		      ;; reached depth 0.
		      (depth (first new-state)))
		 (assert (>= depth 0))
		 (if (= depth 0)
		     ;; Reset partial parse state
		     (setf (notmuch-sexp--partial-state sp) nil
			   (notmuch-sexp--partial-pos sp) nil
			   is-complete t)
		   ;; Update partial parse state
		   (setf (notmuch-sexp--partial-state sp) new-state
			 (notmuch-sexp--partial-pos sp) (point-marker)))))
	     (if is-complete
		 (read (current-buffer))
	       'retry))))
	(t
	 ;; Attempt to read a non-compound value
	 (let ((start (point)))
	   (condition-case nil
	       (let ((val (read (current-buffer))))
		 ;; We got what looks like a complete read, but if
		 ;; we reached the end of the buffer in the process,
		 ;; we may not actually have all of the input we
		 ;; need (unless it's a string, which is delimited).
		 (if (or (stringp val) (not (eobp)))
		     val
		   ;; We can't be sure the input was complete
		   (goto-char start)
		   'retry))
	     (end-of-file
	      (goto-char start)
	      'retry))))))

(defun notmuch-sexp-begin-list (sp)
  "Parse the beginning of a list value and enter the list.

Returns 'retry if there is insufficient input to parse the
beginning of the list.  If this is able to parse the beginning of
a list, it moves point past the token that opens the list and
returns t.  Later calls to `notmuch-sexp-read' will return the
elements inside the list.  If the input in buffer is not the
beginning of a list, throw invalid-read-syntax."

  (skip-chars-forward " \n\r\t")
  (cond ((eobp) 'retry)
	((= (char-after) ?\()
	 (forward-char)
	 (incf (notmuch-sexp--depth sp))
	 t)
	(t
	 ;; Skip over the bad character like `read' does
	 (forward-char)
	 (signal 'invalid-read-syntax (list (string (char-before)))))))

(defun notmuch-sexp-eof (sp)
  "Signal an error if there is more data in SP's buffer.

Moves point to the beginning of any trailing data or to the end
of the buffer if there is only trailing whitespace."

  (skip-chars-forward " \n\r\t")
  (unless (eobp)
    (error "Trailing garbage following expression")))

(defvar notmuch-sexp--parser nil
  "The buffer-local notmuch-sexp-parser instance.

Used by `notmuch-sexp-parse-partial-list'.")

(defvar notmuch-sexp--state nil
  "The buffer-local `notmuch-sexp-parse-partial-list' state.")

(defun notmuch-sexp-parse-partial-list (result-function result-buffer)
  "Incrementally parse an S-expression list from the current buffer.

This function consumes an S-expression list from the current
buffer, applying RESULT-FUNCTION in RESULT-BUFFER to each
complete value in the list.  It operates incrementally and should
be called whenever the input buffer has been extended with
additional data.  The caller just needs to ensure it does not
move point in the input buffer."

  ;; Set up the initial state
  (unless (local-variable-p 'notmuch-sexp--parser)
    (set (make-local-variable 'notmuch-sexp--parser)
	 (notmuch-sexp-create-parser))
    (set (make-local-variable 'notmuch-sexp--state) 'begin))
  (let (done)
    (while (not done)
      (case notmuch-sexp--state
	(begin
	 ;; Enter the list
	 (if (eq (notmuch-sexp-begin-list notmuch-sexp--parser) 'retry)
	     (setq done t)
	   (setq notmuch-sexp--state 'result)))
	(result
	 ;; Parse a result
	 (let ((result (notmuch-sexp-read notmuch-sexp--parser)))
	   (case result
	     (retry (setq done t))
	     (end   (setq notmuch-sexp--state 'end))
	     (t     (with-current-buffer result-buffer
		      (funcall result-function result))))))
	(end
	 ;; Any trailing data is unexpected
	 (notmuch-sexp-eof notmuch-sexp--parser)
	 (setq done t)))))
  ;; Clear out what we've parsed
  (delete-region (point-min) (point)))

(provide 'notmuch-parser)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; notmuch-parser.el ends here

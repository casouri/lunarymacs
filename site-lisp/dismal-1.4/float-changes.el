;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : float-changes.el
;;;; Author          : Frank Ritter
;;;; Created On      : Fri Mar 20 16:56:37 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Mon Aug 17 23:36:58 1992
;;;; Update Count    : 12
;;;; 
;;;; PURPOSE
;;;; 	Changes to float.el to support integer and floats in the same 
;;;; calculations (with corercion to floats).
;;;; TABLE OF CONTENTS
;;;; 	i.	New Variables and constants
;;;;	I.	Saved changes from old float.el
;;;;
;;;; 
;;;; Copyright 1991, Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'float-changes)

;; (if (fboundp 'proclaim-inline)
;;   (proclaim-inline
;;     float-stringp
;; ))


;;;
;;; 	i.	New Variables and constants
;;;

(defconst floating-point-leading-digits-regexp
   "^\\([0-9]*\\)\\(\\.[0-9]*\\|\\)$")

(defconst integer-regexp "^[ \t]*\\(-?\\)\\([0-9]+\\)[ \t]*$"
  "Regular expression to match integer numbers.  Exact matches:
1 - minus sign
2 - integer part
3 - end of string")

(defconst floating-point-list-regexp
  "^[ \t]*\\((quote\\)[ ]*\
\\(([0-9]+\\)[ ]+\\(\\.\\)[ ]+\
\\(-?\\)\
\\([0-9]+\\)\
))"
  "Regular expression to match old (Rosenblatt) floating point numbers as 
lists in strings.
Extract matches:
0 - leading spaces
1 - quote
2 - more spaces
3 - ( and leading number
4 - dot, spaces
5 - optional negative sign
5 - more numbers
6 - two more closing parens
")

(defconst floating-point-regexp
  "^[ \t]*\\(-?\\)\\([0-9]*\\)\
\\(\\.\\([0-9]*\\)\\|\\)\
\\(\\(\\([Ee]\\)\\(-?\\)\\([0-9][0-9]*\\)\\)\\|\\)[ \t]*$"
  "Regular expression to match floating point numbers.  Extract matches:
1 - minus sign
2 - integer part
4 - fractional part
8 - minus sign for power of ten
9 - power of ten
")


;; taken from float.el
(defsubst extract-match (str i)		; used after string-match
  (condition-case ()
      (substring str (match-beginning i) (match-end i))
    (error "")))

;(float-stringp "23.0000")
;(float-stringp ".0000")
;(float-stringp "   ")
;(float-stringp "-")
;(float-stringp "   .")
;(float-stringp "(quote (13 . -123123))")

;; (defun float-stringp (astring)
;;   (and (or (string-match floating-point-regexp astring 0)
;;            (string-match floating-point-list-regexp astring 0))
;;        (not (string-match "^[-. \t]*$" astring 0))))

;; (number-stringp "123. 2 ")
;; (setq astring " e6")
;; number-stringp 
(defsubst dismal-number-stringp (astring)
  (and (or (string-match floating-point-regexp astring 0)
           (string-match floating-point-list-regexp astring 0)
           (string-match integer-regexp astring 0))
       (not (string-match "^[-. \t]*$" astring 0))))

(defsubst integer-stringp (astring) ;(integer-stringp "23")
  (string-match integer-regexp astring 0))

;; the - may need a \\, but doesn't look like it.  3-Aug-92 -FER
; (defconst floating-point-regexp
;   "^[ \t]*\\(-?\\)\\([0-9]*\\)\
; \\(\\.\\([0-9]*\\)\\|\\)\
; \\(\\(\\([Ee]\\)\\(-?\\)\\([0-9][0-9]*\\)\\)\\|\\)[ \t]*$"
;   "Regular expression to match floating point numbers.  Extract matches:
; 1 - minus sign
; 2 - integer part
; 4 - fractional part
; 8 - minus sign for power of ten
; 9 - power of ten
; ")


;;;
;;;	I.	Saved changes from old float.el
;;;

(defconst mantissa-bits 24
  ;"Number of significant bits in this floating point representation."
)

;; other constants
(defconst maxbit (1- mantissa-bits)
  ;"Number of highest bit"
)


(defconst second-bit-mask (ash 1 (1- maxbit))
;  "Masks all bits except the highest-order magnitude bit"
)

(defun float-same-sign (a b)			; two f-p numbers have same sign?
  (not (xor (natnump (car a)) (natnump (car b)))))

(defun xor (a b)			; logical exclusive or
  (and (or a b) (not (and a b))))

(defun fashl (fnum)			; floating-point arithmetic shift left
  (cons (ash (car fnum) 1) (1- (cdr fnum))))

(defun lohalf (n)			; return low halfword
  (logand n masklo))

(setq halfword-bits (/ mantissa-bits 2)	; bits in a halfword
      masklo (1- (ash 1 halfword-bits)) ; isolate the lower halfword
      maskhi (lognot masklo)		; isolate the upper halfword
      round-limit (ash 1 (/ halfword-bits 2)))


(defun hihalf (n)			; return high halfword, shifted down
  (ash (logand n maskhi) (- halfword-bits)))


(defun ftrunc (fnum)			; truncate fractional part
 "Truncate the fractional part of a floating point number."
  (cond ((natnump (cdr fnum))		; it's all integer, return number as is
	 fnum)
	((<= (cdr fnum) (- maxbit))	; it's all fractional, return 0
	 '(0 . 1))
	(t				; otherwise mask out fractional bits
	 (let ((mant (car fnum)) (exp (cdr fnum)))
	   (normalize 
	    (cons (if (natnump mant)	; if negative, use absolute value
		      (ash (ash mant exp) (- exp))
		    (- (ash (ash (- mant) exp) (- exp))))
		  exp))))))

(defconst high-bit-mask (ash 1 maxbit)
;  "Masks all bits except the high-order (sign) bit."
)

;; Arithmetic functions
(defun f+ (a1 a2)
;  "Returns the sum of two floating point numbers."
  (let ((f1 (fmax a1 a2))
	(f2 (fmin a1 a2)))
    (if (float-same-sign a1 a2)
	(setq f1 (fashr f1)		; shift right to avoid overflow
	      f2 (fashr f2)))
    (normalize
     (cons (+ (car f1) (ash (car f2) (- (cdr f2) (cdr f1))))
	   (cdr f1)))))

(defun fmax (a1 a2)
;  "Returns the maximum of two floating point numbers."
  (if (f> a1 a2) a1 a2))

(defun fmin (a1 a2)
;  "Returns the minimum of two floating point numbers."
  (if (f< a1 a2) a1 a2))


;; not used anywhere, commented out, 2-Jan-97 -FER
;; 
;; (defun xor (a b)			; logical exclusive or
;;   (and (or a b) (not (and a b))))


;; (float-to-string 3e4)

(defsubst float-to-string (fnum &optional sci)
  "Convert the floating point number to a decimal string.
Optional second argument non-nil means use scientific notation."
  (if sci 
     (format "%e" fnum)
     (format "%s" fnum)))

(defun fabs (fnum)			; re-normalize after taking abs value
  (normalize (cons (abs (car fnum)) (cdr fnum))))

(defun fint (fnum)			; truncate and convert to integer
  "Convert the floating point number to integer, with truncation, 
like a C cast operator."
  (let* ((tf (ftrunc fnum)) (tint (car tf)) (texp (cdr tf)))
    (cond ((>= texp mantissa-bits)	; too high, return "maxint"
	   mantissa-maxval)
	  ((<= texp (- mantissa-bits))	; too low, return "minint"
	   mantissa-minval)
	  (t				; in range
	   (ash tint texp)))))

(defconst decimal-digits 6
  ;"Number of decimal digits expected to be accurate."
)

(setq _f1 '(4194304 . -22))
(setq _f1/2 '(4194304 . -23))

(setq _f0 '(0 . 1))

(setq _f10 '(5242880 . -19))

;; support for decimal conversion routines
(setq powers-of-10 (make-vector (1+ decimal-digits) _f1))
(aset powers-of-10 1 _f10)
(aset powers-of-10 2 '(6553600 . -16))
(aset powers-of-10 3 '(8192000 . -13))
(aset powers-of-10 4 '(5120000 . -9))
(aset powers-of-10 5 '(6400000 . -6))
(aset powers-of-10 6 '(8000000 . -3))

(setq all-decimal-digs-minval (aref powers-of-10 (1- decimal-digits))
      highest-power-of-10 (aref powers-of-10 decimal-digits))

(defun f- (a1 &optional a2)		; unary or binary minus
  "Returns the difference of two floating point numbers."
  (if a2
      (f+ a1 (f- a2))
    (normalize (cons (- (car a1)) (cdr a1)))))

(defun f/ (a1 a2)			; SLOW subtract-and-shift algorithm
  "Returns the quotient of two floating point numbers."
  (if (zerop (car a2))			; if divide by 0
      (signal 'arith-error (list "attempt to divide by zero" a1 a2))
    (let ((bits (1- maxbit))
	  (quotient 0) 
	  (dividend (car (fabs a1)))
	  (divisor (car (fabs a2)))
	  (sign (not (float-same-sign a1 a2))))
      (while (natnump bits)
	(if (< (- dividend divisor) 0)
	    (setq quotient (ash quotient 1))
	  (setq quotient (1+ (ash quotient 1))
		dividend (- dividend divisor)))
	(setq dividend (ash dividend 1)
	      bits (1- bits)))
      (normalize
       (cons (if sign (- quotient) quotient)
	     (- (cdr (fabs a1)) (cdr (fabs a2)) (1- maxbit)))))))

(defun f= (a1 a2)
  "Returns t if two floating point numbers are equal, nil otherwise."
  (equal a1 a2))

(defun f< (a1 a2)
  "Returns t if first floating point number is less than second,
nil otherwise."
  (not (f>= a1 a2)))

(defun f<= (a1 a2)
  "Returns t if first floating point number is less than or equal to
second, nil otherwise."
  (not (f> a1 a2)))



(defun f> (a1 a2)
  "Returns t if first floating point number is greater than second,
nil otherwise."
  (cond ((and (natnump (car a1)) (< (car a2) 0)) 
	 t)				; a1 nonnegative, a2 negative
	((and (> (car a1) 0) (<= (car a2) 0))
	 t)				; a1 positive, a2 nonpositive
	((and (<= (car a1) 0) (natnump (car a2)))
	 nil)				; a1 nonpos, a2 nonneg
	((/= (cdr a1) (cdr a2))		; same signs.  exponents differ
	 (> (cdr a1) (cdr a2)))		; compare the mantissas.
	(t
	 (> (car a1) (car a2)))))	; same exponents.

(defun f>= (a1 a2)
  "Returns t if first floating point number is greater than or equal to 
second, nil otherwise."
  (or (f> a1 a2) (f= a1 a2)))

(defun f* (a1 a2)			; multiply in halfword chunks
  "Returns the product of two floating point numbers."
  (let* ((i1 (car (fabs a1)))
	 (i2 (car (fabs a2)))
	 (sign (not (float-same-sign a1 a2)))
	 (prodlo (+ (hihalf (* (lohalf i1) (lohalf i2)))
		    (lohalf (* (hihalf i1) (lohalf i2)))
		    (lohalf (* (lohalf i1) (hihalf i2)))))
	 (prodhi (+ (* (hihalf i1) (hihalf i2))
		    (hihalf (* (hihalf i1) (lohalf i2)))
		    (hihalf (* (lohalf i1) (hihalf i2)))
		    (hihalf prodlo))))
    (if (> (lohalf prodlo) round-limit)
	(setq prodhi (1+ prodhi)))	; round off truncated bits
    (normalize
     (cons (if sign (- prodhi) prodhi)
	   (+ (cdr (fabs a1)) (cdr (fabs a2)) mantissa-bits)))))

(defun normalize (fnum)
  (if (> (car fnum) 0)			; make sure next-to-highest bit is set
      (while (zerop (logand (car fnum) second-bit-mask))
	(setq fnum (fashl fnum)))
    (if (< (car fnum) 0)		; make sure highest bit is set
	(while (zerop (logand (car fnum) high-bit-mask))
	  (setq fnum (fashl fnum)))
      (setq fnum _f0)))			; "standard 0"
  fnum)

(defun old-float-to-string (fnum &optional sci)
  "Convert the floating point number to a decimal string.
Optional second argument non-nil means use scientific notation."
  (let* ((value (fabs fnum)) (sign (< (car fnum) 0))
	 (power 0) (result 0) (str "") 
	 (temp 0) (pow10 _f1))

    (if (f= fnum _f0)
	"0"
      (if (f>= value _f1)		; find largest power of 10 <= value
	  (progn				; value >= 1, power is positive
	    (while (f<= (setq temp (f* pow10 highest-power-of-10)) value)
	      (setq pow10 temp
		    power (+ power decimal-digits)))
	    (while (f<= (setq temp (f* pow10 _f10)) value)
	      (setq pow10 temp
		    power (1+ power))))
	(progn				; value < 1, power is negative
	  (while (f> (setq temp (f/ pow10 highest-power-of-10)) value)
	    (setq pow10 temp
		  power (- power decimal-digits)))
	  (while (f> pow10 value)
	    (setq pow10 (f/ pow10 _f10)
		  power (1- power)))))
					  ; get value in range 100000 to 999999
      (setq value (f* (f/ value pow10) all-decimal-digs-minval)
	    result (ftrunc value))
      (if (f> (f- value result) _f1/2)	; round up if remainder > 0.5
	  (setq str (int-to-string (1+ (fint result))))
	(setq str (int-to-string (fint result))))

      (if sci				; scientific notation
	  (setq str (concat (substring str 0 1) "." (substring str 1)
			    "E" (int-to-string power)))

					  ; regular decimal string
	(cond ((>= power (1- decimal-digits))
					  ; large power, append zeroes
	       (let ((zeroes (- power decimal-digits)))
		 (while (natnump zeroes)
		   (setq str (concat str "0")
			 zeroes (1- zeroes)))))

					  ; negative power, prepend decimal
	      ((< power 0)		; point and zeroes
	       (let ((zeroes (- (- power) 2)))
		 (while (natnump zeroes)
		   (setq str (concat "0" str)
			 zeroes (1- zeroes)))
		 (setq str (concat "0." str))))

	      (t				; in range, insert decimal point
	       (setq str (concat
			  (substring str 0 (1+ power))
			  "."
			  (substring str (1+ power)))))))

      (if sign				; if negative, prepend minus sign
	  (concat "-" str)
	str))))

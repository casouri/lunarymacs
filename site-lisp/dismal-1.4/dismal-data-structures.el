;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              
;;;; File            : dismal-data-structures.el
;;;; Authors         : Frank E. Ritter, ritter@cs.cmu.edu
;;;; Created On      : 14 May 94
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: 14 May 94
;;;; Update Count    : 1
;;;; 
;;;; PURPOSE
;;;;     DISMAL - Dis Mode Ain't Lotus.
;;;; 	Spreadsheet program for gnu-emacs.
;;;;
;;;; TABLE OF CONTENTS
;;;;	i.	Requires and provides
;;;;	vii.	Data structures
;;;;	xi.	Preliminary macros and
;;;;		Debugging functions
;;;;
;;;; Copyright 1993, David Fox & Frank Ritter.
;;;; Bug testing (incidental) and some fixes by bob@gnu.ai.mit.edu 
;;;; and altmann@cs.cmu.edu
;;;; 
;;;; Formated in a modified Milnes style, based on
;;;; Oman & Cook, Typographic style is more than cosmetic, CACM, 33, 506-520. 


;;;
;;;	i.	Requires and provides
;;;

(provide 'dismal-data-structures)

;;;
;;;	vii.	Data structures
;;;

;; Column format macros:

(defmacro col-format-width (f) (list 'aref f 0))
(defmacro col-format-decimal (f) (list 'aref f 1))
(defmacro col-format-alignment (f) (list 'aref f 2))

(defmacro set-col-format-width (f val) (list 'aset f 0 val))
(defmacro set-col-format-decimal (f val) (list 'aset f 1 val))
(defmacro set-col-format-alignment (f val) (list 'aset f 2 val))

;; Address accessor functions:  these are cons of row and col numbers
(defmacro dismal-make-address (r c) (list 'cons r c))
(defmacro dismal-address-row (address) (list 'car address))
(defmacro dismal-address-col (address) (list 'cdr address))

(defsubst dismal-addressp (arg)
   (and (consp arg)
        (numberp (car arg))
        (numberp (cdr arg))))

;; Cells: these can be changed by insertion other cells
;; Cell accessor functions:  these are cells that can be relative addresses
(defconst dismal-cell-types '(dismal-r-c- dismal-rfc- dismal-r-cf dismal-rfcf))

(defmacro dis-cell-row (cell) (list 'nth 1 cell))
(defmacro dis-cell-col (cell) (list 'nth 2 cell))

(defsubst dismal-cellp (arg)
   (and (listp arg) 
        (= (length arg) 3)
        ;; could add tests here for valus of cell
        (memq (car arg) dismal-cell-types)))

;; redefined in dismal.el
(defvar dismal-max-row 0)
(defvar dismal-max-col 0)


;; Range accessor functions:
;; (setq a (make-range 2 3 4 5))

(defun make-range (start-row start-col end-row end-col)
   (setq end-row (min end-row dismal-max-row))
   (setq end-col (min end-col dismal-max-col))
   (` (dismal-range (dismal-r-c- (, start-row) (, start-col))
                    (dismal-r-c- (, end-row)
                                 (, end-col)))))

(defmacro range-1st-cell (range)  (list 'nth 1 range))
(defmacro range-2nd-cell (range)  (list 'nth 2 range))
(defmacro range-1st-row (range)   (list 'cadr (list 'range-1st-cell range)))
(defmacro range-1st-col (range)   (list 'caddr (list 'range-1st-cell range)))
(defmacro range-2nd-row (range)   (list 'cadr (list 'range-2nd-cell range)))
(defmacro range-2nd-col (range)   (list 'caddr (list 'range-2nd-cell range)))

(defvar dismal-range 'dismal-range)

(defsubst rangep (arg)
  (and (listp arg)
       (eq (car arg) 'dismal-range)
       (= (length arg) 3)))

;; Range-buffer accessor functions:
(defmacro range-buffer-length (range-buffer) (list 'aref range-buffer 0))
(defmacro range-buffer-width (range-buffer) (list 'aref range-buffer 1))
(defmacro range-buffer-matrix (range-buffer) (list 'aref range-buffer 2))
(defmacro range-buffer-set-rows (rb rows) (list 'aset rb 0 rows))
(defmacro range-buffer-set-cols (rb cols) (list 'aset rb 1 cols))

;;;
;;;	xi.	Preliminary macro(s)
;;;

;; used for troubleshooting
(defmacro my-message (&rest body)
 (` (and (boundp 'my-debug) my-debug
         (progn (message  (,@ body))
                (sit-for 2)))))

;; now in the main release of 19.34 in cl-extra.el, 28-May-97 -FER
;; I don't think as nice, but easier to debug.
;(defmacro mapc (function alist)
; (` (let ((blist (, alist)))
;     (while blist
;      (funcall (, function) (car blist))
;      (setq blist (cdr blist))    ))))


(defmacro dismal-save-excursion-quietly (&rest body)
  (` (let (  ;; (dismal-show-ruler nil)
           (old-row dismal-current-row)
           (old-col dismal-current-col)
           (old-hscroll (window-hscroll))
           (old-window (selected-window)))
       (progn (,@ body))
       (dismal-jump-to-cell-quietly
                            (if (< old-row dismal-max-row)
                                old-row
                              dismal-max-row)
                            (if (< old-col dismal-max-col)
                                old-col
                              dismal-max-col))
       (set-window-hscroll old-window old-hscroll))))

(defmacro dismal-save-excursion (&rest body)
  (` (let ( ;; (dismal-show-ruler nil) ; autoshowing ruler is too slow
           (old-row dismal-current-row)
           (old-col dismal-current-col)
           (old-hscroll (window-hscroll))
           (old-window (selected-window)))
       (progn (,@ body))
       (dismal-jump-to-cell (if (< old-row dismal-max-row)
                                old-row
                              dismal-max-row)
                            (if (< old-col dismal-max-col)
                                old-col
                              dismal-max-col))
       (set-window-hscroll old-window old-hscroll))))

(defmacro dismal-eval (object)
  ;; If object has no value, print it as a string.
  (` (if (or (stringp (, object)) (listp (, object)) 
             ;; put back in ;; 13-Jul-92 -FER, so qreplace can work on numbers
             (numberp (, object)) 
             (and (symbolp (, object)) (boundp (, object))))
         (eval (, object))
       (prin1-to-string (, object)))))
         
(defmacro dismal-mark-row () 
  '(let ((result (aref dismal-mark 0)))
     (if (numberp result) 
         result
         (error "Mark not set."))))

(defmacro dismal-mark-col () 
  '(let ((result (aref dismal-mark 1)))
     (if (numberp result) 
         result
         (error "Mark not set."))))
                              

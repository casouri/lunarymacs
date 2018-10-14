;;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic: t;-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : dismal-model-extensions.el
;;;; Author          : Frank Ritter
;;;; Created On      : Wed May 20 15:50:22 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Fri Sep 24 21:13:50 1993
;;;; Update Count    : 42
;;;; 
;;;; PURPOSE
;;;; 	Specialized extensions to dismal to support the ability to 
;;;; use dismal to align two data streams, say data, and sequential predictions.
;;;; TABLE OF CONTENTS
;;;;
;;;;	i.	Changes to variables
;;;;	I.	model-match-dc
;;;;            model-match-op
;;;;	II.	model-rate
;;;; 
;;;; Copyright 1992, Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dismal-extensions)

;;;
;;;	i.	Changes to variables
;;;


;;;
;;;	I.	model-match
;;;

(defun dis-model-match (range-list)
  "Given a cell RANGE-LIST computes the percentage of colA matched
with something in colA-1.  Only counts stuff that is in order."
  (interactive "P")
  (setq range-list (dismal-adjust-range-list range-list))
  (let* ((total 0) (matches 0))
    (dismal-do (function (lambda (row col old-result)
                  (let ((dc (dismal-get-val row col))
                        (mdc (dismal-get-val row (1- col))) )
                  (setq total (if dc (1+ total) total))
                  (setq matches
                        (if (and dc mdc)
                            (1+ matches) 
                           matches)))))
             range-list 0)
   (dis-div (float matches) (float total)) ))

;; these are not easily combined with the function abov
(defun dis-model-match-op (range-list)
  "Given a cell RANGE-LIST computes the percentage of colA matched
with something in colA-2, and col A is an operator.  Only counts stuff
that is in order."
  (interactive "P")
  (model-matcher range-list "O: "))

(defun dis-model-matcher (range-list string-test)
  (setq range-list (dismal-adjust-range-list range-list))
  (let* ((total 0) (matches 0))
    (dismal-do (function (lambda (row col old-result)
                  (let ((dc (dismal-get-val row col))
                        (mdc (dismal-get-val row (- col 2))) )
    ;;(my-message "Row: %s  dc is %s, match is %s, total %s" row dc mdc total)
                  (if (and dc (stringp dc) (or (not string-match-item)
                                               (string-match string-match-item dc)))
                     (progn
                       (setq total (1+ total))
                       (setq matches
                             (if mdc
                                 (1+ matches)
                                matches)))))))
             range-list 0)
   (dis-div (float matches) (float total)) ))


;;;
;;;	II.	dis-model-rate
;;;

;;                 #  name   matchs       start
;;                               s   type    end
;; (dis-model-rate 1 "write" "K" "A" "J"  51 516)
;; (dis-model-rate 2 "unit"  "K" "A" "J"  52 432)

;; replaced by dumping whole range into S 14-Jul-92 -FER
;; (defun dis-model-rate 
;;        (ep-number ep-name match-col sec-col type-col start-row end-row)
;;   "Computes the points of match noted in MATCH-COL.  Returns a N-tuple of 
;; corresponding (or nearest) numbers in SEC-COL and MATCH-COL, and what's in 
;; TYPE-COL.  It dumps the tuples into a buffer."
;;   (interactive "P")
;;   ;; Set up variables.
;;   (setq match-col (dismal-convert-colname-to-number match-col))
;;   (setq sec-col (dismal-convert-colname-to-number sec-col))
;;   (setq type-col (dismal-convert-colname-to-number type-col))
;;   (let ((results nil) (original-start-row start-row)
;;         (report-buffer (get-buffer-create "*SPA-Report*"))    )
;;     (my-message "starting at %s %s" start-row start-col)
;; 
;;     ;; Do each row.
;;     (while (<= start-row end-row)
;;       (let ((match (dismal-get-val start-row match-col))
;;             (type (dismal-get-val start-row type-col))
;;             (time (dismal-get-val start-row sec-col))  )
;;       (if type
;;           (progn 
;;             (if (floatp time) (setq time (fint time)))
;;             (if (floatp match) (setq match (fint match)))
;;             (push (list time match type) results) ))
;;       (setq start-row (1+ start-row))  ))
;; 
;;   ;; Write out the results.
;;   (setq results (reverse results))
;;   (pop-to-buffer report-buffer)
;;   (erase-buffer)
;;   (insert 
;;     (format 
;;       "For episode %s (%s) Match in Col %s, seconds in %s, type in %s, \
;;         From row %s to %s.\n"
;;   ep-number ep-name match-col sec-col type-col original-start-row end-row))
;;   (insert (format "Total matches: %s" (length results)))
;;   (insert "\nMatching rate: number, name, Seconds, DC's & type\n")
;;   (mapcar (function (lambda (item) 
;;    (insert (format "%s %s %s %s %s\n" ep-number ep-name (or (first item) -1)
;;                              (or (second item) -1) (third item)))))
;;           results)
;;   (setq aa results) ))

;; old version:  10-Jun-92 -FER
;    ;; Do each row.
;    (while (<= start-row end-row)
;      (let ((match (dismal-get-val start-row match-col))
;            (time (dismal-get-val start-row sec-col))
;            (dc (dismal-get-val start-row dc-col))  )
;      (if match
;          (progn (if (not dc)
;                     (let ((row start-row))
;                     (while (and (not dc) (< row dismal-max-row))
;                       (setq row (1+ row))
;                       (my-message "checking out %s %s" row end-col)
;                       (setq dc (dismal-get-val row backup-dc-col)))))
;                 (if (floatp time) (setq time (fint time)))
;                 (if (floatp dc) (setq dc (fint dc)))
;                 (push (cons time dc) results) ))
;      (setq start-row (1+ start-row))  ))


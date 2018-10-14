;;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic: t;-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : auto-aligner.el
;;;; Author          : Frank Ritter
;;;; Created On      : Wed May 20 15:50:22 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Sat Sep 25 15:42:46 1993
;;;; Update Count    : 117
;;;; 
;;;; PURPOSE
;;;; 	Specialized extensions to dismal to support aligning two sequences.
;;;; TABLE OF CONTENTS
;;;;
;;;;	i.	dis-auto-align-model variables
;;;;	I.	dis-auto-align-model
;;;;	II.	Utilities
;;;; 
;;;; Copyright 1992, Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'auto-aligner)
(require 'dismal-data-structures)
(require 'rmatrix)


;;;
;;;	i.	dis-auto-align-model variables
;;;
;;; General algorithm taken from p. 190, Card, Moran, & Newell.
;;; Extensions:  
;;;  * our predseq ends up being a list of cell references
;;;  * we don't just want to compute the final comparison, we also want
;;;    to realign
;;;  * has to be run in buffer with dismal-matrix bound

;; How to use this:
;; . turn auto-update off
;; . insert file of trace
;; . set up middle-column
;; . set up new things in dis-paired-regexps to match with (if necc)
;; . call auto-aligner

;; "regexps that match valid obs codes"

(defvar dis-paired-regexps nil 
  "*The list of paired expressions the user defines the match with.")

(defvar dis-pred-regexps (mapcar 'cdr dis-paired-regexps))
(defvar dis-obs-regexps  (mapcar 'car dis-paired-regexps))

;; keep these two around, so you can match up later...
(defvar dis-predseqresult nil)
(defvar dis-obsseqresult nil)

(defconst dis-auto-aligner-version "1.1 of 8-10-93")
;; 1.1 has improved moving up in lineness


;;;
;;;	I.	auto-align-model variables
;;;
;;; (dis-auto-align-model "B" "J" 82 498) ; for unit
;;; (dis-auto-align-model "D" "M" 64 380) ; for old unit
;;; (dis-auto-align-model "B" "J" 64 380) ; for unit
;;; (dis-auto-align-model "B" "J" 50 555) ; for array
;;; (dis-auto-align-model "B" "J" 50 640) ; for array5, after auto-align
;;; (dis-auto-align-model "B" "J" 50 640) ; for array5, after auto-align
;;; (dis-auto-align-model "B" "J" 42 222) ; for precision
;;; (dis-auto-align-model "B" "J" 42 222) ; for axis

(defun dis-auto-align-model (obs-col pred-col start-row end-row)
  "Aligns the two meta-column rows based on matching up what's in OBS-COL 
and PRED-COL, doing it for all rows between START-ROW and END-ROW.
dis-paired-regexps defines what matches between the rows."
  (interactive "sSubject column to align with (a letter): 
sModel column to align with (a letter): 
nStarting row: 
nEnding row: ")
 (if (not (y-or-n-p (format "Align col %s to col %s, from row %s to row %s? "
                            obs-col pred-col start-row end-row)))
     nil
 (if (or (not (boundp 'dismal-matrix)) (not dismal-matrix))
     (error "dis-auto-align-model can only be called in a dismal buffer")
 (message "Setting up alignment...")
 ;; set up the individual items you'll match on each side
 (setq obs-regexps  (mapcar 'car dis-paired-regexps))
 (setq pred-regexps (mapcar 'cdr dis-paired-regexps))
 ;; put these variables into a let after debug
 (setq obs-col (dismal-convert-colname-to-number obs-col))
 (setq pred-col (dismal-convert-colname-to-number pred-col))
 (setq obs-range-list
       (make-range start-row obs-col end-row obs-col))
 (setq pred-range-list (make-range start-row pred-col end-row pred-col))
 (setq obsseq (dis-match-list obs-range-list obs-regexps))
 (setq predseq (dis-match-list pred-range-list pred-regexps))

 ;; Step 1.  Initialize
 (setq obslength (length obsseq))
 (setq predlength (length predseq))
 (setq score (matrix-create))
 ;; fill score's edges with 0's
 (setq i 0)
 (while (<= i predlength)
   (matrix-set score i 0 0)
   (setq i (1+ i)))
 (setq j 0)
 (while (<= j obslength)
   (matrix-set score 0 j 0)
   (setq j (1+ j)))

 ;; Step 2. Compute the scores for a matrix with one row for every operator
 ;; in the predicted sequence and one column for every operator in the
 ;;  observed sequence.
 (message 
   "Computing score matrix for %s observed actions by %s predicted actions..."
    obslength predlength)
 (setq i 1)
 (while (<= i predlength)
   (setq j 1)
   (while (<= j obslength)
     (if (dis-auto-align-test i j predseq obsseq)
         (matrix-set score i j (1+ (matrix-ref score (1- i) (1- j))))
         ; else
         (matrix-set score i j
                     (max (matrix-ref score (1- i) j)
                          (matrix-ref score i (1- j)))))
     (setq j (1+ j)))
   (setq i (1+ i)))

 ;; Step 3.  Traverse the matrix forward along the path of higest scores
 ;; but do it front first...
 (message "Computing best match...")
 (setq max-seq-length (matrix-ref score predlength obslength))
 (setq length-of-result (+ max-seq-length (- predlength max-seq-length) (-  obslength max-seq-length)))
 (dis-card-compute-best-match)
 (setq match-amount (/ (* 100 (- (+ predlength obslength) length-of-result)) ;; # of matches
                       (max 1 predlength obslength)))
 (setq optimistic-match-amount
       (/ (* 100 (- (+ predlength obslength) length-of-result)) ;; # of matches
          (max 1 (min predlength obslength))))
 ;; equivalent formula:
 ;; (/ (* 100 (matrix-ref score predlength obslength))
 ;;    length-of-result)
 ;;   e.g.,   10 & 10, 8 matches + 4, or 12 total
 ;;           10 & 10, 1 match, +18
 (beep t)
 (if (y-or-n-p (format "Do you want matches (%s %% matched) moved up in line? " 
                       match-amount))
     (dis-move-references-forward obs-col pred-col))

 ;; Step 4a.  Approve the matches you have found
 (dis-choose-to-do-edits)

 ;; Step 5.  Generate a report
  (save-excursion
  (let* ((old-buffer (current-buffer))
         (old-buffer-file-name buffer-file-name)
         (new-buffer (get-buffer-create "*auto-align-model Output*")) )
    (set-buffer new-buffer) 
    (goto-char (point-max))
    (if (not (= (point) 0)) 
        (insert "*********************************************************\n"))
    (save-excursion
      (insert "dis-auto-align-model Output " dis-auto-aligner-version "\n"
              (format "Aligned col %s to col %s, from row %s to %s \n"
                      obs-col pred-col start-row end-row))
      (insert "For file: " (or old-buffer-file-name 
                               (buffer-name)) "\n" (current-time-string) "\n"
              "\nMatching pairs:\n")
      (mapc (function (lambda (pair) (insert (format "%s  to  %s\n" (car pair)
                                                     (cdr pair)))))
            dis-paired-regexps)
      (insert (format "\nMatch = %s %%\n" match-amount))
      (insert (format "\nOptimistic Match = %s %%\n" optimistic-match-amount))
      (insert (format "    %10s %10s  %20s  %30s\n" "Observed" "Predicted" 
                      "Obs value" "Pred value"))
      (setq i 1)
      (while  (<= i length-of-result)
         (let ((obsvalue (if (aref obsseqresult i)
                             (save-excursion
                             (set-buffer old-buffer)
                             (dismal-get-val (car (aref obsseqresult i))
                                           (cdr (aref obsseqresult i))))
                             "nil"))
               (predvalue (if (aref predseqresult i)
                             (save-excursion
                             (set-buffer old-buffer)
                              (dismal-get-val (car (aref predseqresult i))
                                           (cdr (aref predseqresult i))))
                               "nil")))
         (let ((obs-value (aref obsseqresult i))
               (pred-value (aref predseqresult i)))
         (insert (format "%2s: " i)
                 (if obs-value 
                      (format "%6s%4s "  (dismal-convert-number-to-colname (cdr obs-value)) (car obs-value))
                      (format "%10s " obs-value))
                 (if pred-value 
                      (format "%6s%4s "  (dismal-convert-number-to-colname (cdr pred-value)) (car pred-value))
                      (format "%10s " pred-value))
                 (format " %15s  %25s\n"
                          (substring obsvalue 0 (min 20 (length obsvalue)))
                          (substring predvalue 0 (min 30 (length predvalue))))))
         (setq i (1+ i))))
      (display-buffer new-buffer)
      (pop-to-buffer new-buffer)
      (pop-to-buffer old-buffer))))

 ;; Step 6.  Align the matches you have found
 (if (not dis-middle-col)
     (error
   "Can't tell where data stops/model begins: you must set dis-middle-col"))

 (if (not (y-or-n-p (format "Do you want the %s matches out of %s aligned? "
                            max-seq-length (max predlength obslength))))
      nil
   (setq new-rows (dis-align-columns))
   (beep t)
   ;; Now go through and delete any completely blank rows 
   (if (not (y-or-n-p (format "Do you want blank lines deleted? ")))
       nil
     (dis-delete-blank-rows start-row (+ end-row new-rows))))
 (message "Thank you for using dis-auto-align-model.")   )))


;;; 
;;;	II.	Utilities 
;;;

;; (string-match "place-atom \\([0-9]*\\), \\1" "place-atom 33, 33")
;; move the edit references so that they are later.
;; vaiables should be put into a lets
;; ** assumes working with columns 1 and 9 ****
(defun dis-move-references-forward (obs-col pred-col)
  ;; low-post-number is where to start looking for something to move
  ;; observed on LHS, predicted nominally on RHS
  (let ((i 1))
    (setq i 1)
    ;; (setq obs-col 1)
    ;; (setq pred-col 9)
  (while (< i (1- (length predseqresult)))
    ;; Set up
    (setq my-quit-flag nil)
    (setq obs-result (aref obsseqresult i))
    (setq pred-result (aref predseqresult i))
    (if (not (and obs-result pred-result))
        nil ;; quit, he's not paired so can't move, rest in progn
    (setq obs-val (dismal-get-val (car obs-result) (cdr obs-result)))
    (setq obs-match-string
          (car (dis-matching-regexp obs-val dis-paired-regexps)))
    (setq pred-match-string 
          (cdr (dis-matching-regexp obs-val dis-paired-regexps)))
    (setq low-post-number (1+ i))

    ;; (my-message "Doing now ** i= %s, lowpost= %s, flag= %s  finali= %s" 
    ;;            i low-post-number my-quit-flag (length predseqresult))
    ;; Search
    (while (and (not my-quit-flag)
                (setq new-obs-result (aref obsseqresult low-post-number))
                (< low-post-number (length predseqresult)))
      ;; (message "Doing i= %s, lowpost= %s, flag= %s  finali= %s" 
      ;;           i low-post-number my-quit-flag (length predseqresult))
      ;; (beep t) (sit-for 2)
      (setq new-obs-val (dismal-get-val (car new-obs-result) obs-col))
      (setq new-pred-result (aref predseqresult low-post-number))
      ;; find a colleague who will move to your place
      (cond ((and new-obs-val
                 (string-match obs-match-string new-obs-val)
                 (not new-pred-result))
             (message "Moving %s at %s matching %s to %s"
                      obs-match-string obs-result pred-result new-obs-result)
             (sit-for 1)
             (aset predseqresult i nil)
             (aset predseqresult low-post-number pred-result)
             (setq my-quit-flag t))
            ((and new-obs-val
                 new-pred-result)
             (setq my-quit-flag t))
            (t (setq low-post-number (1+ low-post-number))))))
    (setq i (1+ i))  )))

;;     (if (= i 1)
;;         (setq low-post-number min-row)
;;       (setq low-post-number (1+ (car (aref obsseqresult (1- i))))))
;;     (if (= i (length predseqresult))
;;         (setq high-post-number max-row)
;;       (setq max-post-number (1+ (car (aref obsseqresult (1+ i))))))

;; Compute the best match, starting from the front
;; k is length of match since we use position 0 in array
(defun dis-card-compute-best-match ()
 ;; predseq comes in as a global
 ;; obsseq comes in as a global

 ;; Counters into final sequences
 (setq p predlength) ; counter into predicted sequence
 (setq o obslength) ; counter into observed sequence        
 (setq k (- (+ predlength obslength) max-seq-length))
 ;; The results
 ;; add 1 to k, arrays are 0 based reference
 (setq predseqresult (make-vector (1+ k) nil))
 (setq obsseqresult (make-vector (1+ k) nil))

 (while (not (and (= p 0) (= o 0)))
   (cond ((and (not (= p 0))
               (or (= o 0) (> (matrix-ref score (1- p) o)
                              (matrix-ref score (1- p) (1- o)))))
          (aset predseqresult k (nth (1- p) predseq))
          (aset obsseqresult k nil)
          (setq p (1- p))
          (setq k (1- k)))
         ((and (not (= o 0))
               (or (= p 0) (> (matrix-ref score p (1- o)) 
                              (matrix-ref score (1- p) (1- o)))))
          (aset predseqresult k nil)
          (aset obsseqresult  k (nth (1- o) obsseq))
          (setq o (1- o))
          (setq k (1- k)))
         (t
          (aset predseqresult k (nth (1- p) predseq))
          (aset obsseqresult  k (nth (1- o) obsseq))
          (setq p (1- p))
          (setq o (1- o))
          (setq k (1- k))))
  ))


;; (my-message "offset: %s matched %s" offset matched)
;; (my-message "offset: %s matched %s i-test: %s j-test %s" 
;;             offset matched i-test j-test)
;; (y-or-n-p (format "Doing real test on %s %s, scores: %s %s %s %s "
;;                   i-test j-test score0 score+j score+i score+i+j))



; (dis-align-columns 19 11)
;; needs cleaned up
(defun dis-align-columns ()
 ;; Align the pred-row with the obs-row looking across dis-middle-col
 ;; returns how many rows it added
 ;; Assumes done from front at initial time, easiest, maybe not best
 ;; assumes that cells below lowest of p-row and o-row aren't aligned
 (setq i 0)
 (setq p-offset 0)
 (setq o-offset 0)
 (while (<= i length-of-result)
   (message "Checking position %s of %s..." i length-of-result)
   (let* ((pred-cell (aref predseqresult i))
          (obs-cell (aref obsseqresult i))
          (p-row (if pred-cell (+ p-offset (dismal-address-row pred-cell))))
          (o-row (if obs-cell (+ o-offset (dismal-address-row obs-cell))))  )
    (if (and pred-cell obs-cell)
        (let* ( (offset (abs (- p-row o-row))) )
          (message "Aligning position %s of %s..." i length-of-result)
          ;; works from front, and keeps cum. offsets for each side
          ;; so only has to do adds to one side
          (cond ((= p-row o-row) nil)
                ((> p-row o-row)      ; move o-row down
                 (setq o-offset (+ o-offset offset))
                 (dismal-insert-range-cells o-row 0 
                                            o-row dis-middle-col offset))
                ((> o-row p-row)      ; move p-row down
                 (setq p-offset (+ p-offset offset))
                 (dismal-insert-range-cells p-row (1+ dis-middle-col)
                                            p-row dismal-max-col offset))))))
   (setq i (1+ i)) )
 (max p-offset o-offset))

                 ;; this would have kept alignment
                 ;(dismal-insert-range-cells (1+ p-row) (1+ dis-middle-col)
                 ;                           (1+ p-row) dismal-max-col offset)
                 ;; this would have kept alignment
                 ;(dismal-insert-range-cells (1+ o-row) 0
                 ;                       (1+ o-row) dis-middle-col offset)


(defun dis-choose-to-do-edits ()  ;(dis-choose-to-do-edits)
 ;; Uses dynamic scoping, so watch out...
 ;; on y, do nothing, on n, remove from match, on j, quit
 ;; (my-message "entering choose-to-do-edits")
 (let ((do-edit nil) (just-do-rest nil)
       o-row p-row pred-cell obs-cell
       (i 0))
 (while (and (< i length-of-result) (not just-do-rest))
  (let* ((pred-cell (aref predseqresult i))
         (obs-cell (aref obsseqresult i))
         (p-row (if pred-cell  (dismal-address-row pred-cell)))
         (o-row (if obs-cell   (dismal-address-row obs-cell)))  )
  (if (not (and pred-cell obs-cell))
      nil
    ;; I'm happy to make user type CR to be sure.
    (dis-set-mark (dismal-address-row obs-cell) (dismal-address-col obs-cell))
    (dismal-jump-to-cell (dismal-address-row pred-cell)
                         (dismal-address-col pred-cell))
    (setq do-edit
          (dismal-read-minibuffer
             (format
    "Align match %s/%s, row %s:<%s> with row %s:<%s>? (y/n/a accept the rest)"
                     (1+ i) length-of-result
                     o-row (dismal-get-val o-row (dismal-address-col obs-cell))
                     p-row (dismal-get-val p-row (dismal-address-col pred-cell)))
             nil "y"))
    (cond ;; ((string= do-edit "b")
          ;;  (message (substitute-command-keys
	  ;;     "So look at speadsheet, exit with \\[exit-recursive-edit]"))
          ;;  (recursive-edit))
          ((string= do-edit "y"))
          ((string= do-edit "n")
           (aset predseqresult i nil)
           (aset obsseqresult i nil))
          ((string= do-edit "a")
           (setq just-do-rest t))))
   (setq i (1+ i))))  ))


(defun dis-choose-to-do-edit ()
 ;; uses dynamic scoping, so watch out...
 (setq do-edit nil)
 (while (not (or (string= do-edit "y") (string= do-edit "n")))
   (setq do-edit 
         (read-minibuffer
          (format "Should I align row %s:<%s> with row %s:<%s>? (y/n/browse) "
                   o-row (dismal-get-val o-row (dismal-address-col obs-cell))
                   p-row (dismal-get-val p-row (dismal-address-col pred-cell)))))
   (if (string= do-edit "b")
       (progn (message (substitute-command-keys
		"So look at speadsheet, exit with \\[exit-recursive-edit]"))
          (recursive-edit))))
 (string= do-edit "y"))

(defun dis-auto-align-test (i j predseq obsseq)
  ;; finds the cells that match things on dis-paired-regexps
  (let* ((predcell-ref (nth (1- i) predseq))
         (obscell-ref (nth (1- j) obsseq))
         (pred-val (dismal-get-val (car predcell-ref) (cdr predcell-ref)))
         (obs-val (dismal-get-val (car obscell-ref) (cdr obscell-ref))))
  (dis-auto-align-test-regexps pred-val obs-val dis-paired-regexps)))

(defun dis-auto-align-test-regexps (pred-val obs-val regexps)
  ;; (my-message "matchine %s %s with %s" pred-val obs-val regexps)
  (cond ((not regexps) nil)
        ((and (consp regexps) (stringp (cdr regexps)))
         (and (string-match (cdr regexps) pred-val)
              (string-match (car regexps) obs-val)))
        (t (or (dis-auto-align-test-regexps pred-val obs-val (car regexps))
               (dis-auto-align-test-regexps pred-val obs-val (cdr regexps))))))


;; (string-match "Tiny" "Boy, I am Tiny I think")
(defun dis-matching-regexp (obs-val regexps)
  (cond ((not regexps) nil)
        ((and (consp regexps) (stringp (cdr regexps)))
         (if (string-match (car regexps) obs-val)
             regexps
             nil))
        (t (or (dis-matching-regexp obs-val (car regexps))
               (dis-matching-regexp obs-val (cdr regexps))))))

;; Original version that goes greedy from the back:
;;  (setq i predlength) (setq j obslength) (setq k 1)
;;  (while (not (and (= i 0) (= j 0)))
;;   (my-message "doing k %s   i %s   j %s" k i j)
;;   (sit-for 1)
;;   (if (and (not (= i 0))
;;            (or (= j 0) (> (matrix-ref score (1- i) j)
;;                           (matrix-ref score (1- i) (1- j)))))
;;       (progn (aset predseqresult k (nth (1- i) predseq))
;;          (aset obsseqresult k nil)
;;          (setq k (1+ k))
;;          (setq i (1- i)))
;;       (if (and (not (= j 0))
;;                (or (= i 0) (> (matrix-ref score i (1- j))
;;                               (matrix-ref score (1- i) (1- j)))))
;;           (progn (aset predseqresult k nil)
;;              (aset obsseqresult k (nth (1- j) obsseq))
;;              (setq k (1+ k))
;;              (setq j (1- j)))
;;           (progn 
;;              (aset predseqresult k (nth (1- i) obsseq))
;;              (aset obsseqresult k (nth (1- j) obsseq))
;;              (setq k (1+ k))
;;              (setq i (1- i))
;;              (setq j (1- j)))  ))
;;  )


;; valiant but misplaced attempt to do card algorithm from the front, which is how it
;; worked in the first place. 8-Jul-92 -FER
;; 
;; ;; Compute the best match, starting from the front
;; ;; k is length of match since we use position 0 in array
;; (defun dis-compute-best-match ()
;;  (setq predseqresult (make-vector (+ predlength obslength) nil))
;;  (setq obsseqresult (make-vector (+ predlength obslength) nil))
;;  (setq p 0) ; counter into predicted sequence
;;  (setq o 0) ; counter into observed sequence
;;  (setq k 0) ; counter into final sequences
;;  (while (not (and (= p predlength) (= o obslength)))
;;    ;; (my-message "doing k %s i %s o %s" k i o)
;;    (setq score0 (matrix-ref score p o))       ;; score at current cell
;;    (setq score+p (matrix-ref score (1+ p) o)) ;; score at current cell(p+1,o)
;;    (setq score+o (matrix-ref score p (1+ o))) ;; score at current cell(p,1+o)
;;    (setq score+p+o (matrix-ref score (1+ p)   ;; score at current cell(p+1,1+o)
;;                                      (1+ o)))
;;    (cond ((= p predlength)   ;; Pad in obs, at edge
;;           (aset predseqresult k nil)
;;           (aset obsseqresult k (nth o obsseq))
;;           (setq k (1+ k))
;;           (setq o (1+ o)))
;;          ((= o obslength) ;; Pad in pred, at edge
;;           (aset predseqresult k (nth p predseq))
;;           (aset obsseqresult k nil)
;;           (setq k (1+ k))
;;           (setq p (1+ p)))
;;          ( ;; good match
;;           (and (= score0 score+p)           ;; this looks like 0 0
;;                (= score0 score+o)           ;;                 0 1
;;                (= (1+ score0) score+p+o))
;;           (aset predseqresult k (nth p predseq))
;;           (aset obsseqresult k (nth o obsseq))
;;           (setq k (1+ k))
;;           (setq p (1+ p)) (setq o (1+ o)))
;; 
;;          (t ;; need to pad some, search to know where to go, and then go there
;;           (setq next-good-cell (find-good-cell p o predlength obslength))
;;           (setq delta-p (- (car next-good-cell) p))
;;           (setq delta-o (- (cdr next-good-cell) o))
;;           ;; pad in p
;;           (while (> delta-p 0)
;;             (aset predseqresult k (nth p predseq))
;;             (aset obsseqresult k nil)
;;             (setq k (1+ k))
;;             (setq delta-p (1- delta-p))
;;             (setq p (1+ p)))
;;           ;; pad in o
;;           (while (> delta-o 0)
;;             (aset predseqresult k nil)
;;             (aset obsseqresult k (nth o obsseq))
;;             (setq k (1+ k))
;;             (setq delta-o (1- delta-o))
;;             (setq o (1+ o))) )) )
;;  (setq k (1- k)))
;; 
;; 
;; ;; you start at X.
;; ;;         Predicted(i)
;; ;;           0
;; ;; obs(j)  0 X
;; 
;; ;; the original
;; (defun find-good-cell (i j predlength obslength)
;;  ;; Find the next good match from cell (i j) in score matrix.
;;  ;; You know that i,j itself is not a good cell
;;  ;; You know that you don't have to look at cells less than i,j
;;  ;; This version prefers to match locally early in predicted and late in obs
;;  (let ((matched nil)
;;        (offset 1)    ;; offset of current obverse diagonal
;;        (max-offset (+ (- predlength i) (- obslength j))) )
;;    ;; Generate obverse diagonal cells
;;    (while (and (< offset max-offset) (not matched))
;;      (setq i-test i)
;;      (setq j-test (+ j offset))
;;      (while (and (>= j-test j) (not matched))
;;        ;; you haven't come up to the column across you started on, col j
;;        ;; the use of < here (rather than <=) avoids testing cells on edge
;;        ;; which can't match the (00,01) pattern.
;;        (if (and (< j-test obslength) (< i-test predlength))
;;            ;; test the cell to see if it is the next good one.
;;            (progn 
;;              (setq score0 (matrix-ref score i-test j-test))
;;              (setq score+i (matrix-ref score (1+ i-test) j-test))
;;              (setq score+j (matrix-ref score i-test (1+ j-test)))
;;              (setq score+i+j (matrix-ref score (1+ i-test) (1+ j-test)))
;;              (if (and (= score0 score+i) ;; this is a cell looking at 0 0
;;                       (= score0 score+j) ;;                           0 1
;;                       (= (1+ score0) score+i+j))
;;                  (setq matched t))))
;;        ;; Move down diagonal  |_'
;;        (setq i-test (1+ i-test))
;;        (setq j-test (1- j-test)))
;;      (setq offset (1+ offset)))
;;    ;; Return the max cell if you are in the flatlands or the cell you found.
;;    (if (= offset max-offset)
;;        (cons predlength obslength)
;;      (cons (1- i-test) (1+ j-test))) ))
;; 
;; ;; 
;; ;; you start at X.
;; ;;         Predicted(i)
;; ;;           0
;; ;; obs(j)  0 X
;; 
;; (defun find-good-cell2 (i j predlength obslength)
;;  ;; Find the next good match from cell (i j) in score matrix.
;;  ;; You know that i,j itself is not a good cell
;;  ;; You know that you don't have to look at cells less than i,j
;;  ;; This version prefers to match locally early in obs and late in predicted
;;  (let ((matched nil)
;;        (offset 1)    ;; offset of current obverse diagonal
;;        (max-offset (+ (- predlength i) (- obslength j))) )
;;    ;; Generate obverse diagonal cells
;;    (while (and (< offset max-offset) (not matched))
;;      (setq i-test (+ i offset))
;;      (setq j-test j)
;;      (while (and (>= i-test i) (not matched))
;;        ;; you haven't come up to the column across you started on, col i
;;        (if (and (< j-test obslength) (< i-test predlength))
;;            ;; test the cell to see if it is the next good one.
;;            (progn 
;;              (setq score0 (matrix-ref score i-test j-test))
;;              (setq score+i (matrix-ref score (1+ i-test) j-test))
;;              (setq score+j (matrix-ref score i-test (1+ j-test)))
;;              (setq score+i+j (matrix-ref score (1+ i-test) (1+ j-test)))
;;              (if (and (= score0 score+i) ;; this is a cell looking at 0 0
;;                       (= score0 score+j) ;;                           0 1
;;                       (= (1+ score0) score+i+j))
;;                  (setq matched t))))
;;        ;; Move down diagonal  |_'
;;        (setq i-test (1- i-test))
;;        (setq j-test (1+ j-test)))
;;      (setq offset (1+ offset)))
;;    ;; Return the max cell if you are in the flatlands or the cell you found.
;;    (if (= offset max-offset)
;;        (cons predlength obslength)
;;      ;; correct for last move
;;     (cons (1+ i-test) (1- j-test))) ))
;; 

;; don't know why we have this, seems superfluous 18-Jun-92 -FER
;(defun dis-align-range ()
;  "Align the two lines represented by the rows in the marked range,
;looking across dis-middle-col.   Returns how many rows it added."
; (interactive)
;
; (setq i 0)
; (setq p-offset 0)
; (setq o-offset 0)
; (while (< i length-of-result)
;   (message "Checking position %s of %s..." (1+ i) length-of-result)
;   (let* ((pred-cell (aref predseqresult i))
;          (obs-cell (aref obsseqresult i))
;          (p-row (if pred-cell (+ p-offset (dismal-address-row pred-cell))))
;          (o-row (if obs-cell (+ o-offset (dismal-address-row obs-cell))))  )
;    (if (and pred-cell obs-cell (dis-choose-to-do-edit))
;        (let* ( (offset (abs (- p-row o-row))) )
;          (message "Aligning position %s of %s..." (1+ i) length-of-result)
;          (cond ((= p-row o-row) nil)
;                ((> p-row o-row)      ; move o-row down
;                 ;; this would have kept alignment
;                 ;(dismal-insert-range-cells (1+ p-row) (1+ dis-middle-col)
;                 ;                           (1+ p-row) dismal-max-col offset)
;                 (setq o-offset (+ o-offset offset))
;                 (dismal-insert-range-cells o-row 0 o-row
;                                            dis-middle-col offset))
;                ((> o-row p-row)      ; move p-row down
;                 ;; this would have kept alignment
;                 ;(dismal-insert-range-cells (1+ o-row) 0
;                 ;                           (1+ o-row) dis-middle-col offset)
;                 (setq p-offset (+ p-offset offset))
;                 (dismal-insert-range-cells p-row (1+ dis-middle-col)
;                                            p-row dismal-max-col offset))))))
;   (setq i (1+ i)) )
; (max p-offset o-offset))

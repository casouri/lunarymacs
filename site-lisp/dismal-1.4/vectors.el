;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : vectors.el
;;;; Author          : David Fox, fox@cs.nyu.edu
;;;; Created On      : Mon Jan  6 14:19:40 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Mon Aug  3 16:14:59 1992
;;;; Update Count    : 31
;;;; 
;;;; PURPOSE
;;;; Vectors are arrays that on access automatically allocate space on end 
;;;; when new positions are accessed, and insert new and delete extant elements
;;;; like a list.  New space on addition is always double, so size increases
;;;; log rather than linear.
;;;; TABLE OF CONTENTS
;;;; 	|>Contents of this module<|
;;;; 
;;;; Copyright 1992, David Fox & Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vectors)


;;;
;;;	i.	Variables
;;;
;;;

(defvar vector-expansion-function (function (lambda (x) (1+ x)))
  "How much to expand vector when space is needed as a function of its 
current size.")
;; used to be (lambda (x) (* 2 x)), but this was way too big...


;;;
;;; 	I.	Vectors
;;; 

;;
;; VECTOR FUNCTIONS: A vector is implemented as a 4 tuple:
;;      [space-allocated space-in-use item-vector init-element]
;;
;; Entry points:
;;      (vector-create init)      - Create a vector, set new elems to INIT.
;;      (vector-ref v i)          - Return V's I'th element.
;;      (vector-set v i x)        - Set V's I'th element to X.
;;      (vector-insert v i n)     - Insert N new elements into V at I.
;;                                  moving things over
;;      (vector-member v x)       - return position of X, or nil
;;      (vector-delete v i n)     - Delete N elements from V at I.
;;      (vector-length v)       - Macro that returns length of V.
;;      (vec-copy-sequence-r seq) -

;; Can't be a defsubst, recursive...

(defun vec-copy-sequence-r (seq)
  "Recursive copy-sequence."
  (if seq
    (let* ((len (length seq))
           (new (make-vector len nil)))
      (while (> len 0)
        (setq len (1- len))
        (aset new len (if (sequencep (aref seq len))
                          (if (null (aref seq len))
                              nil
                            (vec-copy-sequence-r (aref seq len)))
                        (aref seq len))))
      new)))

(defsubst vector-create (init)
  "Create a vector whose elements are initialized to sequence INIT."
  (let ((v (vec-copy-sequence-r [1 0 [nil] nil])))
    (aset v 3 (vec-copy-sequence-r init))
    (aset (aref v 2) 0 (vec-copy-sequence-r init))
    v))

(defmacro vector-length (vector) 
  `(aref ,vector ,1))

(defsubst vector-expand (vector index)
  "Make sure a VECTOR has space to store into position INDEX."
  (if (>= index (aref vector 0))
      (let ((oldspace (aref vector 0))
            (newspace (aref vector 0))
            (vector-default (aref vector 3))
            (vector-cells (aref vector 2)))
        (while (<= newspace index)
            (setq newspace (funcall vector-expansion-function newspace)))
        (aset vector 2 (vconcat vector-cells
                                (make-vector (- newspace oldspace) nil)))
        (while (< oldspace newspace)
          (aset (aref vector 2) oldspace (vec-copy-sequence-r vector-default))
          (setq oldspace (1+ oldspace)))
        (aset vector 0 newspace))))

(defsubst vector-ref (vector index)
  "Return the VECTOR element at INDEX."
  ;; if index is too high or low return first element
  (if (or (>= index (aref vector 1))
          (< index 0))
      (aref vector 3)
    (aref (aref vector 2) index)))

(defsubst vector-set (vector index value)
  "Set the VECTOR element at INDEX to VALUE and return the value."
  (vector-expand vector index)
  (aset vector 1 (max (aref vector 1) (1+ index)))
  (aset (aref vector 2) index value))

;; I tried to revise this to bind the (aref vector 2), but it didn't work
;;  more time would be needed to optimize it.  I think it is using the value
;; dynamically, and cannot be bound in that way.
(defsubst vector-insert (vector index count)
  "In VECTOR at position INDEX, insert COUNT new elements."
  ;; There is some question as to what this should do if index refers
  ;; to a position past the end of the vector (it does nothing) and
  ;; what it should do if index points to the end of the vector (it
  ;; extends it.)  It might be interesting to allow the length to be
  ;; greater than the allocated space, and return the default element
  ;; if unallocated spaces were referred to (I think that happens now.)
  ;; Oh well, I should check it out, but I have other fish to fry.
  (if (<= index (aref vector 1)) ;; you are in the array's range
      (let ((oldlen (aref vector 1))
            (vector-cells (aref vector 2))
            (vector-default (aref vector 3))
            (newlen (+ count (aref vector 1))))
        (vector-expand vector (1- (+ oldlen count)))
        (while (> oldlen index)
          (setq oldlen (1- oldlen))
          (aset (aref vector 2)
                (+ oldlen count)
                (aref vector-cells oldlen)))
        (setq oldlen (+ oldlen count))
        (while (> oldlen index)
          (setq oldlen (1- oldlen))
          (aset (aref vector 2) oldlen (vec-copy-sequence-r vector-default)))
        (aset vector 1 newlen)))
  vector)

(defsubst vector-member (v object) 
 ;; returns first position of X, or nil
 (let ((spot 0)
       (length (vector-length v)))
   (while (and (<= spot length) (not (equal object (vector-ref v spot))))
       (setq spot (1+ spot)))
   (if (> spot length)
       nil
       spot)))

;;;
;;;	Vector-delete
;;;

;(defun create-vaa ()
;   (setq vaa (vector-create nil))
;   (setq i 0)
;   (while (< i 8)
;     (vector-set vaa i i)
;    (setq i (1+ i))))
;; (vector-push vaa 'a33)
;; (vector-remove vaa 33)
;; (vector-member vaa 33)

;; David left a rather bad bug in here, and I think (!) I've fixed it -FER
;; test code in case I didn't
;; (setq vaa (vector-create nil)) 
;;          = >  [1 0 [nil] nil]
;; (vector-ref vaa 3)
;; (inspect vaa)  [1 0 [nil] nil]
;; (vector-insert vaa 0 1)
;; (vector-set vaa 1 'bob)
;; (vector-set vaa 10 'bob10)
;;
;; (create-vaa)
;; (inspect vaa)[8 8 [0 1 2 3 4 5 6 7] nil]
;; (vector-delete vaa 0 3)
;; (inspect vaa)[8 5 [3 4 5 6 7 nil nil nil] nil]
;; (vector-ref vaa 6)

(defsubst vector-delete (vector index count)
  "In VECTOR at position INDEX delete COUNT elements."
  ;; keeping raw size of vector still large
  ;; not deleteing more then you have to
  (if (< index (aref vector 1))
    (progn
      (setq count (min count (- (aref vector 1) index)))
      (let* ((oldlen (aref vector 1))
             (real-vector (aref vector 2))
             (newlen (- oldlen count)))
        (aset vector 1 newlen)
        (while (and (<= index newlen) (< (+ index count) oldlen))
          ;; (message "loop1 index %s" index) (sit-for 2)
          (aset real-vector index
                (aref real-vector (+ index count)))
          (setq index (1+ index)))
        (setq index newlen)
        (while (< index oldlen)
          ;; (message "loop2 index %s" index) (sit-for 2)
          (aset real-vector index
                (vec-copy-sequence-r (aref vector 3)))
          (setq index (1+ index)))))))

   ;; redone above
;(defun vector-delete (vector start count)
;  "In VECTOR at position START delete COUNT elements."
;  (let* ((oldlen (aref vector 1))
;         (newlen (- oldlen count))
;         (i start)
;         (real-vector (aref vector 2)))
;  (if (> start (aref vector 1))
;      ()
;    (setq count (min count (- oldlen start)))  ;revise count
;    (aset vector 1 newlen)
;    (while (< i oldlen)
;      (aset real-vector i
;              (aref real-vector (+ i count)))
;      (setq i (1+ i)))
;    (setq i count)
;    (while (> i 0)
;      (aset real-vector (- oldlen i)
;              (vec-copy-sequence-r (aref vector 3)))
;      (setq i (1- i))))))



(defsubst vector-remove (vector object)
 ;; remove object once from vector
 (let ((spot (vector-member vector object)))
   (if spot
       (vector-delete vector spot 1))))

(defsubst vector-push (vector object)
 ;; add object to vector
 (vector-insert vector 0 1)
 (vector-set vector 0 object))

(defsubst vector-push-unique (vector object)
 (if (not (vector-member vector object))
     (vector-push vector object)))



(defsubst vector-mapl (function vector)
   (let ((row (aref vector 1))
        (vector-cells (aref vector 2)))
    (while (> row 0)
      (setq row (1- row))
      (apply function (aref vector-cells row) nil))))

;; (aref dismal-formula-cells 2)
;; (aref dismal-formula-cells 1)

;; this walks past the end of vectors!
;; arg!  3-Dec-97- FER
;; Optimization From: Dan Nicolaescu <done@ece.arizona.edu>, 2 Aug
;; 1997 10:20:05 -0700 (MST).  Looks clean, although it reverses the
;; order of application across the vector.  4-Nov-97-FER
;; Added guard to check that there are items in the vector. -FER

;; (defsubst vector-mapl (function vector)
;;  (if (> (aref vector 1) 0)
;;      (mapcar function  (aref vector 2))))


(defsubst vector-map-rc (function row vector)
  (let ((col (aref vector 1))
        (vector-cells (aref vector 2)))
    (while (> col 0)
      (setq col (1- col))
      (apply function (list row col) (aref vector-cells col) nil))))


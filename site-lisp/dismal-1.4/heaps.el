;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : heaps.el
;;;; Author          : David Fox, fox@cs.nyu.edu
;;;; Created On      : Mon Jan  6 14:19:10 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Thu Mar 19 10:20:50 1992
;;;; Update Count    : 3
;;;; 
;;;; PURPOSE
;;;; 	|>Describe module's purpose<|
;;;; TABLE OF CONTENTS
;;;; 	|>Contents of this module<|
;;;; 
;;;; Copyright 1992, Frank Ritter & David Fox.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'heaps)

;; (if (fboundp 'proclaim-inline)
;;  (proclaim-inline heap-insert heap-create))


;;;
;;;	IV.	Heaps
;;;

;;
;; PRIORITY QUEUE - A heap, implemented as a 4-tuple:
;;      [compare-function
;;       vector-size
;;       element-count
;;       element-vector]
;;
;; Entry points:
;;      (heap-create compare-function)  - create an empty heap
;;      (heap-insert heap element)      - insert an element
;;      (heap-deletemin heap)           - delete and return smallest
;;      (heap-empty heap)               - empty heap predicate
;;      (heap-space heap)               - amount of space in heap
;;      (heap-last heap)                - amount of space used, 
;;                                        address of last element

(defmacro heap-compare (h a b)
  "Use HEAP's compare function to compare elements A and B."
  (list 'apply (list 'aref h 0) a b nil))

(defmacro heap-space (h)
  "Return the amount of space available in HEAP's vector."
  (list 'aref h 1))

(defmacro heap-last (h) 
  "Return the index of the element after the HEAP's last element."
  (list 'aref h 2))

(defmacro heap-set-space (h v) (list 'aset h 1 v))
(defmacro heap-set-last (h v) (list 'aset h 2 v))

(defmacro heap-aref (h n)
  "Return the HEAP's Nth element."
  (list 'aref (list 'aref h 3) n))

(defmacro heap-aset (h n v)
  "Set the HEAP's Nth element to V."
  (list 'aset (list 'aref h 3) n v))

(defmacro heap-empty (h)
  "Return t if HEAP is empty."
  (list '= (list 'heap-last h) 0))

(defsubst heap-swap (h i j)
  "Swap HEAP's I'th and J'th elements."
  (let ((elem1 (heap-aref h i))
        (elem2 (heap-aref h j)))
    (heap-aset h i elem2)
    (heap-aset h j elem1)))

(defsubst heap-create (compare-function)
  "Create an empty priority queue (heap) with the given COMPARE-FUNCTION."
  (let ((heap (make-vector 4 nil)))
    (aset heap 0 compare-function)
    (heap-set-space heap 1)
    (heap-set-last heap 0)
    (aset heap 3 (make-vector 1 nil))
    heap))

(defsubst heap-bubble-up (heap index)
  "Helping function for heap-insert."
  (if (> index 0)
      (let* ((half (/ (1- index) 2))
             (elem (heap-aref heap index))
             (parent (heap-aref heap half))
             (comp (heap-compare heap parent elem)))
        (if (<= comp 0)
            ()
          (heap-aset heap index parent)
          (heap-aset heap half elem)
          (heap-bubble-up heap half)))))


(defsubst heap-insert (heap element)
  "Usage: (heap-insert heap element) Insert ELEMENT into HEAP."
  ;; if there is no space, grow the heap doubling it
  (if (= (heap-space heap) (heap-last heap))
      (progn
        (aset heap 3 (vconcat (aref heap 3)
                              (make-vector (heap-space heap) nil)))
        (heap-set-space heap (+ (heap-space heap)
                                          (heap-space heap)))))
  ;; Check to see if element is in heap
  ;; there may be a smarter way, but this will work
  (if (heap-member element heap)
      nil ;; duplicate caught
  ;; Else
    ;; Put the new element in the next free position in the heap vector
    (heap-aset heap (heap-last heap) element)
    ;; Increment the element count
    (heap-bubble-up heap (heap-last heap))
    (heap-set-last heap (1+ (heap-last heap)))))

(defun heap-deletemin (heap)
  "Delete and return the minimum element from the HEAP."
  (if (heap-empty heap)
      nil
    (heap-set-last heap (1- (heap-last heap)))
    (let* ((minelem (heap-aref heap 0))
           (lastelem (heap-aref heap (heap-last heap))))
      (heap-aset heap 0 lastelem)
      (heap-bubble-down heap 0)
      minelem)))

(defsubst heap-index-of-min (heap i j)
  "Given a HEAP and two indices I and J, return the index that points
to the lesser of the corresponding elements."
  (if (> 0 (heap-compare heap (heap-aref heap i) (heap-aref heap j))) i j))
      
(defun heap-bubble-down (heap index)
  "Helper function for heap-deletemin."
  (let* ((leftindex (+ index index 1))
         (rightindex (+ leftindex 1))
         (minchild))
    (if (>= leftindex (heap-last heap)) ; if no left child
        ()
      (if (>= rightindex (heap-last heap)) ; if no right child
          (setq minchild leftindex)
        (setq minchild (heap-index-of-min heap leftindex rightindex)))
      (if (not (= (heap-index-of-min heap index minchild) minchild))
          ()
        (heap-swap heap index minchild)
        (heap-bubble-down heap minchild)))))

(defun heap-member (element heap)
  "Return t if element is in heap."
  ;; assume that heap is a heap  
  ;; brute force (should be faster with a binary search, as in  a HEAP!
  (if (heap-empty heap) nil
  (let ((heap-compare-fun (aref heap 0))
        (i 0) (result nil)
        (last (heap-last heap)))
     (while (and (< i last) (not result)) 
        (if (= 0 (funcall heap-compare-fun element (heap-aref heap i)))
            (setq result t))
        (setq i (+ 1 i)))
      result)))

;; (heap-member '(2 . 2) a)

;; Some test code:
;;
;;  (setq dismal-invalid-heapA (heap-create 'dismal-address-compare))
;;  (setq dismal-invalid-heap dismal-invalid-heapA)
;;  
;;  (heap-empty dismal-invalid-heap)
;;  (heap-insert dismal-invalid-heap (cons 2 0))
;;  (heap-insert dismal-invalid-heap (cons 3 0))
;;  (heap-insert dismal-invalid-heap (cons 4 0))
;;  (heap-insert dismal-invalid-heap (cons 4 0))
;;  (heap-insert dismal-invalid-heap (cons 3 0))
;;  (heap-insert dismal-invalid-heap (cons 2 0))
;;  
;;  (setq addr (heap-deletemin dismal-invalid-heap))

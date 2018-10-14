;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : rmatrix.el
;;;; Author          : David Fox, fox@cs.nyu.edu
;;;; Created On      : Mon Jan  6 14:17:56 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Sat Sep  5 03:53:36 1992
;;;; Update Count    : 43
;;;; 
;;;; PURPOSE
;;;; Provides matrixs that are implemented as vector of vectors, along
;;;; with common matrix operations.  This version gives rows priority.
;;;; 	
;;;; TABLE OF CONTENTS
;;;; 	I.	Matrix creation and major modification
;;;;	II.	Cell insertion and deletion
;;;;	III.	Mapping functions
;;;;	IV.	Useful test functions
;;;; 
;;;; Copyright 1992, David Fox & Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rmatrix)

;;  (if (fboundp 'proclaim-inline)
;;    (proclaim-inline
;;      matrix-create
;;      matrix-delete-rows
;;      matrix-delete-cols  cut in after debugged
;;      matrix-insert-nil-cols
;;      matrix-insert-nil-row-cells
;;      matrix-insert-nil-column-cells
;;      matrix-delete-row-cells
;;      matrix-delete-column-cells
;;      matrix-mapl
;;      matrix-map-rc
;;      ;; matrix-copy
;;      matrix-funcall-rc
;;      matrix-insert-nil-rows
;;      matrix-ref
;;      matrix-set))


;;;
;;; 	I.	Matrix creation and major modification
;;;

;; Matrixs look like:   [rows-allocated rows-in-use row-vector]
;;
;; Each row is an array.  When new cells are accessed, the matrix is 
;; grown dynamically.  Only the row accessed is grown, and it
;; is only grown far enough out to hold the new item.
;;
;; Entry points:
;;   (matrix-width m)                  - width of matrix (macro)
;;   (matrix-set m r c x)              - Set M's element at (R C) to X.
;;   (matrix-ref m r c)                - Return M's element at (R C).
;;   (matrix-mapl f m)                 - Pass every cell to a function
;;   (matrix-map-rc f m)               - Pass every cell and its address to f
;;   (matrix-funcall-rc f r1 c1 r2 c2 m) 
;;                                     - Map FUNCTION across the cells of MATRIX 
;;                                       starting and stopping (inclusive) as
;;                                       indicated.  FUNCTION gets funcalled with 
;;                                       args (r c matrix-value).
;;   (matrix-insert-nil-rows m i n)    - 
;;   (matrix-insert-nil-row-cells   
;;                          m c r n)   - Insert N nil cells at r,c, moving the
;;                                       remaining cells over.
;;   (matrix-insert-nil-column-cells  
;;                          m c r n)   - Insert N nil cells at r,c, moving the
;;                                       remaining cells down.
;;   (matrix-insert-nil-cols m i n)    - 
;;   (matrix-delete-rows m i n)        - 
;;   (matrix-delete-cols m i n)        - 
;;   (matrix-delete-column-cells m c r n) - delete n cells, moving items up 
;;   (matrix-delete-row-cells 
;;                            m c r n) - delete n cells, moving items left
;;   (matrix-copy start-r start-c
;;       stop-r  stop-c
;;       donor-start-r donor-start-c  
;;       donor   recipient)            - copy cells in donor to recipient
;;                                       starting and stopping as specified

(defmacro matrix-width (matrix) (list 'aref matrix 1))
(defmacro matrix-height (matrix) (list 'aref matrix 0))

;; length will vary based on what's in each column

(defsubst matrix-create ()
  ;; Create an empty matrix.
  (vector-create (vector-create nil)))

(defsubst matrix-ref (matrix row column)
  (let ((r (vector-ref matrix row)))
    (if r
        (vector-ref r column)
      (aref matrix 3))))

(defsubst matrix-set (matrix row column value)
  ;; Set the MATRIX element ROW, COLUMN to VALUE and return the result.
  (vector-expand matrix row)
  (let ((r (aref (aref matrix 2) row)))
    (aset matrix 1 (max (aref matrix 1) (1+ row)))
    (vector-set r column value)))

(defsubst matrix-delete-rows (matrix row nrow)
  (vector-delete matrix row nrow))

(defsubst matrix-insert-nil-rows (matrix row nrow)
  (vector-insert matrix row nrow))

(defsubst matrix-delete-cols (matrix col ncol)
  (let ((r (matrix-height matrix))
        (matrix-cells (aref matrix 2)))
    (while (> r 0)
      (setq r (1- r))
      (vector-delete (aref matrix-cells r) col ncol))))

(defsubst matrix-insert-nil-cols (matrix col ncol)
  (let ((r (matrix-height matrix))
        (matrix-cells (aref matrix 2)))
    (while (> r 0)
      (setq r (1- r))
      (vector-insert (aref matrix-cells r) col ncol))))


;;;
;;;	II.	Cell insertion and deletion
;;;

;; users of this function should note that the matrix might grow
(defsubst matrix-insert-nil-row-cells (matrix row col ncol)
   (vector-insert (aref (aref matrix 2) row) col ncol))

;; (matrix-insert-nil-column-cells dismal-matrix 2 0 2 )
;; users of this function should note that the matrix might grow

(defsubst matrix-insert-nil-column-cells (matrix row col nrow)
  (let ((max-row (aref matrix 1))) ; careful, this is not 0 based
    (while (> max-row row)  ;; copy it over
      (setq max-row (1- max-row))
      (matrix-set matrix (+ nrow max-row) col
                  (matrix-ref matrix max-row col)))
    (while (> nrow 0)  ; make cells nil (matrix expanded above)
      (setq nrow (1- nrow))
      (matrix-set matrix (+ nrow row) col nil))))

;; users of this function should note that the matrix might shrink
(defsubst matrix-delete-row-cells (matrix row col ncol)
   (vector-delete (aref (aref matrix 2) row) col ncol))

;; these matrix sets might be done leaving structure there

;; users of this function should note that the matrix might shrink
(defsubst matrix-delete-column-cells (matrix row col nrow)
  ;; delete cells in COL column, starting at ROW, moving down NROWs
  (let ((max-row (aref matrix 1)))
    (while (<= (+ nrow row) max-row)  ;; copy it over
      (matrix-set matrix row col
                  (matrix-ref matrix (+ row nrow) col))
      (setq row (1+ row)))
   (while (> nrow 0)  ; make cells nil
     ;;(message "SEtting m is % s [%s %s]" max-col
     ;;         row (- max-col ncol)) (sit-for 2)
     (matrix-set matrix (- max-row nrow) col nil)
     (setq nrow (1- nrow))) ))


;;;
;;;	III.	Mapping functions
;;;

;; maps FUNCTION across the cells of MATRIX starting and stopping (inclusive)
;; as indicated.  FUNCTION gets funcalled with args (row col matrix-value).
(defsubst matrix-funcall-rc (function mfstart-r mfstart-c mfstop-r mfstop-c
                          mfmatrix)
  (let ((mfc nil))
  (while (<= mfstart-r mfstop-r)
     (setq mfc mfstart-c)
     (while (<= mfc mfstop-c)
       (funcall function mfstart-r mfc (matrix-ref mfmatrix mfstart-r mfc))
       (setq mfc (1+ mfc)) )
     (setq mfstart-r (1+ mfstart-r)))))

(defsubst matrix-mapl (function matrix)
  (let ((max-row (1- (matrix-height matrix)))
        (matrix-cells (aref matrix 2))
        (row 0))
    (while (<= row max-row)
      (vector-mapl function (aref matrix-cells row))
      (setq row (1+ row)))))

(defsubst matrix-map-rc (function matrix)
  (let ((max-row (1- (matrix-height matrix)))
        (matrix-cells (aref matrix 2))
        (row 0))
    (while (>= max-row row)
      (vector-map-rc function row (aref matrix-cells row))
      (setq row (1+ row)))))

;; copy cells in donor to recipient starting and stopping as specified
(defun matrix-copy (don-start-r don-start-c don-stop-r don-stop-c
                    rec-start-r rec-start-c donor recipient)
  (let (don-c rec-c donor-cell)
  (while (<= don-start-r don-stop-r)
     (setq don-c don-start-c)
     (setq rec-c rec-start-c)
     (while (<= don-c don-stop-c)
       ;(message "Copying from donor[%d %d] to rec[%d %d] val: %s" 
       ;          don-start-r don-c rec-start-r rec-c
       ;          (matrix-ref donor don-start-r don-c)) (sit-for 1)
       (setq donor-cell (matrix-ref donor don-start-r don-c))
       (matrix-set recipient rec-start-r rec-c
                   (if donor-cell 
                       (vconcat donor-cell)
                     nil))
       (setq don-c (1+ don-c))
       (setq rec-c (1+ rec-c))     )
     (setq don-start-r (1+ don-start-r))
     (setq rec-start-r (1+ rec-start-r)))))

;(matrix-copy 0 0 3 3 0 0 aa bb)
; (inspect bb)


;;;
;;;	IV.	Useful test functions
;;;

;; looks like matrixes are stored as a vector of columns

;(defun create-aa ()
;  (setq aa (dismal-create-matrix))
;  (matrix-set aa 0 0 'r0c0)
;  (matrix-set aa 0 1 'r0c1)
;  (matrix-set aa 1 1 'r1c1)
;  (matrix-set aa 3 3 'r3c3)
;  (matrix-set aa 3 1 'r3c1)
;  (matrix-set aa 0 2 'r0c2)
;  (matrix-set aa 0 3 'r0c3))
;; Note that these results are different from matrix.el 
;; (which is column based).
; (inspect aa)  [1 1 [[1 0 [nil] nil]] [1 0 [nil] nil]] 
; (matrix-set aa 0 0 'r0c0)
; (inspect aa) [1 1 [[1 1 [r0c0] nil]] [1 0 [nil] nil]] 
; (matrix-set aa 0 1 'r0c1)
; (inspect aa) [1 1 [[2 2 [r0c0 r0c1] nil]] [1 0 [nil] nil]] 
; (matrix-set aa 1 1 'r1c1)
; (inspect aa)
;             [2 2 [[2 2 [r0c0 r0c1] nil][2 2 [nil r1c1] nil]][1 0 [nil] nil]] 
; (matrix-set aa 3 3 'r3c3)
; (matrix-set aa 3 1 'r3c1)
; (matrix-set aa 0 2 'r0c2)
; (matrix-set aa 0 3 'r0c3)
; (inspect aa)
;  [4 4 [[4 4 [r0c0 r0c1 r0c2 r0c3] nil]
;        [2 2 [nil r1c1] nil]
;        [1 0 [nil] nil]
;        [4 4 [nil r3c1 nil r3c3] nil]] [1 0 [nil] nil]]
; (matrix-delete-column-cells aa 0 1 1)
; (matrix-width aa)
; (inspect aa)  
;  [4 4 [[4 4 [r0c0 r1c1 r0c2 r0c3] nil]
;        [2 2 [nil nil] nil]
;        [2 2 [nil r3c1] nil]
;        [4 4 [nil nil nil r3c3] nil]] [1 0 [nil] nil]]

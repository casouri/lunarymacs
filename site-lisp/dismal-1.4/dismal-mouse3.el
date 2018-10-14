;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              
;;;; File            : dismal-mouse-nigel.el
;;;; Authors         : Nigel Jenkins, nej@cs.nott.ac.uk
;;;;                                  lpyjnej@psyc.nott.ac.uk  
;;;; Created On      : 30th April 1996
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: 1-3-97
;;;; Update Count    : ???
;;;; 
;;;; PURPOSE
;;;;    DISMAL - Dis Mode Ain't Lotus.
;;;; 	Spreadsheet program for gnu-emacs.
;;;;    
;;;;    This program supplies functionality for using a mouse inside of  
;;;;    the dismal spreadsheet.
;;;;
;;;; Bugs:
;;;; * select column is slow, probably due to how it walks the matrix
;;;;   consider using just character changes.  Not currently offered as a 
;;;;   binding.
;;;;
;;;; TABLE OF CONTENTS
;;;;
;;;;	i.	Modify `dismal-map' to cope with new mouse controls
;;;;	ii.	dismal-find-cell function
;;;;
;;;;	I.	mouse-highlight-cell-or-range bound to [down-mouse-1]
;;;;	II.	mouse-highlight-column bound to [down-mouse-2]
;;;;            Deprecated 6-Oct-96 -, can be real slow.
;;;;	III.	dismal-highlight-cell
;;;;	IV.	dismal-highlight-range
;;;;	V.	Redefinition of dismal-goto-cell to allow highlighting
;;;;
;;;;
;;;;
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	Mouse functions system for using with DISMAL spreadsheet
;;;;
;;;; Optimazations by  Mikio Nakajima <minakaji@osaka.email.ne.jp> 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;;	i.	Modify `dismal-map' to cope with new mouse controls
;;;
;;; Keymap additions to dismal-map keymap, allowing the mouse to
;;; be used with dismalfor selecting cells and ranges of cells.

(define-key dismal-map [down-mouse-1] 'dis-mouse-highlight-cell-or-range)
(define-key dismal-map [double-mouse-1] 'ignore)
(define-key dismal-map [triple-mouse-1] 'ignore)

;; These are too slow, because of how the matrix is represented, 
;; so don't offer to user.
;; (define-key dismal-map [down-mouse-2] 'dis-mouse-highlight-column)
;; (define-key dismal-map [mouse-2] 'dis-mouse-highlight-column)
;; had been mouse-yank-at-point, which is a mess with plain text

(define-key dismal-map [down-mouse-2] 'dis-mouse-highlight-cell-or-range)
(define-key dismal-map [mouse-2] 'dis-mouse-highlight-cell-or-range)
(define-key dismal-map [double-mouse-2] 'ignore)
(define-key dismal-map [triple-mouse-2] 'ignore)


(define-key dismal-map [down-mouse-3] 'dis-mouse-highlight-row)
(define-key dismal-map [mouse-3] 'dis-mouse-highlight-row)
(define-key dismal-map [double-mouse-3] 'ignore)
(define-key dismal-map [triple-mouse-3] 'ignore)



;;;
;;;	ii.	dismal-find-cell function
;;;
;;; Used to set dismal point and mark based on mouse clicks.
;;;
;;; Function give to arguments for x and y position in
;;; the window will output the dismal cell in which
;;; these mouse coordinates point to.


;; replaced 7-May-97 -FER
;; I don't know how it ever worked, clever stuff moved into new version
;;
;;  (defun dismal-find-cell ()
;;    "Find cell that the mouse is pointing to at the moment."
;;    (interactive)
;;    (setq bb (mouse-position))
;;  
;;    (save-window-excursion
;;    ;; Store position of point and mouse-position
;;    (setq x-pos (car (cdr (mouse-position)))
;;  	y-pos (cdr (cdr (mouse-position))))
;;    (setq click-pos (point))
;;    
;;    ;; read the row from the front of the column
;;    (beginning-of-line)
;;    (setq row (read (current-buffer)))
;;                    
;;    ;; set absolute values for x and y position of mouse in window
;;    (setq x-pos-abs (- click-pos (point)))
;;    (setq y-pos-abs (count-lines (point-min) (point)))
;;    
;;    ;; check for the values being out of too small or too big
;;    (if (< y-pos-abs 2)
;;        (setq y-pos-abs 2))
;;    (if (< x-pos-abs 3)
;;        (setq x-pos-abs 3))
;;    (if (> (- y-pos-abs 1) dismal-max-row)
;;        (setq y-pos-abs (+ dismal-max-row 2)))
;;    
;;    ;; goto line 2 of buffer to find column point is in
;;    (goto-line 2)
;;    
;;    ;; scan forward for point to find end of column
;;    (forward-char (+ 0 x-pos-abs))
;;    (setq x-pos-abs (+ x-pos-abs (skip-chars-forward "^+")))
;;    (setq end-point (point))
;;    
;;    ;; from here move to top line and scan backwards to find column marking
;;    (goto-line 1)
;;    (forward-char (+ 0 x-pos-abs))
;;    (skip-chars-backward "^a-zA-Z")
;;    
;;    ;; column marking found now convert into the correct number
;;    (backward-char 2)
;;    (if (equal (char-after (point)) 32)
;;        (setq col (- (char-after (+ (point) 1)) 65)) 
;;      (setq col (+ (- (char-after (+ (point) 1)) 65)
;;  		 (* 26 (- (char-after (point)) 64)))))
;;    
;;    ;; goto line beginning of line y-pos-abs and convert row marking
;;    (goto-line (1+ y-pos-abs))
;;    (if (< dismal-max-row 10)
;;  
;;        ;; if max row is less than 10 then spreadsheet is 1 char to the left 
;;        ;; and only one digit needs decoding
;;        (setq row (- (char-after (point)) 48))
;;  
;;    ;; max rox is greater than 10 so decode both digits (first maybe a space)
;;      (if (equal (char-after (point)) 32)
;;  	(setq row (- (char-after (+ (point) 1)) 48))
;;        (setq row (+ (- (char-after (+ (point) 1)) 48)
;;  		   (* 10 (- (char-after (point)) 48))))))
;;      
;;    ;; column and row of cell which mouse points to are now known 
;;    ;; leave them as the return of the defun
;;    ;; inserted a guard, for seems to get wacky values
;;    (if (> col dismal-max-col) (setq col dismal-max-col))
;;    (if (> row dismal-max-row) (setq col dismal-max-row))
;;    (cons col row)))

(defun dismal-find-cell ()
  "Find cell that the mouse is pointing to at the moment."
  (interactive)

  ;; (setq bb (mouse-position))  ;used for debugging

  (save-window-excursion

  ;; Store position of point and mouse-position
  (setq x-pos (car (cdr (mouse-position)))
	y-pos (cdr (cdr (mouse-position))))
  (setq click-pos (point))
  
  ;; Read the row from the front of the column. (!)
  (beginning-of-line)
  (setq row (read (current-buffer)))

  ;; Get the column width directly from y-pos of point
  (setq col (dismal-raw-column-to-dismal-column x-pos))
    
  ;; column and row of cell which mouse points to are now known 
  ;; leave them as the return of the defun
  ;; inserted a guard, for seems to get wacky values
  (if (> col dismal-max-col) (setq col dismal-max-col))
  (if (> row dismal-max-row) (setq col dismal-max-row))
  (cons col row)))


;;;
;;;	iii.	dis-mouse-highlight-cell-or-range bound to [down-mouse-1]
;;;
;;;  Function is bound to [down-mouse-1] in dismal-map keymap.
;;;  It allows the user to select a single cell, or drag the mouse
;;;  and select a range of cells.
;;;

(defun dis-mouse-highlight-cell-or-range ()
  "Highlights a cell or range of cells as choosen by the mouse."
  (interactive)
  (mouse-set-point last-command-event)
  ;; (setq aa last-command-event)
  ;; Grab mouse position now and highlight the current cell, store
  ;; the cell information incase a drag is performed
  (mouse-set-point last-command-event)
  ;; First, clear out old highlight.
  (dismal-add-text-properties (point-min) (point-max) (list 'face 'default))
  (setq start-drag (dismal-find-cell))

  (dismal-highlight-cell (car start-drag) (cdr start-drag))

  ;; now track the mouse to see if it either moves or the button is released
  ;; set DRAG-ON variable to true so as to track the mouse movement.
  (setq drag-on t)
  (track-mouse
    ;; optimization here from Mikio Nakajima <minakaji@osaka.email.ne.jp>
    (while drag-on
      
      ;; read an event
      (setq mouse-event (read-event))
           
      ;; work out what event was
      (cond

       ;; mouse-movement is sensed move cursor and highlight the range
       ((eq (car mouse-event) 'mouse-movement)
;; was	(goto-char (car (cdr (car (cdr mouse-event)))))
        (let ((mouse-char  (car (cdr (car (cdr mouse-event))))))
           (if (not mouse-char)
               (setq mouse-char (point-max)))
           (goto-char mouse-char))
	(setq last-drag (dismal-find-cell))
	(message (format "Range from: %s  to: %s" 
			 (dismal-cell-name (cdr start-drag)(car start-drag))
			 (dismal-cell-name (cdr last-drag)(car last-drag))))

	(dismal-highlight-range (car start-drag) (cdr start-drag)
                                (car last-drag) (cdr last-drag)))

       ;; Mouse button release at the same place it was pressed
       ;; visit cell and stop tracking motion
       ((eq (car mouse-event) 'mouse-1)
	(dismal-jump-to-cell (cdr start-drag)
			   (car start-drag))
	(setq drag-on nil
	      dismal-current-row (cdr start-drag)
	      dismal-current-column (car start-drag)))

       ;; Drag motion of mouse has been completed turn tracking off and 
       ;; highlight the selected range of cells
       ((eq (car mouse-event) 'drag-mouse-1)
	(setq drag-on nil)
        (if (or (not (boundp 'last-drag)) last-drag)
            (setq last-drag (dismal-find-cell)))
	;; make sure that start-drag is top-left corner of selection
	;; and that last-drag is the bottom-right corner of selection
	(let ((t-start-drag (cons (min (car start-drag) (car last-drag))
			       (min (cdr start-drag) (cdr last-drag))))
	      (t-last-drag (cons (max (car start-drag) (car last-drag))
			      (max (cdr start-drag) (cdr last-drag)))))

	  ;; use temporary variables then reset start-drag and last-drag
	  (setq start-drag t-start-drag
		last-drag t-last-drag))

	;; set dismal point and mark to the start and end of the range
	(dismal-set-mark (cdr start-drag) (car start-drag))
	(setq dismal-current-row (cdr last-drag)
	      dismal-current-column (car last-drag))

	(dismal-jump-to-cell dismal-current-row dismal-current-column)

	;; leave message to say what the range limits are
	(message (format "Range %s to %s has been selected."
			 (dismal-cell-name (cdr start-drag)(car start-drag))
			 (dismal-cell-name (cdr last-drag)(car last-drag))
			 )))))))


;;;
;;;	I.	dis-mouse-highlight-column bound to [down-mouse-2]
;;;
;;;        Deprecated 6-Oct-96 - can be real slow.
;;;
;;; Function is bound to [down-mouse-2] in dismap-map keymap.
;;; It highlights the column the mouse pointer is over.
;;;
;;
;; (defun dis-mouse-highlight-column ()
;;  "Highlight column that mouse button 2 has been clicked upon."
;;  (interactive)
;;  (message "Please wait selecting column......")
;;
;;  ;; set point to position of mouse on window
;;  (mouse-set-point last-command-event) ; may go as well
;;   
;;  ;; find out what colum is to be highlighted and highlight it
;;  (setq column (car (dismal-find-cell)))
;;  (dis-highlight-range column 0 column dismal-max-row)
;;  (dismal-goto-row 0 t)
;;  (dismal-goto-column column)
;;  (message (format "Column %s has been selected." column)))


;;;
;;;	II.	dis-mouse-highlight-row bound to [down-mouse-3]
;;;
;;;        Function is bound to [down-mouse-3] in dismap-map keymap.
;;;        It highlights the row the mouse pointer is over.

(defun dis-mouse-highlight-row ()
  "Highlight row that mouse button 3 has been clicked upon, and set to be 
current range."
  (interactive)
  (let (row)
  (message "Please wait selecting row......")

  ;; set point to position of mouse on window
  (mouse-set-point last-command-event)

  ;; Find out what row is to be highlighted and highlight it.
  (setq row (cdr (dismal-find-cell)))
  (dismal-highlight-range 0 row dismal-max-col row)
  (dismal-goto-row row t)
  (dismal-goto-column 0)
  ;; This sets up range
  (dismal-set-mark dismal-current-row dismal-max-col)
  (message (format "Row %s has been selected." row))))


;;;
;;;	III.	dismal-highlight-cell
;;;
;;; This function highlights the cell the mouse-pointer is over.

(defun dismal-highlight-cell (x-cell y-cell)
  "Function highlights the cell inverting the colours on screen."
  (interactive "nX-pos:\nnY-pos:")
  ;; jump to the appropriate cell 
  (dismal-goto-row y-cell t)
  (dismal-goto-column x-cell)
 
  ;; find start and end point of cell and highlight characters
  ;;(setq cell-end (1+ (point))
  ;;      cell-start (1+ (- (point) (dismal-column-width x-cell))))
  ;; used to be highlight
  (dismal-add-text-properties (1+ (- (point) (dismal-column-width x-cell)))
                              (1+ (point)) (list 'face 'underline)))



;;;
;;;	IV.	dismal-highlight-range 
;;;
;;;  This function highlights a range of cells, supplied to it in
;;;  the form of four arguments, which are x and y positions for
;;;  two opposing corners.

(defun dismal-highlight-range (x-start y-start x-end y-end)
  "Highlights a range of cells in a dismal buffer."
  (interactive "nxs\nnys\nnxe\nnye")
  ;; clear window of highlighting
  (dismal-add-text-properties (point-min) (point-max) (list 'face 'default))
  (let ((oxend x-end) (oyend  y-end)
        range-start range-end y-now)

  ;; Make sure x-start is smaller that x-end.
  (if (> x-start x-end)
      (let ((temp))
	(setq temp x-start
	      x-start x-end 
	      x-end temp)))
  ;; Make sure y-start is smaller than y-end.
  (if (> y-start y-end)
      (let ((temp))
	(setq temp y-start
	      y-start y-end 
	      y-end temp)))

  ;; Go through lines one by one highlighting the cells.
  (setq y-now y-start)
  (while (<= y-now y-end)
    
    ;; Jump to left-most cell and find start-point of cell.
    (dismal-goto-row y-now t)
    (dismal-goto-column x-start)
    (setq range-start (1+ (- (point) (dismal-column-width x-start))))
    
    ;; Jump to right-most cell and find end-point of cell.
    (dismal-goto-column x-end)
    (setq range-end (1+ (point)))
    
    ;; Now highlight line by line from range-start to range-end each line.
    (dismal-add-text-properties range-start range-end (list 'face 'underline))

    ;; Increase y-now by 1.
    (setq y-now (1+ y-now)))

    ;; now go back to where you were meant to end up
    (dismal-goto-row oyend t)
    (dismal-goto-column oxend)))


;;;
;;;	V.	Redefinition of dismal-goto-cell to allow highlighting
;;;
;;;  This function is a redefinition of the original function found
;;;  in `dismal.el', it highlights the selected cell that dismal-point
;;;  is currently pointing to.

(defun dismal-goto-cell (row column interactivep)
  ;; Move cursor to the end of the cell at ROW, COLUMN.
  ;; does not set dismal-current-row, etc.
  (dismal-goto-row row interactivep)
  (dismal-goto-column column)
  (dismal-add-text-properties (point-min) (point-max) (list 'face 'default))
  (setq cell-end (point)
	cell-start (1+ (- (point) (dismal-column-width column))))
  (dismal-add-text-properties cell-start cell-end (list 'face 'underline)))
;;highlight

;; helper function

(defsubst dismal-add-text-properties (start end props &optional object)
 "Add properties while preserving the modified flag."
 (let ((original-modified-p (buffer-modified-p)))
   (add-text-properties start end props object)
    ;; don't let highlighting a cell mark it as modified.23-May-96 -FER
   (set-buffer-modified-p original-modified-p)))

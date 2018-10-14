;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic: t;-*-
;;; File to make aliases
;;; Ritter & Nichols
;;; Nov. 1994
;;;
;;; A simple way to create aliases using command name acronyming for 
;;; multi-word commands, and truncation otherwise.
;;;
;;;
;;; Table of contents
;;;
;;;	i.	Variables and initializations
;;;	I.	make-alias
;;;	II.	Some testing code.


;;;
;;;	i.	Variables and initializations
;;;

(defvar *old-aliases* nil "*Old commands that you have passed in.")
(make-variable-buffer-local '*old-aliases*)

(defvar *new-aliases* nil "*Aliases that you have created.") 
(make-variable-buffer-local '*new-aliases*)

(defvar *dup-aliases* nil "*Where possible clashes are stored.")
(make-variable-buffer-local '*dup-aliases*)

(setq dis-user-cell-functions 
      (cons 'init-make-aliases dis-user-cell-functions))

(defun init-make-aliases ()
  "Clears out the global variables for make-aliases."
  (interactive)
  (setq *old-aliases* nil)  
  (setq *new-aliases* nil)
  (setq *dup-aliases* nil)
  (message "Cleared out make-alias state."))

(defun display-dup-aliases ()
  "Prints out the duplicated aliases so far for make-aliases."
  (interactive)
  (let ((old-buffer (current-buffer))
        (dups *dup-aliases*))
    (pop-to-buffer help-buffer)
    (erase-buffer)
    (insert "Duplicate aliases stored on *dup-aliases* :\n\n")
    (mapc (function (lambda (x)
             (insert (prin1-to-string x))))
          dups)
    (goto-char (point-min))  ))


;;;
;;;	I.	make-alias
;;;

(setq dis-user-cell-functions (cons 'make-alias dis-user-cell-functions))

(defun make-alias (old) 
  "Makes an alias given an OLD command.  If OLD is less than 5 char, 
use the first character.  Else take the take the first letter of each
word.  M-x init-make-aliases must be called first."
  ;; check that old command is a string
  (if (not (stringp old)) (error "%s not a string" old))
  (let (new
        (current-cell (cons dismal-current-col dismal-current-row)))
  (setq *old-aliases* (cons (cons old current-cell) *old-aliases*))
  (cond ((> (length old) 5)
         ;; if long, take first letter of each word
         (let ((count 0))
           (setq new (substring old 0 1))
           (while (< count (length old))
             (if (string= "-" (substring old count (1+ count)))
                 (setq new (concat new (substring old (1+ count) 
                                                   (+ count 2)))))
             (setq count (1+ count)))
           (if (eq (length new) 1)
               (setq new (concat new (substring old 1 2))))))
        ;; if short, just take first letter
        (t (setq new (substring old 0 1))))
  ;; keep track of duplicate aliases
  (setq new (cons new current-cell))
  (if (match-old-alias new)
      (setq *dup-aliases* (cons new *dup-aliases*))
   (setq *new-aliases* (cons new *new-aliases*)))
  ;; return the alias
  (car new)))

;; (member (cons "nu" (cons 1 2))  (list (cons "nu" (cons 1 2)) ))

;; (member "ru" '("ru" "asdf"))

(defun match-old-alias (new)
 "Returns t is new alias matches but cell does not."
 (let ((alias (car new))
       (cell (cdr new)))
   (recursive-match-old-alias alias cell *new-aliases*)))

(defun recursive-match-old-alias (alias cell set)
 "Returns t is new alias matches but cell does not."
 (cond ((not set) nil)
       ((string= alias (car (first set)))
        (if (and (equal (car cell) (car (cdr (first set))))
                 (equal (cdr cell) (cdr (cdr (first set)))))
            (recursive-match-old-alias alias cell (cdr set))
          ;; return t
          t))
       (t (recursive-match-old-alias alias cell (cdr set)))))


;;; 
;;;	II.	Some testing code.
;;;

;; (init-make-aliases)
;; (make-alias "chunk-free-prblem-spaces")
;; *old-aliases*
;; *new-aliases*
;; *dup-aliases*






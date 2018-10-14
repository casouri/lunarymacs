;;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic: t;-*-
;;; Sarah Nichols
;;; University of Nottingham
;;; 23/XII/94
;;;
;;; Lisp functions that implement the keystroke-level model to 
;;; be incorporated into Dismal.
;;; For a full description of use, see 
;;;   Nichols, S., & Ritter, F. E. (1994).  A theoretically motivated tool
;;;   for automatically generating command aliases.  Proceedings of 
;;;   Chi '95. 393-400.
;;; 
;;;
;;; This file contains two usable functions.
;;;
;;; The first function is used to calculate the time to enter
;;; a command that given as an argument when the function is
;;; called, or entered as a value in a Dismal spreadsheet cell.  
;;; It will compute the number of mental operators needed, but it can 
;;; also take that as an optional arguement if the analyst knows that this 
;;; is higher or lower than expected.  The default algorithm give one Mop 
;;; per word separated by a dash or space.  This rule is
;;; derived from the heuristic specified by Card et al (1983, Figure 8.2
;;; rule 2).  It was specifically used when looking at Soar commands
;;; -- the structure of which meant that this approach was particularly 
;;; appropriate.  
;;;
;;; The default specifications for the variable that are included here
;;; are the length of the Mental Operator -- 1.35 seconds as specified
;;; by Card et al (1981), and the typing speed. This is in words per
;;; minute so that is can be easily altered to suit different users, and 
;;; a simple modification to the function could allow the individual's
;;; typing speed to be entered into a cell on a spreadsheet, and the
;;; value read from there.  The key-const value is a necessary constant
;;; used to convert from words per minute to average time for a keystroke
;;; in seconds, and was derived from Card et al (1981).
;;; 
;;; Two example sheets like this are included, one called keystroke4.dis, 
;;; a complete version, and one called simple-keystroke.dis, a simpler 
;;; version. 
;;;
;;;	Table of contents
;;;
;;;	i.	Variables
;;; 	I.	command-time: Time to enter a command
;;; 	II.	make-alias: Function for making Aliases 
;;;	III.	Deprecated code


;;;
;;;	i.	Variables
;;;
;;; Default specifications for variables - can be changed

(defvar mop 1.35 "Mental operator value in seconds.")

;;; Word per minute typing speed for average no-secretary expert typist
(defvar wpm 40 "Typing speed in words per minute.")

;;; Constant divisor used to calculate average time per keystroke from wpm
;; 10.80/wpm = Time (sec) per keypress (Card et al)
(defvar key-const 10.80 "Constant for comverting wpm to s/keystroke.")


;;;
;;; 	I.	command-time: Time to enter a command
;;; 
;;; Takes COMMAND (a string) as arguement, automatically calculates 
;;; mental operators by looking for hyphens, and includes average typing 
;;; speed from wpm to return time to execute command in seconds.

(setq dis-user-cell-functions 
      (cons 'klm-time dis-user-cell-functions))

(defun klm-time (command &optional nm)
  "Time to type COMMAND <SPC | CR>, including the number of mental ops
(optionally passed in)."
  (interactive)
  (if (not (stringp command)) (error "%s not a string" command))
  (let (tk ; keystroke time
        tm ; mental time
        (count 0))   ; count of chars
    ;; read wpm
    (if (= 0 wpm)
        (setq wpm (read-minibuffer "Enter typing speed: ")))
    ;; calculate tk, 10.80/wpm = Time (s) per keypress (Card et al.)
    (setq tk (* (1+ (length command))  (/  key-const wpm)))
    ;; check number of mental operators required - one per command 
    ;; and one per dash
    ;; nm - number of mental operators used
    (if (numberp nm)
         nil
       (setq nm 1) ; you get one for the command
       (while (< count (length command))
         (if (or (string= "-" (substring command count (1+ count)))
                 (string= " " (substring command count (1+ count))))
             (setq nm (1+ nm)))
           (setq count (1+ count))))
    ;; calculate Tm, time for mental ops
    (setq tm (* nm mop))
    ;; calculate keystroke value (Texecute)
    ;; return total time
    (+ tk tm) ))

;; key-val used to be used, but its name has been changed to be 
;; more clear. 

(defalias 'key-val 'klm-time)

;; (key-val "excise")
;; (key-val "exciseasdf")
;; (klm-time "kjsdhfkjsdhkfjhk")


;;;
;;; 	II.	make-alias: Function for making Aliases 
;;;
;; (moved to make-km-aliases.el, and expanded., at least by 20-Apr-96 -FER)
;;
;; (setq dis-user-cell-functions 
;;       (cons 'make-alias dis-user-cell-functions))
;; 
;; (defun make-alias (command) 
;;   "Given COMMAND, make an alias.
;; If over 5 letters, by taking the first letter of each hyphanated word in it, 
;; otherwise, just the first letter."
;;   (interactive)
;;   (if (not (stringp command)) (error "%s not a string" command))
;;   (let (new)
;;   (cond ((> (length command) 5)
;;          (let ((count 0))
;;            (setq new (substring command 0 1))
;;            (while (< count (length command))
;;              (if (string= "-" (substring command count (1+ count)))
;;                  (setq new 
;;                        (concat new (substring command 
;;                                               (1+ count) (+ count 2)))))
;;              (setq count (1+ count)))
;;            (if (eq (length new) 1)
;;                (setq new (concat new (substring command 1 2)))
;;              new)))
;; 	(t (substring command 0 1)))))
;;   


;;;
;;;	III.	Deprecated code
;;;
;;; This used to be necessary when using the functions with 
;;; earlier versions of Dismal (0.94 and earlier I think). 
;;;

;; use dismal-decp
;;(defun decp (dp flo-num) 
;;  (cond ((not (numberp flo-num)) (error "%s not correct syntax" flo-num))
;;	(t (let (dp-place1 sflo-num)
;;	     (setq sflo-num (number-to-string flo-num))
;;	     (let ((count 0))
;;	       (while (< count (length sflo-num))
;;	       (if (string= "." (substring sflo-num count (1+ count)))
;;		   (setq dp-place1 (1+ count)))
;;		 (setq count (1+ count))))
;;	     (let (dp-string)
;;	       (setq dp-string (substring sflo-num dp-place1 (+ dp dp-place1)))
;;	       (if (<= 5 (string-to-number (substring dp-string (1- dp) dp)))
;;		   (setq dp-string 
;;                       (number-to-string (1+ (string-to-number dp-string)))))
;;	       (format "%s%s" (substring sflo-num 0 dp-place1) dp-string))))))


;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              
;;;; File            : dismal.el
;;;; Authors         : David Fox, fox@cs.nyu.edu 
;;;;                   and Frank E. Ritter, ritter@cs.cmu.edu
;;;; Created On      : 31 Oct 1991.
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Sat Sep 25 16:02:30 1993
;;;; Update Count    : 835
;;;; 
;;;; PURPOSE
;;;;     DISMAL - Dis Mode Ain't Lotus.
;;;; 	Spreadsheet program for gnu-emacs.
;;;;
;;;; TABLE OF CONTENTS
;;;;	i.	Disclaimer
;;;;	ii.	Overview of how dismal-mode works
;;;;	iii.	What you must do to start up
;;;;	iv.	HISTORY
;;;; 	v.	Global user visible variables
;;;; 	vi.	Requires and loads and autoloads
;;;;	vii.	Former ritter-math
;;;; 	viii.	System constants
;;;; 	ix.	Mandatory variables - must be set in/by the control file
;;;;	x.	Internal variables
;;;;	xi.	Preliminary macro(s)
;;;;	xii.	Cell access and setting functions and defsubsts
;;;;	xiii.	Cell formatting
;;;;	xiv.	Known bugs
;;;;
;;;; 	I.	dismal-mode and startup code
;;;; 	II.	Other mode helpers & macros
;;;;	IIa.	Ruler code
;;;;	IIb.	Stuff taken from float.el
;;;;	III.	Set up the keymaps
;;;;	IV.	Dismal versions of commands
;;;; 	V.	dismal-mark
;;;; 	VI.	Range and range-buffer functions
;;;;	VIIa.	Date functions: variables and inits
;;;;	VIIb.	Erik's insert-date-string & insert-time-string
;;;; 	VIII.	Changed movement functions
;;;;	IX.	Cell editing
;;;;	X.	Cell re-evaluation
;;;;	XI.	Cell evaluation
;;;;	XIIa.	Insertion - of rows and columns.
;;;;	XIIb.	Deletion - of rows, columns & ranges
;;;;	XIIc.	Insertion and Deletion - Cell reference updating
;;;;	XIII.	Cell dependencies
;;;;	XIVa.	File I/O - Reading and writing
;;;;            Including compression code
;;;;    XIVb.	File I/O - Translation functions between Excel and Forms
;;;;	XIVc.	File I/O - Report functions
;;;;	XIVd.	File I/O - Dumping tabbed regions
;;;;	XIVe.	File I/O - Working with gnuplot
;;;;	XV.	Redrawing the screen
;;;;	XVI.	Cell formatting
;;;;	XVII.	Cell expression conversions
;;;;	XVIII.	Column formating commands
;;;;	XIXa.	Utility functions - Date functions
;;;;	XIXb.	Utility functions - List functions
;;;;	XIXc.	Utility functions - Math functions
;;;;	XIXd.	Utility functions - Misc
;;;;		(Debugging functions)
;;;;	XX.	Testing functions
;;;;	N.	Final code
;;;;	N+1.	History 
;;;;
;;;; Copyright 1993, David Fox & Frank Ritter.
;;;; Bug testing (incidental) and some fixes by bob@gnu.ai.mit.edu 
;;;; and altmann@cs.cmu.edu.  David Lamkins (DBLamkins@aol.com): Speed 
;;;; up file format and modifications for use with Marc Parmet's 
;;;; Macintosh port. Some date improvements by amy@vts.ed.ray.com.
;;;; Some speed ups and clarity added by Mikio Nakajima 
;;;; (minakaji@osaka.email.ne.jp)
;;;;
;;;; 
;;;; Formated in a modified Milnes style, based on
;;;; Oman & Cook, Typographic style is more than cosmetic, CACM, 33, 506-520. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fixed other utils 15k float, 18k popper
;;;  63 k 19-Dec-91 -DF
;;; 104 k 11-Jan-92 -FER
;;; 135 k 25-Jan-92 -FER
;;; 147 k 20-Feb-92 -FER  (+ 127 8 7 5)
;;; 178 k 14-Mar-92 -FER => bytecomp 116k (137K with lots of macros)
;;; 192 k 3-Apr-92 -FER
;;; 195 k 8-Apr-92 -FER V.62
;;; 242 k 18-Jul-92 -FER v.81 (+ 197 7 1 11 2  5 10 9)
;;; 247 k 26-Aug-92 -FER v.82 (+ 202 7 1 11 2  5 10 9)
;;; 435 k 6-6-94 - FER v.0.94 


;;;
;;;	i.	Disclaimer
;;;

;;; This file works with GNU-Emacs.  GNU Emacs is distributed in the hope
;;; that it will be useful, but WITHOUT ANY WARRANTY.  No author or
;;; distributor accepts responsibility to anyone for the consequences of
;;; using it or for whether it serves any particular purpose or works at
;;; all, unless he says so in writing.  Refer to the GNU Emacs General
;;; Public License for full details.
;;; 
;;; Everyone is granted permission to copy, modify and redistribute GNU Emacs, 
;;; and this file and its associated files
;;; but only under the conditions described in the GNU Emacs General Public
;;; License.  A copy of this license is supposed to have been given to you
;;; along with GNU Emacs so you can know your rights and responsibilities.  If
;;; you don't have this copy, write to the Free Software Foundation, Inc., 675
;;; Mass Ave, Cambridge, MA 02139, USA.


;;;
;;;	ii.	Overview of how dismal-mode works
;;;

;;; Date: Thu, 19 Dec 91 17:58:44 -0500
;;; From: David Fox <fox@GRAPHICS.CS.NYU.EDU>
;;; To: Frank_Ritter@SHAMO.SOAR.CS.CMU.EDU
;;; Subject: dismal results
;;; 
;;; Dismal is something I've worked on for quite a while on and off.
;;; I haven't been working on it lately, I've been hoping someone
;;; would take over for me.  I'll give you a call, or you can call me:
;;; 
;;; Office: 212-998-3389
;;; Home, weekdays: 212-874-7382
;;; Home, weekends: 908-273-3667
;;; 
;;; Frank Ritter ++ 44 (602) 436 265  (h)  ++ 44 (602) 515 292 (w)
;;; now at the U. of Nottingham, England
;;; Ritter doubled it in size and scope, and is now hoping for someone like 
;;; Fox was hoping.

;; INSTRUCTIONS FOR PRELIMINARY VERSION:  Commands are similar to sc,
;; "=" to enter a cell value, etc.   Numbers can be entered like numbers.
;; Use dis-find-file to create or retrieve a spreadsheet.
;; more help is available from the menu (C-c C-m), from mode
;; help (C-h m), and from the manual that comes with dismal.
;; dismal.info, available on the menu under doc.

;; Discussion:  The spreadsheet is stored in the buffer local variable
;; dismal-matrix.  This is a two dimensional array where each element
;; contains a five-tuple:
;;
;;   exp - the expression whose value is to be displayed in the cell
;;   val - the most recent result of evaluating the expression
;;   dep - a list of the addresses of the cells that use this cell's value
;;   mrk - a field used by some of the algorithms for temporary marks
;;   fmt - a function that takes the value and returns a formatted string
;;
;; The expression is a s-expression that, when eval-ed, returns the
;; current value of the expression.  The other cells of the spreadsheet
;; can be referred to in this expression using the four cell reference
;; functions:  dismal-r-c-, dismal-rfc-, dismal-r-cf, dismal-rfcf.
;; These functions are produced when the user inputs cell references
;; of the form A1, A$1, A1$, and A$1$ respectively, where the meaning
;; is that the reference is "fixed" in the dimension the dollar sign
;; follows.  Thus if you insert a new row zero, the reference A1 will
;; become A2, but A1$ remains A1$.  The four functions take row and
;; column as arguments.
;;
;; When the value field is non-nil, it is used rather than eval-ing
;; the expression.  When a cell's expression changes a function is
;; called that recursively sets the value field of all its dependents
;; to nil.  Note that that value field need have no particular type,
;; as long as the format function can convert it to a string.
;;
;; The format function takes four arguments and returns a string:
;;      (format value width decimal extra)
;; Value is the value to be formatted.  Width is the total length
;; of the returned string.  Decimal is the number of characters to
;; follow the decimal point, if this is meaningful.  Finally, extra
;; is the total width of the empty cells to this cell's right.  When
;; a left justified string is formatted, it is allowed to overlap
;; any empty cells to its right.
;; Tiny fonts let you open a window on a big display as large as 220 col
;; by 100 rows.  This is hard to read though.


;;;
;;;	iii.	What you must do to start up
;;;
;;; Load the dismal-mode-defaults.el file in your .emacs or by hand.
;;;

;; Some notes on bytecompiling
;;   [.elC is with Zawinski bytecompiler]
;; 3:07 to enter 31x1071 dismal spreadsheet with .el
;; 3:00 to enter 31x1071 dismal spreadsheet with .elc
;; 3:00 to enter 31x1071 dismal spreadsheet with .elC
;; 3:00 to enter 31x1071 dismal spreadsheet with .elC w/ some inline
;; 2:33 to enter 31x1071 dismal spreadsheet with .elC w/ lots inline
;; 2:21 to enter 31x1071 dismal spreadsheet with .elC w/ lots^2 inline
;; 1:54 to enter 31x1071 dismal spreadsheet with .elC w/ lots^3 inline
;; 2:54 to enter 31x1071 dismal spreadsheet " " " w/ integer numbers


;;;
;;;	iv.	HISTORY
;;;
;;; Deprecated, see end of file.


;;;
;;; 	v.	Global user visible variables
;;;

;; 30-Aug-95 -FER This causes lots of errors.  Make the default be nil now.
;;(defvar dis-use-popper nil
;;  "*Use the popper package for temporary results.")

(defvar dis-show-selected-ranges t
  "*Show the user the selected range when cutting or erasing.")

(defvar dis-recursion-limit 9
  "*Maximum depth allowed when evaluating (perhaps) circular dependencies.")

(defvar dis-iteration-limit 3 ;; 9 might be good in a released version
  "*Maximum number of iterations of update cycles.")

(defvar dis-inhibit-startup-message nil
  "*Print out helpful messages after starting up.")

(defvar dis-load-hook nil
  "*Hook variable run after dismal-mode is loaded.")

(defvar dis-mode-hooks nil
  "*Hook functions to be run upon entering dismal-mode.")

(defvar dis-query-on-entry-p nil
  "*Ask for confirmation each time dismal-mode is called.  Normally
unimportant for normal users, who should have this set to nil.  In released
versions this should be set to nil.")

(defvar dis-default-column-width 10
  "*Default width for columns.")
(make-variable-buffer-local 'dis-default-column-width)

(defvar dis-default-column-alignment 'default
 "*Default way to align a cell.")
(make-variable-buffer-local 'dis-default-column-alignment)

(defvar dis-default-column-decimal 2
 "*Default number of decimal digits for a cell.")
(make-variable-buffer-local 'dis-default-column-decimal)

(defvar dis-page-length 64
  "*Anticipated page length for printing.")
(make-variable-buffer-local 'dis-page-length)

(defvar dis-field-sep "\t"
  "*Default field separator character when reading in with other system
dump files (default TAB).")


;;;
;;; dis-dump-range and TeX dumping Variables
;;; (For writing out dismal files for interchange.)

(defvar dis-dump-end-row-marker "\n"
  "*Text inserted by dis-dump-range at the end of each row")
(defvar dis-dump-start-row-marker ""
  "*Text inserted by dis-dump-range at the beginning of each row")

(defvar dis-dump-between-col-marker "\t"
  "*Text inserted by dis-dump-range betweeen columns")

(defvar  dis-dump-tex-end-row-marker "\\\\ \n"
  "*Text inserted by dis-tex-dump-range at the end of each row")

(defvar  dis-dump-tex-between-col-marker " & "
  "*Text inserted by dis-tex-dump-range betweeen columns")

(defvar dis-show-update nil
  "*If t, then display the values being updated when a variable changes.")

(defvar dis-auto-save-interval 1000
  "*Number of dismal movements between auto-saves.
Zero means disable autosaving.")
;; counts number of visit-cells

(defvar dis-ruler-row -2
  "*The row to use to make a ruler on the top.")
(make-variable-buffer-local 'dis-ruler-row)

(defvar dis-show-ruler t
  "*If t (the default) show the ruler at the top.")
(make-variable-buffer-local 'dis-show-ruler)

(defvar dis-auto-update t
  "*If t (the default) automaticaly call update after a cell changes.
Setting this to nill can save significant amounts of time on large sheets.")
(make-variable-buffer-local 'dis-auto-update)

;; We use enscript cause it works well here at CMU.   You will have to use
;; a local command probably, although enscript appears to standard unix...
;; -r is rotate                ;; -L is page length in lines
;; -G is gaudy                 ;; -c truncates long lines
;; -f is font                  ;; -pfile will print to a file
;; later (earlier?) versions won't take -c

;; Common to a user, not a buffer
(defvar dis-raw-print-command
        "enscript -r -G -fCourier7 -L%d "
  "*Format statement to make the local print command.  
Must take an argument of max display-width, or dis-print-command must
be set by hand.")

(defvar dis-print-command (format dis-raw-print-command
				  (+ 2 dis-page-length))
  "Command to print a file on locally.  Created from dis-raw-print-command.")

(defvar dis-middle-col nil
  "*The last col (coming from the left) that is grouped with the left hand side
columns when alighning.")
(make-variable-buffer-local 'dis-middle-col)

(defvar dismal-normal-max-column-width 20
 "*The normal maximum column width.  Widths larger than this must be 
confirmed on entering.")

(defvar dismal-copy-to-dismal-binding "\C-c\M-c"
  "*Key to globally bind to copy-to-dismal.")

(defvar dis-codes-file (concat dismal-directory "/example-codes.txt")
  "*Default file to get codes from.")


;;; these variables increase the size of a file by about, say, 50% before
;;; compression, and increasing loading speed by a factor of, say, 80.
;;; Overall, a pretty good speed/space tradeoff. 8-17-94 - FER

;; 2. Added an option to save and reload the display image.  This saves
;;    the contents of the displayed buffer in a backward-compatible
;;    format.  Upon reloading, the presence of the saved image
;;    eliminates the need to redraw the spreadsheet from saved variables.
;;    The save action is controlled by the dismal-save-image option,
;;    and the restore action is controlled by dismal-load-image.
;;    Functions dismal-write-buffer and dismal-mode were altered.
;;    
;;    The speedup from this change is dramatic.  On my SE/30 running
;;    Marc's 1.14b1 release, reading a spreadsheet of 150 rows by 33
;;    columns took 40 minutes using the unmodified Dismal 0.92.
;;    Saving this spreadsheet with the display image increased its
;;    file size from 89K to 150K, but reduced the load time to 28
;;    seconds, an improvement of 39.5 minutes, or a speedup of 85
;;    times.  This factor will probably depend upon the Emacs
;;    implementation and the size and contents of the .dis file, but I
;;    think it should be a big win for anyone using Dismal.

;; with compression, it will take a while longer to load, but not a
;; lot, and space now goes down on my (FER) test image to 40% of the
;; original total when the image is included.


;DBL
(defvar dismal-save-image t
  "*Display image is saved with file if non-NIL.  This speeds reloading.")

;DBL
(defvar dismal-load-image t
  "*When non-NIL, load saved display image if available.")

(defvar dismal-save-compression nil
  "*Saved files are compressed upon save.")
(make-variable-buffer-local 'dismal-save-compression)

;; When you write a new function for the user, put it on here.
(defvar dis-user-cell-functions
  '(dis-count dis-count-words-in-range
    dis-count-regexp-in-range
    dis-count-if-regexp-match
    dis-match-list
    dis-sum dis-mean dis-product dis-div dis-plus
    dis-current-date dis-date-to-days
    dis-copy-to-dismal dis-grader dis-ungrader)
  "Functions the user can put in a cell.")

;; not really user settable, but needed by loads up here, 19-Jun-96 -FER
(defvar dismal-map nil "")


;;;
;;; 	vi.	Requires and loads and autoloads
;;;

;; The Emacs Common-lisp look-alike package
;; Every site should have this.  Email us if you don't.
;; Put here because it doesn't fit in the make otherwise.
(require 'cl)

;; set up the logger

(setq *log-timer-program*
      (concat dismal-directory "/timer.bin"))

(setq log-compress! nil)
(load "log.el")

;; so popper won't clobber
;;(if (or (not (boundp 'dis-use-popper)) dis-use-popper)
;;    (setq popper-load-hook 
;;         (function (lambda ()
;;           ;; Define key bindings
;;           (define-key global-map "\C-c1" 'popper-bury-output)
;;           (define-key global-map "\C-cv" 'popper-scroll-output)
;;           (define-key global-map "\C-cg" 'popper-grow-output)
;;           (define-key global-map "\C-cb" 'popper-switch)))))
;;           ;; Make *Manual windows default to 10 lines

;; (if (or (not (boundp 'dis-use-popper)) dis-use-popper)
;;    (require 'popper))

;; going with 19 rationals on 28/9/94
;; leave in for one release at least
;; taken out, 2-Jan-97 -FER
;; (if (not (boundp 'exp-base))
;;    (load "float"))             ; float should (provide 'float)!

;; these are now changes/additions to the native floats
(require 'float-changes)

(require 'vectors)
(require 'heaps)
;; (require 'matrix)  ;; a column based matrix
(require 'rmatrix)
(require 'dismal-data-structures)
(require 'dismal-simple-menus)
(require 'soar-misc)

;; (require 'dismal-metacolumn)
(autoload 'dis-set-metacolumn
  "dismal-metacolumn"
  "Set the middle-column, which is used to create two meta-columns in 
the spreadsheet.")

(autoload 'dis-insert-metacolumn-cells 
  "dismal-metacolumn"
  "Insert ARG cells in the metacolumn that COL (default, current-col) is in,
at ROW (default, current-row).")

(autoload 'dis-insert-z-box 
  "dismal-metacolumn"
  "Insert ARG rows of cells on each side of dis-middle-col,
starting at the rows of point and mark, which must be on opposite 
sides of the middle-col.")

(autoload 'dis-align-metacolumns
  "dismal-metacolumn"
  "Align the metacolumns so that point and mark are on the same line,
keeping other parts of the columns still aligned.")


;; now assume that we get our directory in load path
(global-set-key "\C-cd" 'dis-insert-date-string)


;; can't require this (it has no provide), but we'll try
(autoload 'delete-extract-rectangle "rect")

(autoload 'dis-model-match "dismal-extensions"
  "Given a cell RANGE computes the percentage of colA matched
with something in colA-1.  Only counts stuff that is in order." t)

(autoload 'dis-model-match-op "dismal-extensions"
  "Given a cell RANGE computes the percentage of colA matched
with something in colA-2, and col A is an operator.  Only counts stuff
that is in order." t)

(autoload 'dis-auto-align-model  "auto-aligner"
  "Automatically align the two metacolumns in
the spreadsheet." t)

(autoload 'dis-model-match-op    "dismal-model-extensions"
   "Given a cell RANGE computes the percentage of colA matched
with something in colA-2, and col A is an operator.  Only counts stuff
that is in order." t)

(autoload 'dis-model-match "dismal-model-extensions"
  "Given a cell RANGE computes the percentage of colA matched
with something in colA-1.  Only counts stuff that is in order." t)

(autoload 'dis-initialize-operator-codes "semi-coder"
  "Initialize the dismal operator codes." t)

(autoload 'dis-load-op-codes "semi-coder"
 "Load operator codes into dismal.  UNION-OR-REPLACE can be either." t)

(autoload 'dis-op-code-segment "semi-coder"
  "Code a segment with an operator name." t)

;; cheap enough, so always include
(load "keystroke")
(load "make-km-aliases")

;; this here could be modified to use 19's modified zawinski compiler
;;  (if (fboundp 'proclaim-inline)
;;    (proclaim-inline
;;      ;; dismal-address-compare  ;; makes me nervous
;;      dismal-cellp
;;      dismal-adjust-range
;;      dismal-cell-name
;;      ;; dismal-end-of-col-non-interactive ;interactive
;;      dismal-char-col-to-dismal-col
;;      dismal-cleanup-long-string
;;      dismal-column-alignment
;;      dismal-column-decimal
;;      dismal-column-width
;;      dismal-convert-number-to-colname
;;      dismal-del
;;      dismal-draw-column-label
;;      dismal-draw-row-label
;;      dismal-evaluate-cell
;;      dismal-evaluate-cellref
;;      dismal-execute-delayed-commands
;;      dismal-flat-format
;;      dismal-get-cell
;;      dismal-get-cell-alignment
;;      dismal-get-cell-exp 
;;      dismal-get-cell-val 
;;      dismal-get-cell-dep 
;;      dismal-get-cell-mrk 
;;      dismal-get-cell-fmt
;;      dismal-get-or-make-cell
;;      dismal-get-create-column-format
;;      dismal-get-column-format
;;      dismal-get-deps
;;      dismal-get-exp
;;      dismal-get-fmt
;;      dismal-get-mrk
;;      dismal-get-val
;;      dismal-goto-cell
;;      dismal-insert-blank-box
;;      dismal-insert-blank-col
;;      dismal-insert-blank-range
;;      dismal-insert-report-rulers
;;      dismal-invalidate-cell
;;      dismal-isearch-guts
;;      dismal-isearch-queryer
;;      dismal-jump-to-cell
;;      dismal-jump-to-cell-quietly
;;      dismal-make-print-file-name
;;      dismal-note-selected-range
;;      dismal-edit-cell
;;      dismal-read-row
;;      dismal-redraw-cell
;;      dismal-redraw-row
;;      dismal-remove-row-label
;;      dismal-report-header
;;      dismal-set-cell
;;      dismal-set-cell-exp 
;;      dismal-set-cell-val 
;;      dismal-set-cell-dep 
;;      dismal-set-cell-mrk 
;;      dismal-set-cell-fmt 
;;      dismal-set-deps
;;      dismal-set-exp
;;      dismal-set-fmt
;;      dismal-set-mark
;;      dismal-set-mrk
;;      dismal-set-val
;;      ; dismal-show-selected-range ; basically interactive
;;      dismal-string-to-range
;;      dismal-sum-column-widths
;;      dismal-visit-cell
;;    ))

;; done with defsubst (so far):
;;    rangep (in dismal-data-structures.el)
;;    dismal-query-replace-guts
;;    dismal-addressp  (in dismal-data-structures.el)

;;;
;;;	vii.	Former ritter-math
;;;

;;;  Some simple math extensions to elisp.
;;; Formerly a separate file called ritter-math.
;;;

;; log10 is now in emacs 19!  19-Oct-97-FER
;; but it now returns a real, not an integer...
;;
;;   (defun log10plus (x)
;;     (let ((dividend (/ x 10)))
;;     (if (not (= 0 dividend))
;;         (+ 1 (log10plus dividend))
;;        0)))
;;   
;;   (defsubst log10minus (x)
;;     (error "log10minus in dismal not defined."))
;;   
;;   (defsubst log10 (x)
;;     (if (< x 0) (error "log10 error."))
;;     (if (> x 1)
;;         (log10plus x)
;;       (if (= x 1)
;;           0
;;       (log10minus x))))

(defmacro signp (arg)
  (list 'if (list '> arg 0) 1 -1))


;;;
;;; 	viii.	System Constants
;;;

(defconst dismal-version "1.4"
  "Version of dismal-mode implementation.")

(defun dismal-version (&optional here)
  "Show the version of dismal.el in the minibuffer.
If optional argument HERE is non-nil, insert info at point."
  (interactive "P")
  (let ((version-string 
         (format "Version of \`dismal.el\': %s" dismal-version)))
    (if here 
        (insert version-string)
      (if (interactive-p)
          (message "%s" version-string)
        version-string))))

;; (string-match dismal-cell-name-regexp " aa23")

;; These only match up to column ZZ because of problems having symbols like
;; diag16 matched (ie, columns over 70,000)

(defconst dismal-cell-name-regexp "^\
\\([A-Z][A-Z]?\\|[a-z][a-z]?\\)\
\\(\\$\\|\\)\
\\([0-9][0-9]*\\)\
\\(\\$\\|\\)\
$"
  "Matches cell names, such as 'A1', 'b$2', 'C$3$', etc.")

(defconst dismal-cell-range-regexp "^\
\\([A-Z][A-Z]?\\|[a-z][a-z]?\\)\
\\(\\$\\|\\)\
\\([0-9][0-9]*\\)\
\\(\\$\\|\\)\
:\
\\([A-Z][A-Z]?\\|[a-z][a-z]?\\)\
\\(\\$\\|\\)\
\\([0-9][0-9]*\\)\
\\(\\$\\|\\)\
$"
  "Matches two cell names separated by a colon.")

          
(defconst dismal-startup-message-lines
  (list 
   "Type C-c C-m for the dismal command menu."
   "dismal has a mailing list as dismal-users@psychology.nottingham.ac.uk"
   "To start logging, use the dismal menu: Command: 1log"
   "To stop logging, use the dismal menu: Command: 0log"
   (concat "Type \\[describe-mode] for help on dismal version "
	   dismal-version)
   "You may redistribute dismal.  Type `\\[describe-copying:]' to see conditions."
   "Dismal comes with NO WARRANTY; type `\\[show-no-warranty]' for details."
   "In version 1.1, all float functions are now native, use + instead of f+"
   "Happy hacking!"))


;; Xemacs I think is a bad thing, but it's close to GNU Emacs, and we 
;; should help its users as well, and dismal won't need many changes to 
;; work there. 
;; Marko Schuetz <marko@hisplace.rhein-main.de> has found the bugs so 
;; far in it (and someone else maybe in the past) that this variable 
;; protects from.

(defconst dismal-xemacs-p (string-match "XEmacs" (emacs-version))
  "T is XEmacs is loaded.")

(if dismal-xemacs-p
  (progn 
    (message "Dismal is not designed/tested w/ Xemacs (use GNU you!), but it might work.")
    (sit-for 1)))


;;;
;;; 	ix.	Mandatory variables - must be set in/by the control file
;;;

;; Variables that will be written out on save.
(defvar dismal-saved-variables
      '(dis-auto-update
        dismal-default-column-format
        dismal-column-formats
        dismal-formula-cells
        dismal-max-row
        dismal-max-col
        dis-middle-col
        dis-page-length
        dis-ruler-row
        dis-show-ruler
        dismal-save-compression
        dismal-compress-command
        dismal-write-file-version
        dismal-uncompress-command
        dismal-matrix))

;;
;; BUFFER-LOCAL VARIABLES: Variables saved in spreadsheet file.
;;

(defvar dismal-matrix nil
  "The elements of this matrix represents cells.  Each element is
a list with up to four elements:
        1. The expression (exp) associated with each cell.
        2. The result of the most recent evaluation (val) of this cell.
        3. A list of addresses (row-column pairs) of cells that depend
           on the value of this cell (dep).
        4. The current depth of recursive evaluations of this cell.
           This is saved in the data file for simplicity even though
           it is temporary data (mrk?)
        5. Format, specific format information pertaining to that cell only.")
(make-variable-buffer-local 'dismal-matrix)

(defvar dismal-default-column-format
  [dis-default-column-width 2 default]
  "Columns corresponding to nil elements or elements beyond the end of
dismal-column-formats are considered to have this format.  An array that is
width, decimals shown, and justification (default, left, right, center).")

(aset dismal-default-column-format 0 dis-default-column-width)
(aset dismal-default-column-format 1 dis-default-column-decimal)
(aset dismal-default-column-format 2 dis-default-column-alignment)
(make-variable-buffer-local 'dismal-default-column-format)

(defvar dismal-column-formats (vector-create nil)
  "A vector of the formats of the columns.  Each element is a list of
two numbers, the column width and the number of characters after the
decimal point.")
(make-variable-buffer-local 'dismal-column-formats)

(defvar dismal-formula-cells (vector-create nil)
  "A vector of the cells with real expressions in them.  Each element is a 
cons of the row and column numbers of the cell with a formula.")
(make-variable-buffer-local 'dismal-formula-cells)

(defvar dismal-max-row 0
  "Number of the bottom-most row that contains a value.")
;; number of rows is 1+ this number
(make-variable-buffer-local 'dismal-max-row)

(defvar dismal-max-col 0
  "Equal to (1- (length dismal-expressions)), but used to help draw new 
column labels.")
;; number of rows is 1+ this number
(make-variable-buffer-local 'dismal-max-col)

(defvar dismal-middle-col-name nil
  "The name of the middle column, such as 'A' or 'L'.")
(make-variable-buffer-local 'dismal-middle-col-name)

(defvar dismal-buffer-auto-save-file-name nil)
(make-variable-buffer-local 'dismal-buffer-auto-save-file-name)

(defvar dismal-auto-save-counter 0)
(make-variable-buffer-local 'dismal-auto-save-counter)

(defvar dismal-delayed-commands nil)
(make-variable-buffer-local 'dismal-delayed-commands)

(defvar dismal-write-file-version dismal-version
  "Version of dismal that wrote the file.")
(make-variable-buffer-local 'dismal-write-file-version)

(defvar dismal-buffer-read-only nil
  "Is the original dismal buffer a read only buffer?")
(make-variable-buffer-local 'dismal-buffer-read-only)

(defvar dismal-interactive-p t)
;; Set to nil when doing secret commands, that we don't want side effects from.
;; Not buffer local, if doing a command, must be accessable in all buffers.

(defvar dismal-mark [nil nil])
(make-variable-buffer-local 'dismal-mark)


;;;
;;; 	x.	Internal variables
;;;

;; Make the lisp function stack larger.
(setq max-lisp-eval-depth (max max-lisp-eval-depth 500))

(defvar dismal-ctl-x-map nil)
(defvar dismal-ctl-c-map nil)
(defvar dismal-delete-prefix-map nil)
(defvar dismal-insert-prefix-map nil)

(defvar dismal-mode-line-format
 '(""  
   ;; taken from: mode-line-modified  "--%1*%1+-"
   "--" (dismal-buffer-read-only "%%%%" "--") "%1+-" 
   mode-line-buffer-identification "   " 
   global-mode-string
   " " dismal-current-cell " "
   (dis-auto-update "AutoUp" "ManUp ")
   ;; doing this right might be hard, so leave it alone.
   " <"
   (dis-middle-col dismal-middle-col-name)
   "]"
   "  %[(" mode-name minor-mode-alist "%n"
   mode-line-process ")%]----" (-3 . "%p") "-%-"))

(defvar dismal-row-label-lined nil
  "If t (default nil), put a | up at the end of row number.")
;; not kept around separately in all buffers
;; See Tufte, and Short papers @ chi '92 for why this is ok.

(defvar dismal-row-label-format
   (concat "%6d "
           (if dismal-row-label-lined "|" "")))
(make-variable-buffer-local 'dismal-row-label-format)

(defvar dismal-current-first-ruler-row nil
  "Where the ruler is currently displayed.")
(make-variable-buffer-local 'dis-ruler-row)

(defvar dismal-ruler "") ; the actual string put up
(make-variable-buffer-local 'dismal-ruler)

(defvar dismal-buffer-using-minibuffer nil
  "Set when a buffer is doing a read.")
;; don't make local, so that minibuffer and current buffer can get back to it.
;; only bound with a let anyhow, so can't be clobbered...

(defconst dismal-blank-bag '(?\ ?\t))  ;took out ?:

(defvar dismal-first-data-line 3
  "Argument to goto-line that takes you to row 0.")
(make-variable-buffer-local 'dis-default-column-width)

(defvar dismal-first-printed-column 8
  "Argument to move-to-column that takes you to the beginning of column A.")
;; Is typically 2 for less than 10 rows
(make-variable-buffer-local 'dismal-first-printed-column)

(defvar dismal-number-p 'floatp)
(defvar dismal-number-to-string 'prin1)

(defvar dismal-invalid-heap nil
  "Heap to hold addresses of cells that need re-evaluation.")
(make-variable-buffer-local 'dismal-invalid-heap)

(defvar dismal-invalid-heap-not nil
  "Backup heap to hold addresses of cells that need re-evaluation.")
(make-variable-buffer-local 'dismal-invalid-heap-not)

;; These two are used to let you note changes in a cycle.
;; dismal-invalid-heap is set to one of them
(defvar dismal-invalid-heapA nil
  "Heap to hold addresses of cells that need re-evaluation.")
(make-variable-buffer-local 'dismal-invalid-heapB)

(defvar dismal-invalid-heapB nil
  "Heap to hold addresses of cells that need re-evaluation.")
(make-variable-buffer-local 'dismal-invalid-heapA)

;;
;; BUFFER-LOCAL VARIABLES: Transient.
;;

(defvar dismal-setup nil
  "T if dismal-mode has been called on the file/buffer.") 
(make-variable-buffer-local 'dismal-setup)

(defvar dismal-current-col 0
  "Current column of point, where first col (labelled `A') is number 0.")
(make-variable-buffer-local 'dismal-current-col)

(defvar dismal-current-row 0
  "Current row number of point.")
(make-variable-buffer-local 'dismal-current-row)

(defvar dismal-cell-buffer nil
  "The range expression copied by range selection commands is saved here,
including its address of origin.")
;; not local so it can be shared
;(make-variable-buffer-local 'dismal-cell-buffer)

(defvar dismal-range-buffer nil
  "The cells copied by range command for range pasting are saved here, 
along with its size.  Format: [rows-used cols-used matrix].")
;; By not making this local, we can cut and paste between sheets...
;;(make-variable-buffer-local 'dismal-range-buffer)

;; There must be a faster, cheaper way to do this, but I don't see it 
;; tonight: this sucker will get reset repeatedly.
(defvar dismal-current-cell "A0"
  "String indicating current cell.")
(make-variable-buffer-local 'dismal-current-cell)


;;;
;;;	xii.	Cell access and setting functions and defsubsts
;;;

(defsubst dismal-convert-number-to-colname (column)
  ;; Convert a number to a column name string.  Maximum column is 26^2-1.
  ;; 0 -> `A', 25 -> `Z', 26 -> `AA', 51 -> `AZ', 52 -> `BA' ...
  (if column
     (concat (if (> column 25)
                 (char-to-string (1- (+ ?A (% (/ column 26) 26))))
                "")
             (char-to-string (+ ?A (% column 26))))
     "nil"))

(defsubst dismal-cell-name (row column)
  (concat (dismal-convert-number-to-colname column) (int-to-string row)))

(defsubst dismal-get-create-column-format (colnum)
  (or (vector-ref dismal-column-formats dismal-current-col)
      (vector-set dismal-column-formats dismal-current-col
                  (vec-copy-sequence-r dismal-default-column-format))))

(defsubst dismal-set-column-alignment (colnum style)
  (aset (dismal-get-create-column-format colnum)
        2 style))

;; Get the value of a particular field in a cell


;; heavyweight, make cell exist
(defsubst dismal-get-or-make-cell (r c)
  (let ((cell (matrix-ref dismal-matrix r c)))
    (or cell
       (matrix-set dismal-matrix r c (setq cell (make-vector 5 nil))))))

;; lightweight, does not make cell exist
(defsubst dismal-get-cell (r c)
  (matrix-ref dismal-matrix r c))

(defsubst dismal-get-cell-exp (cell) (if (null cell) nil (aref cell 0)))
(defsubst dismal-get-cell-val (cell) (if (null cell) nil (aref cell 1)))
(defsubst dismal-get-cell-dep (cell) (if (null cell) nil (aref cell 2)))
(defsubst dismal-get-cell-mrk (cell) (if (null cell) nil (aref cell 3)))
(defsubst dismal-get-cell-fmt (cell) (if (null cell) nil (aref cell 4)))

;; Set the value of a particular field in cell
;; may be dangerous

(defun dismal-set-cell-exp (cell x) (if (or cell x) (aset cell 0 x)))
(defun dismal-set-cell-val (cell x) (if (or cell x) (aset cell 1 x)))
(defun dismal-set-cell-dep (cell x) (if (or cell x) (aset cell 2 x)))
(defun dismal-set-cell-mrk (cell x) (if (or cell x) (aset cell 3 x)))
(defun dismal-set-cell-fmt (cell x) (if (or cell x) (aset cell 4 x)))

;; Get the value of a field of the cell at r, c

(defun dismal-get-exp (r c)
  (dismal-get-cell-exp (matrix-ref dismal-matrix r c)))
(defun dismal-get-val (r c)
  (dismal-get-cell-val (matrix-ref dismal-matrix r c)))
(defun dismal-get-deps (r c)
  (dismal-get-cell-dep (matrix-ref dismal-matrix r c)))

;; 2-Mar-92 -FER old address (row col) based way
; (defun dismal-get-deps (a)              ; A is a row-col pair.
;  (dismal-get-cell-dep (matrix-ref dismal-matrix (nth 0 a) (nth 1 a))))

(defsubst dismal-get-mrk (r c)
  (dismal-get-cell-mrk (matrix-ref dismal-matrix r c)))
(defsubst dismal-get-fmt (r c)
  (dismal-get-cell-fmt (matrix-ref dismal-matrix r c)))

(defun dismal-set-exp (r c x)
  ;; Set the value of a field of the cell at r, c
  (dismal-set-cell-exp (dismal-get-or-make-cell r c) x))

(defun dismal-set-val (r c x)
  (dismal-set-cell-val (dismal-get-or-make-cell r c) x))

(defun dismal-set-deps (r c x)
  (dismal-set-cell-dep (dismal-get-or-make-cell r c) x))

;; old way, with address as a list
;(defun dismal-set-deps (a x)    ; A is a row-col pair
;  (let ((cell (matrix-ref dismal-matrix (nth 0 a) (nth 1 a))))
;    (if (null cell)
;        (matrix-set dismal-matrix (nth 0 a) (nth 1 a)
;                    (setq cell (make-vector 5 nil))))
;    (dismal-set-cell-dep cell x)))

;; this should probably become a push-mark, but alas, no time/understanding...
(defsubst dismal-set-mark (row col)
  (aset dismal-mark 0 row)
  (aset dismal-mark 1 col))

(defun dismal-set-mrk (r c x)
  (dismal-set-cell-mrk (dismal-get-or-make-cell r c) x))

;; this really means alignment...16-Jan-92 -FER
(defun dismal-set-fmt (r c x)
  (dismal-set-cell-fmt (dismal-get-or-make-cell r c) x))

;; Jump to ROW, COLUMN and display the contents of the cell
;; in the status line.
;; hard to make a defsubst, used somewhere earlier than this file!
(defun dismal-jump-to-cell (r c)
  (dismal-visit-cell r c)
  (setq dismal-current-row r)
  (setq dismal-current-col c))

(defsubst dismal-jump-to-cell-quietly (r c)
  "Jump to ROW, COLUMN but don't display the contents of the cell
in the status line."
  (dismal-goto-cell r c t)
  (setq dismal-current-row r)
  (setq dismal-current-col c))

;; bummed by Mikio Nakajima <minakaji@osaka.email.ne.jp>, 3-Sep-97 -FER
(defsubst dismal-goto-row (row interactivep)
  ;; Move the cursor to the requested ROW.
  (let ((rows-missing (goto-line (+ row dismal-first-data-line))))
    (if (not (bolp)) (setq rows-missing (1+ rows-missing)))
    (open-line rows-missing)
    (forward-char rows-missing))    )


(defun dis-transpose-cells ()
  "Like ESC t but applies to adjacent horizontal cells.
Flips the current cell and the one to its left."
  ;; if on left edge, moves one cell right
  ;; this is stupid about updating forumla references, etc.
  (interactive)
  (dismal-save-excursion
  (if (= 0 dismal-current-col)
      (setq dismal-current-col (1+ dismal-current-col)))
  (let* ((cell1 (dismal-get-or-make-cell dismal-current-row
                                         dismal-current-col))
         (cell2 (dismal-get-or-make-cell dismal-current-row
                                        (1+ dismal-current-col)))
         (exp (dismal-get-cell-exp cell1))
         (val (dismal-get-cell-val cell1))
         (dep (dismal-get-cell-dep cell1))
         (mrk (dismal-get-cell-mrk cell1))
         (fmt (dismal-get-cell-fmt cell1))  )
    ;; swap A and B
    (dismal-set-cell-exp cell1 (dismal-get-cell-exp cell2))
    (dismal-set-cell-val cell1 (dismal-get-cell-val cell2))
    (dismal-set-cell-dep cell1 (dismal-get-cell-dep cell2))
    (dismal-set-cell-mrk cell1 (dismal-get-cell-mrk cell2))
    (dismal-set-cell-fmt cell1 (dismal-get-cell-fmt cell2))
    ;; swap temp and B
    (dismal-set-cell-exp cell2 exp)
    (dismal-set-cell-val cell2 val)
    (dismal-set-cell-dep cell2 dep)
    (dismal-set-cell-mrk cell2 mrk)
    (dismal-set-cell-fmt cell2 fmt)
    (dismal-redraw-row dismal-current-row t)))
  (dis-forward-column 1))

(defsubst dismal-create-matrix ()
  (let ((m (matrix-create)))
    (vector-insert m 0 1)))

;; (inspect (matrix-create)) 
;; should look like:  [1 0 [[1 0 [nil] nil]] [1 0 [nil] nil]]
;; (inspect (dismal-create-matrix))
;;  should look like:  [1 1 [ [1 0 [nil] nil]] [1 0 [nil] nil]]
;; bug looks like:     [1 1 [ [1 0 #1 nil] ]   [1 0 [[1 0 #2 nil]] nil]]


;; 
;; (dismal-possible-live-sexp '(+ (dismal-get-val dismal-current-row (-
;;  dismal-current-col 1)) (dismal-get-val dismal-current-row (-
;;  dismal-current-col 2))))
;; (dismal-possible-live-sexp 'asdf)
;; (dismal-possible-live-sexp '+)
;; (dismal-possible-live-sexp 12)
;; (symbolp 12)

(defsubst dismal-possible-live-sexp (sexp)
   (and sexp                ;; not nil
        (or (and (listp sexp)    ;; a list, not a number or string
                 (listp (cdr sexp)))  ; not a cons cell             
            )  
        ;; (not (floatp sexp)) ; not an old style float
   ))
;      (or (null sexp)         ;; up and out immediately if these types,
;          (floatp sexp)
;          (symbolp sexp)  ;; plain variables donot count, cant see changes
;          (not (listp sexp))) ;; b/c they have nothing to do

(defsubst dismal-file-header (mode-name-to-write)
  (insert ";; -*- Mode: " mode-name-to-write " -*-")
  (insert "\n;; This file was produced for user " (user-login-name)
          " by dismal-mode (Vers " dismal-version ")"
          "\n;; This file written ")
  (insert-current-time-string)
  (insert "\n;; dismal-mode Copyright 1992, Fox & Ritter."
          "\n;; No user serviceable parts, but it is your data.\n\n\n"))

(defsubst dismal-column-width (column)
  ;; Compute the width of the given COLUMN from dismal-column-formats.
  (aref (dismal-get-column-format column) 0))

(defsubst dismal-column-alignment (column)
  ;; may return nil
  (let ( (format (dismal-get-column-format dismal-current-col)) )
    (if format
        (aref format 2))))

(defsubst dismal-column-decimal (column)
  ;; Compute the decimal field width of the given COLUMN from 
  ;; dismal-column-formats.
  (aref (dismal-get-column-format column) 1))


;;;
;;;	xiii.	Cell formatting
;;;
;;; These functions can be replaced by the user to allow other sorts 
;;; of math packages.

;; (float-to-string _f1)
;; (dismal-flat-format (+ 2.3 3.4) 2)
;; (dismal-flat-format 0.0 2)
;; (dismal-flat-format _f1 2)
;; (dismal-flat-format -52.52 2)
;; (dismal-flat-format 0.00 2)
;; (floatp 0.0)
;; (dismal-flat-format-float 0.0 2)
;; (dismal-flat-format '(quote (4194304 . -21)) 8)
;; (setq value (car aa))

;; (dismal-float-expr-p '(4194304 . -21))

(defsubst dismal-float-expr-p (sexp)
  ;; Returns true if this is an expression of the form (quote float),
  ;; which is how numbers appear in expressions so they can be eval'ed.
  (and (listp sexp)
       (eq (car sexp) 'quote)
       (oldfloatp (nth 1 sexp))))


(defsubst dismal-flat-format (value decimal)
  ;; return a string in its full glory
  (cond ( (numberp value)
          (if (integerp value)
              (int-to-string value)
            (dismal-flat-format-float value decimal)))
        ( (dismal-float-expr-p value)
          (dismal-flat-format-float-string 
                    (old-float-to-string (eval value)) 8))
        ;; ( (floatp value) ;it's a special number
        ;;  (dismal-flat-format-float-string (float-to-string value) decimal))
        (t value)))


;;;
;;;	xiv.	Known bugs
;;;

;; Emacs bugs notes: 
;;    * (format "%e" fnum) not documented in C-h f format
;;    * defsubst on a recursive function is bad, and not warned.

;; CAUTION: I haven't made it safe from looping on circular definitions
;; yet.

;; For David:  
;; 17.  Implement evaluation loop counting [you should take a whack at this,
;;     it's outside my interests and talents (reletively)].
;;     [On DF's list]
;; * On updates, and adding, deleting rows/cols, we're are losing time
;;   big in dismal-erase-all-dependencies and dismal-record-all-dependencies.
;;   I've gotten the redraw routines partially beat, can you look at these?
;; Compatibility Notes: 
;; * column-format's are now arrays of 3, incompatible, but fixable in existing
;;   files by replacing "(" with "[", old files with (Num Num)'s in 
;;   dismal-column-formats should be replaced with [Num Num nil], and 
;;    within the dismal-matrix itself
;;    (query-replace "ff+" "dis-sum")
;;    (query-replace "ff*" "dis-product")
;;    (replace-string "dismal-format-string-left" "left")
;;    (replace-string "dismal-format-string-right" "right")
;;    (replace-string "dismal-format-number" "default")
;;    (replace-string "dismal-format-center" "center")
;;    (replace-string "dismal-current-column" "dismal-current-col")
;;    (replace-string "dismal-maximum-column" "dismal-max-col")
;;    (replace-string "dismal-maximum-row" "dismal-max-row")


;; Known bugs II
;; To Do:
;; ? continued popper problems and specific popper error:
;;   As of 19.14, `length' no longer works with compiled-function objects:
;;   2-Jan-97 -FER
;; 2 (60) Should have a submit bug command.
;; 2 (300) Check for and note circular references.
;; 3 (200) Insert cell references when they are created in formula
;;         more generally, probbaly have to have a check when creating a new
;;         cell, for can live that it is null until it has a value.
;; 3 (100) incorporate new frame-pop.el from David Smith
;; 3 (40) make the foramt of saved files more readable and concise
;; 3 (?) recover-file fails
;; 2 (20) chaning a null cell to a values does not update formula depending on it
;; 1 (15) yank does not update cells that rely on the yanked into cell
;; 2 (25) Allow the row labels to be hid
;; 2 (30) takes 10 seconds to load.  Cut this in half with autoloads 
;;         (or single sourcing?)
;; 2 (?) bigger numbers, see number-size-bug.txt
;; 2 (?) tie to calc
;; 3 (20) redraw-visible area C-c C-m L
;; 3 (30) on delete col, offer to save contents
;; 2 (30) on paste cells, cleanup long strings
;; 2 (10) tidy n,p,f,b commands
;; 2 (15) when inserting a row, update the ruler row
;; 3 (?)  in general, do timing of routines
;;        load write5.dis, 8:18, unit, 8:43
;; 3 (15) on insert col, adjust dis-middle-col
;; 2 (50) Compute average delay of slots, 
;;    eg. v9 matches 32 between two segmetns that match at 61 & 62
;; also get this error with 18.54: could not sbrk, return=1
;; ? (?)  Show state changes
;; ? (?)  support coding before matching
;; 3 (25) updating formula on row additions does it twice
;; ?      How to update ranges in formula upon cell insertion/deletion
;;     If insert-range (delete-range)
;; 3 (40) Check each fun in function-list
;;    If range of insertion after range of fun, do nothing
;;    If range of insertion includes range of fun, make range bigger
;;         setting row-end to  row-end + insert
;;    If range of insertion before range of fun, move range
;;         setting row-start to  row-start + insert, and for row-end
;;    Do for columns too
;; 3 (20) dis-max on a range
;; 3 (20) When printing, check for long line of blanks
;; 3 (20) General function to set a variable 
;; 3 (20) when printing a message about the cell contents, substitute
;;        %% for % in the string.
;; 3 (50) count number and types of match, i.e., a report in cells
;; 3 (30) add tally and report to dismal and menus
;; 3 (240) typing should put stuff into a cell, what's this = thing?
;;        have to move all keys off of a-zA-Z map
;;        if cell is filled, query for editing?
;;        18-Jul-92 -Bob@gnu
;;        could we just change jump-to-cell to end with an edit?
;; 3 (45) insert-range takes 30s to insert 2 cells on 30x550.  Make 25% faster
;; 3 (30) make delete-column to be a case of delete-range
;; 3 (20) dismal-backward-filled-column breaks searching back when wrapping?
;; 3 (20) on printout, put ruler and top cell label (a, b, etc) up
;; 3 (10) better printout name on pprint
;; 3 ()   do printing of cells better by simply drawing R to L (?!)
;; 3 (40) How to do cleanup right:
;;       - Find farthest away dirty cell in direction you are cleaning
;;       - Walk back to current cell unmarking and cleaning up
;;       - Do the other direction 
;;       - if pointer is used (but not by you) or cell has value, redraw it
;; 3 (25:68) support search, C-s, so that it leaves you in the cell you 
;;        started from and so that it matches numbers
;; 4 (30) display cell expr's instead of val's, by setting a flag bob 19-Jul-92
;; 3 (15) yank text from kill-ring, ie, from another buffer into a cell, M-C-y
;; 3 (20) On yank, redraw rows with wide cells
;; 3 (40) support query replace for numbers
;; 3 (120:35+30+35+10) fix all the little commands suggested in the keymap
;; 3 (15) if end up in first window line, should pop away from display bug
;; 4 (40)  add a calculation dialog menu, to set iterations, etc.
;; 4 (?)  Add database facilities, of sorting and searching
;; 4 (?)  Consider "clicking" on a trace line and jumping to the trace? model?
;; 4 (?)  Consider using watch 0 and adding your own trace
;; 4 (180) hide rows
;; 4 (80) we can't split buffer in two, at least not for long.  Support
;;            two windows on same buffer, with different points
;;        - with window-buffer-local variables
;;        - using two different buffers with the same data structures
;; 4 (15) check for simple circular dependencies (does cell depend on itself)
;; 4 (600) Support Undo.
;; 4 (240) support commas in numbers
;;         13-Jul-92 -Bob@gnu
;;         print out with commas (cell format type)
;;         read in with commas (all the time)
;;         dump commas

;; 
;; Done:
;; pays attention to make-backup-files, 28-May-97 -FER
;; 3 (35) can't put (+ a3 a4) in cell, must use f+
;; 3 (30) Can't open read-only files very well....
;; 3 (30) it would be nice to have the help come up at load time (?),
;;        offer hints as it goes along (?), be more integrated (?)
;; 3 (20) make popper a user preference
;; 16-Mar-95 -prints help on first load
;; 16-Mar-95 -fixed last (!) bug in relative cell movements
;; 3 (40:720) write autoalign function to take two cols, and line up
;; 2 (15:45) Save-some-buffers will not save files the right way, need to redefine 
;;       save-some-buffers to do .dis special, 20-Aug-93 -FER
;; 1 (?) previous-pa.dis somehow blew up from 24k to 769k, with a big null row
;;       was a basic pointer problem in how we assinged cells.  fixed now Oct-92
;; copy on left mouse button fixed. 6-Nov-92 -fer
;; shell-command fixed (for some), but using new popper.
;; * And related to this, is updating cell refernces,  If you have 
;;   the code in hand, I would be happy to cut in a fucntion that knows how
;;   to update the contents of a range when it is pasted in, and that could
;;   apply when a rectangle is inserted or deleted.  12-Jul-93 -FER/EMA?
;; 3 (15) yank does not invalidate cells


;;;
;;; 	I.	dismal-mode and startup code
;;;

(defun dismal-mode (&optional no-query)
  "A major mode for editing SpreadSheets.  
   (The version is available from M-x dismal-version)
A command menu is available by typing C-c C-m (C-m is also RET).
M-x dis-copy-to-dismal will paste text from another buffer into dismal.
The left mouse button is bound to dis-mouse-highlight-cell-or-range
and right mouse button is bound to dis-mouse-highlight-row.

 Special commands:\n\\{dismal-map}\n
 Special commands:\n\\{dismal-minibuffer-map}\n"
  (interactive)
  (if (and (not dismal-setup)
           (or (not dis-query-on-entry-p) no-query
               (y-or-n-p (format "Put %s into dismal-mode? "
                                 (buffer-name)))))
    (progn 
      (kill-all-local-variables)
      (use-local-map dismal-map)
      (setq mode-name "dismal")
      (setq major-mode 'dismal-mode)
      (auto-save-mode 0)
      (setq dismal-buffer-auto-save-file-name (make-auto-save-file-name))
      (setq truncate-lines t)
      (setq mode-line-format dismal-mode-line-format)
      ;; must be set in all buffers
      (setq dismal-matrix (dismal-create-matrix))
      (setq dismal-invalid-heapA (heap-create 'dismal-address-compare))
      (setq dismal-invalid-heapB (heap-create 'dismal-address-compare))
      (setq dismal-invalid-heap dismal-invalid-heapA)
      (setq dismal-invalid-heap-not dismal-invalid-heapB)
      (setq dismal-range-buffer [0 0 0])
      (aset dismal-range-buffer 2 (dismal-create-matrix))
      (setq dismal-current-row 0   dismal-current-col 0)
      ;; fixed 28-May-97 -FER
      (setq dismal-mark [0 0])
      ;; DBL cut this out, don't know why
      (setq dismal-column-formats (vector-create nil))
      (setq dismal-auto-save-counter dis-auto-save-interval)
      (setq dismal-write-file-version nil)
      (setq dismal-buffer-read-only buffer-read-only)
      (if buffer-read-only (setq buffer-read-only nil))

      ;; eval the stuff that makes sense, and then uncompress
      ;; 8-17-94 - FER
      ;; eval upto the dismal-matrix
      (let ((matrix-point 
                (save-excursion (goto-char 0)
                    (search-forward "setq dismal-matrix" (point-max) t)
                             (forward-line -1) (point))))
        ;; eval this first part, which may define dismal-save-compression
        (eval-region 0 matrix-point)
        (if (and dismal-save-compression
                 (not (= matrix-point 1))) ; new buffer
            (dismal-compress-region 
               (progn (goto-char 0)
                      (search-forward "setq dismal-matrix") (point))
               (point-max) t))
        (eval-region matrix-point (point-max)))
      (setq dismal-first-printed-column 
            ;; DBL: next line was (max 10 ...)
            (+ (1+ (truncate (log10 (max 1 dismal-max-row)))) ; numbers
               1 ; space
               (if dismal-row-label-lined 1 0)))
      (setq dismal-row-label-format
                  (format "%%%dd %s" 
                          (1+ (truncate (log10 (max 1 dismal-max-row))))
                          (if dismal-row-label-lined "|" "")))
      (if (or (not dismal-write-file-version)
              (not (string-equal dismal-write-file-version dismal-version)))
          (progn 
             (dismal-remove-floats)
             (setq dismal-write-file-version dismal-version)))
      (if (not (dismal-find-and-prepare-image)) ;DBL
          (progn
            (erase-buffer)
            ;; have to do this explicetly
            (switch-to-buffer (current-buffer)) 
            (let ((dis-show-ruler nil))
              (dis-redraw nil))))
      (setq dismal-middle-col-name
            (dismal-convert-number-to-colname dis-middle-col))
      (dismal-make-ruler)
      (set-buffer-modified-p nil)
      (run-hooks 'dis-mode-hooks)
      (when (and (not (get 'dismal-display-startup-message 'displayed))
                 (not dis-inhibit-startup-message))
        (add-hook 'post-command-hook 'dismal-display-startup-message-hook-fn))
      (setq dismal-setup t)
   ;; some convolutions here to get redraw to work in 19.34
   (goto-line 2)
   (goto-char 2)
   ;; (message "jumping3!") (sit-for 1)
   (dismal-visit-cell dismal-current-row dismal-current-col)
  )
  ;; if not going to be in dismal-mode, make sure auto-fill is off
  (auto-fill-mode -1)
))

;; taken from Hucka's SDE mode
(defvar dismal-startup-post-command-function 'dismal-display-startup-message)

(defun dismal-display-startup-message-hook-fn ()
  (when dismal-startup-post-command-function
    (funcall dismal-startup-post-command-function)))

;; (dismal-display-startup-message)
(defun dismal-display-startup-message ()
  "Display the dismal startup messages, including the copyright."
  (setq dismal-startup-post-command-function nil)
  (put 'dismal-display-startup-message 'checked t)
  (message (first dismal-startup-message-lines))
  (when (sit-for 1)
    (let ((lines dismal-startup-message-lines))
      (while (and (sit-for 4) lines)
	(message (substitute-command-keys (pop lines))))))
  (message ""))

(defun dismal-remove-floats ()
 "Remove old (Rosenblatt) floating point numbers if you find them."
  ;; remove floats
  (matrix-funcall-rc
     (function (lambda (row2 col2 cell)
       (let ((expression (dismal-get-cell-exp cell)))
                ;;(message "not converting %s" expression)
                 ;; (setq aa (cons expression aa))
         ;; convert a float to a rational  (dismal-float-expr-p (car aa))
         ;; (dismal-flat-format (car aa) 8)
         (if (dismal-float-expr-p expression)
             (progn
                (message "converting %s in %s %s to %s" expression
                         row2 col2 (car (read-from-string (dismal-flat-format 
                                  expression 8))))
                (sit-for 1)
             (dismal-set-exp row2 col2  
             (dismal-set-val row2 col2  
                (car (read-from-string (dismal-flat-format 
                                  expression 8)))))))       )))
       0 0
       dismal-max-row dismal-max-col dismal-matrix))

;; old version, before DBL version of 8-17-94-FER
;; (defun dismal-mode (&optional no-query)
;;   "A major mode for editing SpreadSheets.  Version 0.85+
;; A command menu is available by typing C-c C-m (C-m is also RET).
;; All the numerical values in the spreadsheet are floating point numbers as
;; implemented in the float.el package in the standard GNU emacs distribution.
;; Therefore you can put arbitrary emacs expressions into the cells which use
;; the floating point functions f+, f-, f*, f/, abs, fabs, f%, f=, f>, f>=,
;; f<, f<=, f/=, fmin, fmax, fzerop, float-to-string.
;;  Special commands:\n\\{dismal-map}\n
;;  Special commands:\n\\{dismal-minibuffer-map}\n"
;;   (interactive)
;;   (if (and (not dismal-setup)
;;            (or (not dis-query-on-entry-p) no-query
;;                (y-or-n-p (format "Put %s into dismal-mode? " (buffer-name)))))
;;     (progn (kill-all-local-variables)
;;       (use-local-map dismal-map)
;;       (setq mode-name "dismal")
;;       (setq major-mode 'dismal-mode)
;;       (auto-save-mode 0)
;;       (setq dismal-buffer-auto-save-file-name (make-auto-save-file-name))
;;       (setq truncate-lines t)
;;       (setq mode-line-format dismal-mode-line-format)
;;       (setq dismal-matrix (dismal-create-matrix)) ;must B set in all buffers
;;       (setq dismal-invalid-heapA (heap-create 'dismal-address-compare))
;;       (setq dismal-invalid-heapB (heap-create 'dismal-address-compare))
;;       (setq dismal-invalid-heap dismal-invalid-heapA)
;;       (setq dismal-invalid-heap-not dismal-invalid-heapB)
;;       (setq dismal-range-buffer [0 0 0])
;;       (aset dismal-range-buffer 2 (dismal-create-matrix))
;;       (setq dismal-current-row 0   dismal-current-col 0)
;;       (setq dismal-column-formats (vector-create nil))
;;       (setq dismal-auto-save-counter dis-auto-save-interval)
;;       (eval-current-buffer)
;;       (setq dismal-first-printed-column 
;;             (+ (1+ (log10 (max 10 dismal-max-row))) ; numbers
;;                1 ; space
;;                (if dismal-row-label-lined 1 0)))
;;       (setq dismal-row-label-format
;;                   (format "%%%dd %s" (1+ (log10 (max 1 dismal-max-row)))
;;                           (if dismal-row-label-lined "|" "")))
;;       (erase-buffer)
;;       ;; have to do this explicetly
;;       (switch-to-buffer (current-buffer)) 
;;       (let ((dis-show-ruler nil))
;;         (dismal-redraw nil))
;;       (setq dismal-middle-col-name
;;             (dismal-convert-number-to-colname dis-middle-col))
;;       (dismal-make-ruler)
;;       (set-buffer-modified-p nil)
;;       (run-hooks 'dis-hooks)
;;       (setq dismal-setup t)
;;       (beep t) (beep t) )))

;; DBL. Added an option to save and reload the display image.  This saves
;;    the contents of the displayed buffer in a backward-compatible
;;    format.  Upon reloading, the presence of the saved image
;;    eliminates the need to redraw the spreadsheet from saved variables.
;;    The save action is controlled by the dismal-save-image option,
;;    and the restore action is controlled by dismal-load-image.
;;    Functions dismal-write-buffer and dismal-mode were altered.
;;    
;;    The speedup from this change is dramatic.  On my SE/30 running
;;    Marc's 1.14b1 release, reading a spreadsheet of 150 rows by 33
;;    columns took 40 minutes using the unmodified Dismal 0.92.
;;    Saving this spreadsheet with the display image increased its
;;    file size from 89K to 150K, but reduced the load time to 28
;;    seconds, an improvement of 39.5 minutes, or a speedup of 85
;;    times.  This factor will probably depend upon the Emacs
;;    implementation and the size and contents of the .dis file, but I
;;    think it should be a big win for anyone using Dismal.
;;

(defun dis-find-file (filename)
  "Edit file FILENAME in dismal-mode.
Switch to a buffer visiting file FILENAME, creating one if none exists."
  (interactive "FFilename: ")
  (let ((buffer (get-file-buffer filename)))
    (if (and buffer (buffer-name buffer))
        (pop-to-buffer buffer)
      (setq buffer (create-file-buffer filename)) ; Create a buffer to draw in
      (set-buffer buffer)                 ; Make it current
      (dismal-mode)                       ; Remember this kills local variables
      (setq buffer-file-name filename)    ;probably goes when redone
      (setq dismal-current-row 0          ; Reset the cursor
            dismal-current-col 0)
      (set-buffer-modified-p nil)
      (pop-to-buffer buffer))))

;; DBL
;; This looks for an image header in the current buffer, then
;; transforms the contents of the buffer back to display format.
(defun dismal-find-and-prepare-image ()
  (if (and dismal-load-image
           (goto-char 0)
           (re-search-forward "^;image.*$" nil t))
      (progn
        ;; let the display get created
        (message "Loading display image...")
        (delete-region (point-min) (1+ (point)))
        ;; this strips off the leading ;;
        (replace-regexp "^;\\(.*\\)" "\\1")
        (message "Loading display image...finished.")
        ;; set it back
        t)
    nil))


;;;
;;; 	II.	Other mode helpers & macros
;;;

(defmacro dismal-check-for-read-only ()
  '(if dismal-buffer-read-only
  (error "Can't change a cell in read-only buffer.  \
C-x C-q to change read-only.")))

(defun dis-toggle-auto-update ()
  "Toggle whether or not the spreadsheet is updated on each cell entry."
  (interactive)
  (setq dis-auto-update (not dis-auto-update))
  (if dis-auto-update
      (progn (message "Updating dismal-matrix")
             (dis-update-matrix))
      (dismal-save-excursion
        (dismal-redraw-cell dismal-current-row dismal-current-col t))))

(defun dis-toggle-show-update ()
  "Toggle whether or not the updated values are shown as they are changed."
  (interactive)
  (setq dis-show-update (not dis-show-update))
  (message "dis-show-update set to %s." dis-show-update))


;; Drawing leaves you in the right spot, but on entry
;; find-file puts you at point=0, this moves us to cell 0.0
(defun dismal-find-file-hook ()
  (if (eq major-mode 'dismal-mode)
      (progn (forward-line 2)
             (move-to-column (+ dismal-first-printed-column -1
                                (dismal-column-width 0)))
             (dismal-display-current-cell-expr 0 0))))

(add-hook 'find-file-hooks 'dismal-find-file-hook)

(defun dismal-set-first-printed-column ()
 (let* ((width (truncate (log10 (max 1 dismal-max-row))))
        (old-dismal-first-printed-column dismal-first-printed-column)
        (difference nil) )
 (setq dismal-first-printed-column
       (+ (1+ width) ; numbers
           1 ; a space
           (if dismal-row-label-lined 1 0)))
 (setq difference (- dismal-first-printed-column 
                     old-dismal-first-printed-column))
 (if (not (= 0 difference))
     (dismal-save-excursion 
        (setq dismal-row-label-format
              (format "%%%dd %s" (1+ (truncate (log10 (max 1 dismal-max-row))))
                      (if dismal-row-label-lined "|" "")))
        (if (> difference 0)
            (string-rectangle (point-min) (point-max) " ")
           ; a speed improvement inspired by 
           ; Dan Nicolaescu <done@ece.arizona.edu>, 17-Jun-97 -FER
           ; (dismal-insert-blank-box (point-min)
           ;                          (+ dismal-first-data-line
           ;                             dismal-max-row) 1 " ")
          (let ((start (point-min))
                (end (save-excursion (goto-char (point-max))
                                     (beginning-of-line)
                                     (forward-char (- difference))
                                     (point))) )
            (kill-rectangle start end)) )))))

;;; This code is only for compiling the 'new' version.  Normal users will find 
;;; make a better approach.  They can also use byte-recompile-directory.
(defconst dismal-files
         '("/dismal-simple-menus" ;; put here first for a reason
                                     ;; this puts duplicates on with reloads
           "/dismal-metacolumn"
           ;; "/float" 
           ;; "/popper"
           "/float-changes"       
           "/vectors"
           "/heaps"                  "/rmatrix"
           "/dismal-mouse-x"
           "/dismal-mode-defaults"   "soar-misc"
           "/simple-menu"
           "/dismal")
  "For development only.  Use make instead.")

;; (defun dismal-compile-n-load-dismal ()
;;   "For development only.  Use make instead.
;; Byte compile and load all the files dismal uses."
;;   (interactive)
;;   (dismal-compile-dismal)
;;   (mapc (function (lambda (x) (load x)))
;;         (cdr dismal-files)))

;; FUNCTIONS USED IN DEFVAR INITIALIZERS have to be defined before they
;; appear, since they are actually invoked when the file is loaded.
;;

;; (dismal-address-compare '(1 . 2) '(1 . 3))
(defun dismal-address-compare (addr1 addr2)
  ;; Compare function for two addresses.
  (let ((rowdiff (- (dismal-address-row addr1) (dismal-address-row addr2))))
    (if (= rowdiff 0)
        (- (dismal-address-col addr1) (dismal-address-col addr2))
      rowdiff)))


;;;
;;;	IIa.	Ruler code
;;;

(defun dismal-draw-ruler (expected-current-row)
  ;; Uses expected-current-row, b/c it is draw before current-row is updated
  (sit-for 0)  ;this sit-for appears to be necssary, which is v. weird.
  (dismal-save-excursion
  (let ((new-ruler-row (- expected-current-row (current-line-in-window)))
        (buffer-originally-clean (not (buffer-modified-p))))
  (if (and dis-show-ruler (>= new-ruler-row -1))
      (progn
       (save-excursion
         (let ((current-line (current-line-in-window)) )
         (forward-line (- current-line))
         (delete-region (point) (save-excursion (forward-line 1)
                                                (end-of-line) (point)))
         (insert dismal-ruler))   )
       (setq dismal-current-first-ruler-row
             (- expected-current-row (current-line-in-window))) ))
  (if buffer-originally-clean (set-buffer-modified-p nil)))))

(defun dismal-undraw-ruler-rows ()  ;(dismal-undraw-ruler-rows)
  (let ((buffer-originally-clean (not (buffer-modified-p))))
  (if (numberp dismal-current-first-ruler-row)
      (if (>= dismal-current-first-ruler-row 0)
          (dismal-save-excursion
             (dismal-goto-row dismal-current-first-ruler-row nil)
             (delete-region (point) (save-excursion (end-of-line) (point)))
             (forward-line 1)
             (delete-region (point) (save-excursion (end-of-line) (point)))
           (dismal-redraw-row dismal-current-first-ruler-row nil)
           (dismal-redraw-row (1+ dismal-current-first-ruler-row) nil)
           (setq dismal-current-first-ruler-row nil))
        (if (>= dismal-current-first-ruler-row -1)
            (dismal-save-excursion
               (dismal-goto-row dismal-current-first-ruler-row nil)
               (delete-region (point) (save-excursion (end-of-line) (point)))
               (dismal-goto-row (1+ dismal-current-first-ruler-row) nil)
               (delete-region (point) (save-excursion (end-of-line) (point)))
               (dismal-redraw-row (1+ dismal-current-first-ruler-row) nil)
               (setq dismal-current-first-ruler-row nil)
               (dismal-draw-column-labels)))))
  (if buffer-originally-clean (set-buffer-modified-p nil))))

(defun dismal-make-ruler ()
  (cond (dis-ruler-row
         (save-excursion
           (goto-line (+ dismal-first-data-line dis-ruler-row))
           (setq dismal-ruler
                 (buffer-substring (point) (progn (end-of-line) (point))))))
        (t (setq dismal-ruler "")) )
  ;; Add the dashed line below.
  (setq dismal-ruler
        (concat dismal-ruler
         (save-excursion
           (goto-line (- dismal-first-data-line 1))
           (buffer-substring (1- (point)) (progn (end-of-line) (point)))))))

(defun dismal-increment-ruler (start-row arg)
 ;; update the location of the ruler if necessary
 (if (and dis-ruler-row
          (<= start-row dis-ruler-row))
     (let ((dis-show-ruler t))
       (dismal-undraw-ruler-rows)
       (setq dis-ruler-row (+ dis-ruler-row arg)) 
       (sit-for 0)
       (dismal-make-ruler)
       (dismal-draw-ruler dismal-current-row))     ) )

(defun dis-update-ruler (arg)
 "Move ruler to top of screen.  If ARG is supplied, remakes the ruler."
 (interactive "p")
 (let ((dis-show-ruler t))
   (dismal-save-excursion
     (dismal-undraw-ruler-rows)
     (if arg (dismal-make-ruler))
     (dismal-draw-ruler dismal-current-row))))

(defun dis-set-ruler-rows (row-to-use)
  "Set the row to use a ruler and redraws it.  Set to -2 to get letters."
  (interactive (list (dismal-read-minibuffer
                          (format "Replace ruler row <%s> with: "
                                  dis-ruler-row)
                           t (prin1-to-string dismal-current-row))))
  (if (< row-to-use 0) (setq row-to-use -2))
  (dismal-save-excursion
    (setq dis-ruler-row row-to-use)
    ;; go grab it
    (dismal-make-ruler)
    (dismal-undraw-ruler-rows)
    (dismal-draw-ruler dismal-current-row)))

(defun dis-set-ruler (use-ruler)
  (interactive (list (dismal-read-minibuffer "Use ruler t/nil: " t
                                   (prin1-to-string dis-show-ruler))))
  (let ((old-ruler dis-show-ruler))
  (cond ((and old-ruler use-ruler) nil)
        ((and (not old-ruler) (not use-ruler)) nil)
        ((and old-ruler (not use-ruler))
         (dismal-undraw-ruler-rows)
         (setq dis-show-ruler use-ruler))
        (t (setq dis-show-ruler use-ruler)))))


;;;
;;;	IIb.	Stuff taken from float.el
;;;

;;    Moved to float-changes.el.   2-Jan-97 -FER


;;;
;;;	III.	Set up the keymaps
;;;

(defvar dismal-keybinding-bug-holder nil
   "Place to hold a potential bug report.")

;; This is used outside of dismal, so must rebind.
(cond ((not (key-binding dismal-copy-to-dismal-binding))
       (global-set-key dismal-copy-to-dismal-binding 'copy-to-dismal))
      ((eq (key-binding dismal-copy-to-dismal-binding) 'copy-to-dismal))
      (t (message 
         "Change value of dismal-copy-to-dismal-binding, %s already used as %s"
           (key-description dismal-copy-to-dismal-binding)
           (key-binding dismal-copy-to-dismal-binding))
         (setq dismal-keybinding-bug-holder
               (cons (key-description dismal-copy-to-dismal-binding)
                     (key-binding dismal-copy-to-dismal-binding)))
         (beep)
         (sit-for 4)))

(if dismal-map
    ()
  (setq dismal-map (make-keymap))
  (suppress-keymap dismal-map)

  (setq dismal-insert-prefix-map (make-sparse-keymap))
  (setq dismal-delete-prefix-map (make-sparse-keymap))
  (setq dismal-ctl-x-map (make-sparse-keymap))
  (setq dismal-ctl-c-map (make-sparse-keymap))

  ;; could del work appropriately?

  ;; box keys first
  (if (not dismal-xemacs-p)
      (progn 

        (define-key dismal-map [begin] 'dis-first-column)
        (define-key dismal-map [up] 'dis-backward-row)
        (define-key dismal-map [down] 'dis-forward-row)
        (define-key dismal-map [left] 'dis-backward-column)
        (define-key dismal-map [right] 'dis-forward-column)
        (define-key dismal-map [home] 'dis-first-column)
        (define-key dismal-map [C-home] 'dis-beginning-of-buffer)
        (define-key dismal-map [end] 'dis-end-of-row)
        (define-key dismal-map [C-end] 'dis-end-of-buffer)
        (define-key dismal-map [prior] 'dis-scroll-down-in-place)
        (define-key dismal-map [next] 'dis-scroll-up-in-place)
        (define-key dismal-map "\t" 'dis-forward-column) )

  ;; else, do them for xemacs

    (progn
      (define-key dismal-map 'begin 'dis-first-column)
      (define-key dismal-map 'up 'dis-backward-row)
      (define-key dismal-map 'down 'dis-forward-row)
      (define-key dismal-map 'left 'dis-backward-column)
      (define-key dismal-map 'right 'dis-forward-column)
      (define-key dismal-map 'home 'dis-first-column)
      (define-key dismal-map '(control home) 'dis-beginning-of-buffer)
      (define-key dismal-map 'end 'dis-end-of-row)
      (define-key dismal-map '(control end) 'dis-end-of-buffer)
      (define-key dismal-map 'prior 'dis-scroll-down-in-place)
      (define-key dismal-map 'next 'dis-scroll-up-in-place)
      (define-key dismal-map 'tab 'dis-forward-column))  )

  ;; plain keys now
  (define-key dismal-map "?" 'describe-mode)
  (define-key dismal-map "<" 'dis-edit-cell-leftjust)
  (define-key dismal-map ">" 'dis-edit-cell-rightjust)
  ;; (define-key dismal-map "\"" 'dis-edit-cell-string)
  ;; (define-key dismal-map "'" 'dis-edit-cell-string)
  (define-key dismal-map "=" 'dis-edit-cell-default)
  (define-key dismal-map "|" 'dis-edit-cell-center)
  (define-key dismal-map "\ " 'dis-forward-column)
  (define-key dismal-map "c" 'dis-copy-range)
  (define-key dismal-map "d" dismal-delete-prefix-map)
  (define-key dismal-map "dc" 'dis-delete-column)
  (define-key dismal-map "dd" 'dis-delete-range)
  (define-key dismal-map "dr" 'dis-delete-row)
  (define-key dismal-map "d " 'dis-delete-blank-rows)
  (define-key dismal-map "e" 'dis-edit-cell-plain)
  (define-key dismal-map "f" 'dis-read-column-format)
  (define-key dismal-map "h" 'dis-help)
  (define-key dismal-map "i" dismal-insert-prefix-map)
  (define-key dismal-map "ic" 'dis-insert-column)
  (define-key dismal-map "ii" 'dis-insert-range)
  (define-key dismal-map "iz" 'dis-insert-z-box)
  (define-key dismal-map "i." 'dis-insert-cells)
  (define-key dismal-map "ir" 'dis-insert-row)
  (define-key dismal-map "j" 'dis-jump)
  (define-key dismal-map "m" 'dis-set-mark)
  (define-key dismal-map "n" 'dis-next-filled-row-cell)
  (define-key dismal-map "p" 'dis-previous-filled-row-cell)
  (define-key dismal-map "q" 'dis-bury-buffer)
  (define-key dismal-map "r" 'dis-hard-redraw-row)
  (define-key dismal-map "v" 'dis-paste-range)
  (define-key dismal-map "x" 'dis-kill-range)
  (define-key dismal-map "z" 'dis-redraw-range)

  ;; C-j newline-and-indent should goto work better
  ;; C-o should work appropriately
  ;; C-r should goback to search
  ;; C-z should work appropriately
  ;; C-x i should be rebound
  ;; C-x [ and C- ] (paging) should work appropriately
  ;; C-x > C-x < scroll right & left

  (define-key dismal-map "\C-?" 'dis-backward-kill-cell) ;del
  ;; very tricky key definition follows, allowing C-space to work:
  (if (not dismal-xemacs-p)
      (define-key dismal-map [?\C-\ ] 'dis-set-mark))
  (define-key dismal-map "\C-@" 'dis-set-mark)
  (define-key dismal-map "\C-a" 'dis-first-column)
  (define-key dismal-map "\C-b" 'dis-backward-column)
  (define-key dismal-map "\C-c" dismal-ctl-c-map)
  (define-key dismal-map "\C-c\C-m" 'dis-run-menu)
  ;; something binds it to insert mail buffer, which is dangerous
  (define-key dismal-map "\C-cm" 'dis-no-op) 
  (define-key dismal-map "\C-d" 'dis-clear-cell)
  (define-key dismal-map "\C-e" 'dis-end-of-row)
  (define-key dismal-map "\C-f" 'dis-forward-column)
  (define-key dismal-map "\C-k" 'dis-kill-line)
  ;; this appears to be too slow, leave as plain recenter
  ;(define-key dismal-map "\C-l" 'dis-recenter)
  (define-key dismal-map "\C-m" 'dis-forward-row)
  (define-key dismal-map "\C-n" 'dis-forward-row)
  (define-key dismal-map "\C-o" 'dis-open-line)
  (define-key dismal-map "\C-p" 'dis-backward-row)
  (define-key dismal-map "\C-r" 'dis-isearch-backwards)
  (define-key dismal-map "\C-s" 'dis-isearch)
  (define-key dismal-map "\C-t" 'dis-no-op) ; transpose-chars
  (define-key dismal-map "\C-q" 'dis-quoted-insert)
  (define-key dismal-map "\C-w" 'dis-kill-range)
  (define-key dismal-map "\C-xu" 'dis-undo)
  (define-key dismal-map "\C-_" 'dis-undo)
  (define-key dismal-map "\C-v" 'dis-scroll-up-in-place)
  (define-key dismal-map "\C-x" dismal-ctl-x-map)
  (define-key dismal-map "\C-xi" 'dis-insert-file)
  (define-key dismal-map "\C-x\C-i" 'dis-insert-file)
  (define-key dismal-map "\C-xr" 'dis-update-ruler)
  ;; (define-key dismal-map "\C-xs" 'dis-save-some-buffers)
  (define-key dismal-map "\C-x\C-s" 'dis-save-file)
  (define-key dismal-map "\C-x\C-w" 'dis-write-file)
  (define-key dismal-map "\C-x\C-x" 'dis-exchange-point-and-mark)
  (define-key dismal-map "\C-x[" 'dis-start-of-col)
  (define-key dismal-map "\C-x]" 'dis-end-of-col)
  (define-key dismal-map "\C-x>" 'dis-no-op) ; set-fill-prefix
  (define-key dismal-map "\C-x\C-q" 'dis-toggle-read-only) 
  (define-key dismal-map "\C-y" 'dis-paste-range)

  (define-key dismal-map "\C-c\M-\C-c" 'dis-op-code-segment)
  ;; M-a should work appropriately
  ;; M-m back-to-indentation should work appropriately
  ;; M-r replace-string should work appropriately
  ;; M-y yank-pop should work appropriately
  ;; M-z down-one-line 
  ;; M-del should work appropriately?

  (define-key dismal-map "\M-\C-?" 'dis-backward-kill-cell) ;del
  (define-key dismal-map "\M-\ " 'dis-backward-column)
  (define-key dismal-map "\M-<" 'dis-beginning-of-buffer)
  (define-key dismal-map "\M->" 'dis-end-of-buffer)
  (define-key dismal-map "\M-[" 'dis-no-op) ; 
  (define-key dismal-map "\M-]" 'dis-no-op) ; not bound
  (define-key dismal-map "\M-\t" 'dis-backward-column)
  (define-key dismal-map "\M-a" 'dis-no-op) ; backward-sentence
  (define-key dismal-map "\M-b" 'dis-backward-filled-column)
  (define-key dismal-map "\M-c" 'dis-capitalize-cell)
  (define-key dismal-map "\M-d" 'dis-kill-cell)
  (define-key dismal-map "\M-e" 'dis-last-column)
  (define-key dismal-map "\M-f" 'dis-forward-filled-column)
  (define-key dismal-map "\M-g" 'dis-no-op) ; fill-region
  (define-key dismal-map "\M-h" 'dis-no-op) ; mark-paragraph
  (define-key dismal-map "\M-i" 'dis-no-op) ; tab-to-tab-stop
  (define-key dismal-map "\M-j" 'dis-align-metacolumns) ; fill-paragraph
  (define-key dismal-map "\M-k" 'dis-no-op) ; kill-sent
  (define-key dismal-map "\M-l" 'dis-downcase-cell)
  (define-key dismal-map "\M-n" 'dis-next-filled-row-cell)
  (define-key dismal-map "\M-o" 'dis-insert-range)
  (define-key dismal-map "\M-p" 'dis-previous-filled-row-cell)
  (define-key dismal-map "\M-q" 'dis-query-replace)
                                      ;; used to be replace-string
  (define-key dismal-map "\M-r" 'dis-move-to-window-line)
  (define-key dismal-map "\M-t" 'dis-transpose-cells) ;used 2be transpose-words
  (define-key dismal-map "\M-u" 'dis-upcase-cell)
  (define-key dismal-map "\M-v" 'dis-scroll-down-in-place)
  (define-key dismal-map "\M-w" 'dis-copy-range)
  (define-key dismal-map "\M-=" 'dis-debug-cell)
  (define-key dismal-map "\M-%" 'dis-query-replace)
  (define-key dismal-map "\M-," 'dis-no-op) ; tags-loop-continue

  ;; C-M-b, f, a, & e should work appropriately
  (define-key dismal-map "\M-\C-k" 'dis-no-op) ;kill-sexp
  (define-key dismal-map "\M-\C-e" 'dis-erase-range)
  (define-key dismal-map "\M-\C-m" 'dis-backward-row)
  (define-key dismal-map "\M-\C-r" 'dis-redraw)
  (define-key dismal-map "\M-\C-t" 'dis-transpose-cells) ; used to be transpose-sexps
  (define-key dismal-map "\M-\C-u" 'dis-update-matrix) ; used to be backward-up-list
  ;; dis-recalculate-matrix
  )

(defvar dismal-minibuffer-local-map nil "")
(if dismal-minibuffer-local-map
    ()
  (setq dismal-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key dismal-minibuffer-local-map "\C-j" 'dismal-exit-minibuffer-down)
  (define-key dismal-minibuffer-local-map "\C-m" 'exit-minibuffer)
  (define-key dismal-minibuffer-local-map "\C-n" 'dismal-exit-minibuffer-down)
  (define-key dismal-minibuffer-local-map "\C-p" 'dismal-exit-minibuffer-up)
  ;; (define-key dismal-minibuffer-local-map "\M-\C-b" 'dismal-exit-minibuffer-left)
  ;; (define-key dismal-minibuffer-local-map "\M-\C-f" 'dismal-exit-minibuffer-right)
  )


;; Support undo in the minibuffer since we use it so much
(buffer-enable-undo (window-buffer (minibuffer-window)))

(defun dismal-exit-minibuffer-down ()
  (interactive)
  (save-excursion (set-buffer dismal-buffer-using-minibuffer)
     (push '(dis-forward-row 1) dismal-delayed-commands))
     (exit-minibuffer))

(defun dismal-exit-minibuffer-up ()
  (interactive)
  (save-excursion (set-buffer dismal-buffer-using-minibuffer)
    (push '(dis-forward-row -1) dismal-delayed-commands))
  (exit-minibuffer))

;(defun dismal-exit-minibuffer-right ()
;  (interactive)
;  (save-excursion (set-buffer dismal-buffer-using-minibuffer)
;    (push '(dis-forward-column 1) dismal-delayed-commands))
;  (exit-minibuffer))

;(defun dismal-exit-minibuffer-left ()
;  (interactive)
;  (save-excursion (set-buffer dismal-buffer-using-minibuffer)
;      (push '(dis-backward-column 1) dismal-delayed-commands))
;  (exit-minibuffer))


;;;
;;;	IV.	Dismal versions of commands
;;;

(defun dis-undo (arg)
 "Undo how the screen is drawn.  Not a real dismal undo."
 (interactive "P")
 (advertised-undo arg)
 (message "Not a real dismal undo, only changing how buffer looks!")
 (beep t))

(defun dis-toggle-read-only ()
  "Toggle the read-only-ness of a dismal buffer."
  (interactive)
  (setq dismal-buffer-read-only (not dismal-buffer-read-only))
  (sit-for 0))

;; this should maybe be just C-h m
(defun dis-help ()
  "Function point for the dismal help function, such as it is."
  (interactive)
  (require 'info)
  (Info-goto-node (concat "(" dismal-directory "/dismal.info" ")Top")))

(defun dis-bury-buffer (&optional buffer)
  "Bury the current buffer and notify user."
  (interactive)
  (message "Burying %s as a way of quiting dismal..." 
           (buffer-name (current-buffer)))
  (bury-buffer nil))

;; old dismal-query-replace-guts by FER
(defun dismal-query-replace-guts (i j prompt)
  (if (cond ((and (stringp cell-value)
             (string-match from-string cell-value)))
            (t (equal cell-value from-string)))
      (progn (dismal-jump-to-cell i j)  ;; present match
        (message prompt)                ;; query for action
        (setq prompt-result (downcase (read-char)))
        (dismal-save-excursion
        (cond ((or (= prompt-result ?\ ) (= prompt-result ?y))
               (setq match-start
                   (string-match from-string cell-value))
               (dismal-set-exp i j  ;; need to be careful about
               (dismal-set-val i j  ;; need to be careful about
                   (if (stringp from-string)
                       (concat (substring cell-value 0 match-start)
                               to-string
                               (if (> (match-end 0) (length cell-value))
                                   ""
                                        (substring cell-value (match-end 0))))
                      to-string)))
               (dismal-redraw-cell i j t))
             ;; skip on del (127) and n
              ((or (= prompt-result ?n) (= prompt-result 127)))
              ;; C-h goes here too to give help
              ((= prompt-result ?\h)
               (with-output-to-temp-buffer "*Help*"
                 (princ query-replace-help)))
              ;; quit on anything else
             (t (setq done t)))))))

(defun dis-query-replace (from-string to-string)
  "Replace some occurrences of FROM-STRING with TO-STRING.
Currently only works for cells with string values.
As each match is found, the user must type a character saying
what to do with it.  For directions, type C-h at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries."
;; Query replacing aa with bb.
;; 
;; Type Space or `y' to replace one match, Delete or `n' to skip to next,
;; ESC or `q' to exit, Period to replace one match and exit,
;; Comma to replace but not move point immediately,
;; C-r to enter recursive edit (ESC C-c to get out again),
;; C-w to delete match and recursive edit,
;; C-l to clear the screen, redisplay, and offer same replacement again,
;; ! to replace all remaining matches with no more questions,
;; ^ to move point back to previous match.
  (interactive
   (let ((f-string (dismal-convert-input-to-cellexpr 
                      (read-string "Dismal query replace: "))))
     (list f-string
           (dismal-convert-input-to-cellexpr
               (read-string (format "Dismal query replace %s with: " 
                                    f-string))))))

  (dismal-set-mark dismal-current-row dismal-current-col)
  (let ((i dismal-current-row)
        (j dismal-current-col)
        (done nil) cell-value  match-start
        prompt-result
        (prompt (format "Dismal query replacing %s with %s:"
                        (dismal-convert-cellexpr-to-string from-string)
                        (dismal-convert-cellexpr-to-string to-string)) ))
   (while (and (not done) (<= i dismal-max-row))
     (while (and (not done) (<= j dismal-max-col))
       (setq cell-value (dismal-get-exp i j))
       ;;(message "Doing %s:%s with <<%s>> match: %s" i j cell-value
       ;;         (and (stringp cell-value)
       ;;              (setq match-start
       ;;                  (string-match from-string cell-value))))(sit-for 2)
       ;; search forward for a match
       (dismal-query-replace-guts i j prompt)
       (setq j (1+ j)) ) ; end while
     (setq j 0)
     (setq i (1+ i)) ) ;end while
   (message "Finished Dis Query replace.")
   (sit-for 1)))


(defun dis-isearch-backwards (search-string)
  "Do incremental search backwards in dismal, sorta.  Not started."
  (interactive "cDis I-search backward: ")
  (message "We don't do isearch-backwards yet."))

(defun dis-isearch (search-string)
  "Do incremental search forward in dismal, sorta.  Not complete.
As you type characters, they add to the search string and are found.
Type Delete to cancel characters from end of search string.
Type ESC to exit, leaving point at location found.
Type C-s to search again forward, C-r to search again backward.
Type C-w to yank word from buffer onto end of search string and search for it.
Type C-y to yank rest of line onto end of search string, etc.
Type C-q to quote control character to search for it.
Other control and meta characters terminate the search
 and are then executed normally.
The above special characters are mostly controlled by parameters;
 do M-x apropos on search-.*-char to find them.
C-g while searching or when search has failed
 cancels input back to what has been found successfully.
C-g when search is successful aborts and moves point to starting point."
  (interactive "cDismal I-search: ")
  (if (not (stringp search-string))
      (setq search-string (char-to-string search-string)))
  (let ((i dismal-current-row)
        (j dismal-current-col)
        (saved-i dismal-current-row)
        (saved-j dismal-current-col)
        (done nil) 
        result
        (prompt (format "Dismal I-search: %s" search-string)) )
   (while (and (not (eq result 'aborted)) (not done) (<= i dismal-max-row))
     (while (and (not (eq result 'aborted))
                 (not done) (<= j dismal-max-col))
       ;; search forward for a match
       (setq result (dismal-isearch-guts))
       (setq j (1+ j)) ) ; end while
     (setq j 0)
     (setq i (1+ i)) ) ;end while
   (cond ((eq result 'aborted)
          (dismal-jump-to-cell saved-i saved-j))
         ((not done)  (beep) ;; leave this beep without a t
          (message "Failing Dismal I-search: %s" search-string)
          (dismal-isearch-queryer)
          (if (not done)
              (dismal-jump-to-cell saved-i saved-j)))
         (t (dis-set-mark))) ))

(defun dismal-isearch-guts ()
 ;(message "starting isearch-guts with %s at %s %s" search-string i j)
 (let ( match-start
         (cell-value (dismal-get-val i j)))
  (if (and (stringp cell-value)
           (setq match-start (string-match search-string cell-value)))
      (progn (dismal-jump-to-cell i j)  ;; present match
        (message prompt)                ;; query for action
        (dismal-isearch-queryer)))))

(string-match "[a-zA-Z0-9!@#$%^&*-]" "-")

;; done is used across these functions as a flag
(defun dismal-isearch-queryer ()
  (let (next-char)
  (setq next-char (char-to-string (read-char)))
;; (message "  in isearch-guts with next-char %s" next-char)
;; (setq aa next-char)
  (cond ((string-match "[a-zA-Z0-9!@#\\-$%^&*-]" next-char)
         (setq search-string (concat search-string next-char))
         (setq prompt (format "Dismal I-search: %s" search-string))
               ;(message "  in isearch-guts with %s match %s" search-string
               ;            (string-match search-string cell-value))
         (if (string-match search-string cell-value)
             (dismal-isearch-guts)))
        ((string-match "[]" next-char))
        ((string-match "[]" next-char) (setq done t))
        ;; quit on anything else
        ((string-match "[]" next-char) 'aborted)
        (t (call-interactively (key-binding next-char))
                 ;(my message "just did interactively call/")
           (setq done t)))))


;; (dismal-search "comint-" 1)
(defun dismal-search (search-string &optional times)
  "Do nonincremental search forward for SEARCH-STRING times TIMES."
  (interactive "cDismal search: ")
  (if (not (stringp search-string))
      (error "Search string %s must be a string"))
  (if (not (numberp times)) (setq times 1))
  (let ((i dismal-current-row)
        (j dismal-current-col)
        (saved-i dismal-current-row)
        (saved-j dismal-current-col)
        (done nil)
        cell-value
        result )
   (while (and (not done) (<= i dismal-max-row)
               (> times 0))
     (while (and (not done) (<= j dismal-max-col))
       ;; search forward for a match
       (setq cell-value (dismal-get-val i j))
       (if (and (stringp cell-value)
                (string-match search-string cell-value))
           (progn (dismal-jump-to-cell i j)
                  (setq times (- times 1))
                  (if (= 0 times) (setq done t))))
       (setq j (1+ j)) ) ; end inner while
     (setq j 0)
     (setq i (1+ i)) ) ;end outer while
   (cond ((not done)  (beep) ;; leave this beep without a t
          (message "Failing Dismal search: %s" search-string)
          (if (not done)
              (dismal-jump-to-cell saved-i saved-j)))
         (t (dis-set-mark))) ))


;;;
;;; 	V.	dismal-mark
;;;

;; (defmacro dismal-mark-col () '(aref dismal-mark 1))

(defun dis-set-mark ()
  "Set mark in dismal buffers to cell where point is at."
  (interactive)
  (dismal-set-mark dismal-current-row dismal-current-col)
  (if (eq window-system 'x)
      (dismal-add-text-properties (point-min) (point-max)
                                  '(face default)))
  (message "Dismal mark set."))

(defun dis-exchange-point-and-mark ()
  "Put the dismal mark where point is now, and point where mark is now."
  (interactive)
  (let ((temp-row (aref dismal-mark 0))
        (temp-col (aref dismal-mark 1)))

    (aset dismal-mark 0 dismal-current-row)
    (aset dismal-mark 1 dismal-current-col)
    (dismal-jump-to-cell temp-row temp-col)
  (if (eq window-system 'x)
      (dismal-highlight-range (aref dismal-mark 1) (aref dismal-mark 0)
                            dismal-current-col dismal-current-row))))


;;;
;;; 	VI.	Range and range-buffer functions
;;;
;;; dismal ranges look like:
;;;   (setq ar  '(dismal-range (dismal-r-c- 1 0) (dismal-r-c- 3 2)))

;; (inspect (dismal-string-to-range "A1:c3"))
(defsubst dismal-string-to-range (rangename)
  ;; Convert string RANGENAME to an expression representing a cell range.
  (string-match dismal-cell-range-regexp rangename)
  (let* ((from (substring rangename (match-beginning 1) (match-end 4)))
         (to (substring rangename (match-beginning 5) (match-end 8))))
    (list 'dismal-range
          (dismal-convert-cellname from)
          (dismal-convert-cellname to))))

(defun dis-copy-range ()
  "Copy a range into the mark buffer."
  (interactive)
  (dismal-select-range)
  (dismal-show-selected-range)
  (let ((start-row (range-1st-row dismal-cell-buffer))
        (start-col (range-1st-col dismal-cell-buffer))
        (end-row (range-2nd-row dismal-cell-buffer))
        (end-col (range-2nd-col dismal-cell-buffer))  )
  (dismal-note-selected-range "Copying range %s%d:%s%d")
  (matrix-copy start-row start-col end-row end-col
       0 0 dismal-matrix (range-buffer-matrix dismal-range-buffer))
  (aset dismal-range-buffer 0 (abs (- start-row end-row)))
  (aset dismal-range-buffer 1 (abs (- start-col end-col)))
  (dismal-note-selected-range "Copied range %s%d:%s%d")))


(defun dis-kill-range ()
  "Cut a range (mark + point) into the mark buffer."
  (interactive)
  (dismal-save-excursion
    (dismal-select-range)
    (if dis-show-selected-ranges 
       (progn (dismal-show-selected-range)
              (dismal-note-selected-range "Cutting range %s%s:%s%d...")))
    (let ((start-row (range-1st-row dismal-cell-buffer))
          (start-col (range-1st-col dismal-cell-buffer))
          (end-row (range-2nd-row dismal-cell-buffer))
          (end-col (range-2nd-col dismal-cell-buffer))
          (dismal-interactive-p nil)  )
    (matrix-copy start-row start-col end-row end-col
         0 0 dismal-matrix (range-buffer-matrix dismal-range-buffer))
    (aset dismal-range-buffer 0 (abs (- start-row end-row)))
    (aset dismal-range-buffer 1 (abs (- start-col end-col)))
    (matrix-funcall-rc
          (function (lambda (r c dummy)
              (dismal-set-cell r c nil nil)
              (dismal-cleanup-long-string r c)))
          start-row start-col end-row end-col dismal-matrix)
    (if dis-auto-update (dismal-private-update-matrix))
    (dis-redraw-range start-row end-row)
    (if dis-show-selected-ranges
       (dismal-note-selected-range "Cut range %s%s:%s%d")))))

(defun dis-erase-range ()
  "Erase a range without saving."
  ;; deliberately leaves saved range-buffer alone though...
  (interactive)
  (let ((old-range-buffer-r (aref dismal-range-buffer 0))
        (old-range-buffer-c (aref dismal-range-buffer 1)))
  (dismal-select-range)
  (if dis-show-selected-ranges (dismal-show-selected-range))
  (let ((start-row (range-1st-row dismal-cell-buffer))
        (start-col (range-1st-col dismal-cell-buffer))
        (end-row (range-2nd-row dismal-cell-buffer))
        (end-col (range-2nd-col dismal-cell-buffer))  )
  (dismal-note-selected-range "Erasing range %s%d:%s%d")
  (matrix-funcall-rc 
         (function (lambda (r c dummy) (dismal-set-cell r c nil nil)))
         start-row start-col end-row end-col dismal-matrix)
  (if dis-auto-update (dismal-private-update-matrix))
  (aset dismal-range-buffer 0 old-range-buffer-r)
  (aset dismal-range-buffer 1 old-range-buffer-c)
  (dismal-note-selected-range "Erased range %s%d:%s%d"))))

(defun dis-paste-range-as-text ()
  "Paste the dismal-cut-buffer as text into a text buffer."
  (interactive)
  (dismal-check-for-read-only)  
  (let ((end-row (aref dismal-range-buffer 0))
        (end-col (aref dismal-range-buffer 1)))
  (matrix-funcall-rc    ;; do the actual copy  
      (function (lambda (r c cell)
         (let ((expr (dismal-get-cell-exp cell)))
           (insert (if expr (format "%s" expr)) "\n"))))
      0 0 
      end-row end-col
      (range-buffer-matrix dismal-range-buffer))))

;; this still leaves a problem if block-block-block is in col 1,
;; and then aaa is pasted in th middle, block aaa block results
;; b/c the cleanup thingy when it cleansup, redraws the aaa (or ""),
;; and
(defun dis-paste-range ()
  "Paste a range into the spreadsheet."
  (interactive)
  (dismal-check-for-read-only)
  (if (not (numberp (aref dismal-range-buffer 0)))
      (error "No range selected"))
  (if dis-show-selected-ranges
     (dismal-note-selected-range "Pasting range %s%d:%s%d"))
  (if (not dismal-matrix)
     (dis-paste-range-as-text)
  (dismal-save-excursion
  (let* ((end-row (aref dismal-range-buffer 0))
         (end-col (aref dismal-range-buffer 1))
         (over-max-row (- (+ dismal-current-row end-row)
                          dismal-max-row))
         (range-first-row (range-1st-row  dismal-cell-buffer))
         (range-first-col (range-1st-col  dismal-cell-buffer)) )
  ;; this attempts to clean up long strings
  (matrix-funcall-rc
      (function (lambda (r c dummy)
         (dismal-set-exp r c "") ;or val should work here
         (dismal-set-val r c "") ;or val should work here
         (let ((old-mrk (dismal-get-mrk r c)))
           (if (and (consp old-mrk) old-mrk)
               (dismal-cleanup-long-string (car old-mrk) (cdr old-mrk)))
           (dismal-cleanup-long-string r c))))
      dismal-current-row dismal-current-col
      (+ dismal-current-row end-row) (+ dismal-current-col end-col)
      dismal-matrix)
  ;; this does the actual copy
  (matrix-funcall-rc    
      (function (lambda (r c cell)
         (let ((expr (dismal-get-cell-exp cell))
               (offrow (- dismal-current-row range-first-row)) 
               (offcol (- dismal-current-col range-first-col))
               (new-r (+ r dismal-current-row))
               (new-c (+ c dismal-current-col)) )
         ;; (dismal-set-exp new-r new-c
         ;;                (dismal-change-indices expr
         ;;                                 (- dismal-current-row orgrow)
         ;;                                 (- dismal-current-col orgcol)))
         ;; (dismal-set-val new-r new-c nil)
         ;; (dismal-invalidate-cell (dismal-make-address r c))
         (dismal-set-cell-internals new-r new-c 
                         (dismal-change-indices expr offrow offcol)
			 nil)
         (dismal-redraw-cell new-r new-c t))))
      0 0 
      end-row end-col
      ;; (if (= 0 end-row) 0 (1- end-row)) (if (= 0 end-col) 0 (1- end-col))
      (range-buffer-matrix dismal-range-buffer))
  ;; (matrix-copy 0 0 end-row end-col
  ;;    dismal-current-row dismal-current-col
  ;;    (range-buffer-matrix dismal-range-buffer) dismal-matrix)
  (setq dismal-max-col (max dismal-max-col (+ dismal-current-col end-col)))
  (if (> over-max-row 0)
      (progn (dismal-set-first-printed-column)
             (setq dismal-max-row (+ dismal-current-row end-row))
             (dismal-add-row-labels-at-end (1+ over-max-row))))
  ;; cleanup if you are written on, or if you might have wrote on someone
  (matrix-funcall-rc
      (function (lambda (r c dummy) 
         (let ((old-mrk (dismal-get-mrk r c)))
           (if (and (consp old-mrk) old-mrk)
               (dismal-cleanup-long-string (car old-mrk) (cdr old-mrk)))
           (dismal-cleanup-long-string r c)
           (dismal-redraw-cell r c t))))
      dismal-current-row dismal-current-col
      (+ dismal-current-row end-row) (+ dismal-current-col end-col)
      dismal-matrix)
  (dismal-visit-cell dismal-current-row dismal-current-col)
  (aset dismal-mark 0 (+ dismal-current-row end-row))
  (aset dismal-mark 1 (+ dismal-current-col end-col))
  (if dis-auto-update (dismal-private-update-matrix))
  (set-buffer-modified-p t) ))))

;; should be lableled make-range
;; should be done destructively
(defun dismal-select-range ()
  ;; Select a range, setting dismal-cell-buffer to hold the result.
  (if (not (aref dismal-mark 0)) (error "Mark not set")
  (let ((start-row dismal-current-row)   (start-col dismal-current-col)
        (end-row (dismal-mark-row))   (end-col (dismal-mark-col))
        result)
  (if (> start-row end-row)
      (progn (setq result start-row)
        (setq start-row end-row)
        (setq end-row result)))
  (if (> start-col end-col)
      (progn (setq result start-col)
         (setq start-col end-col)
         (setq end-col result)))
  (setq result
        (` (dismal-range (dismal-r-c- (, start-row) (, start-col))
                         (dismal-r-c- (, end-row)
                                             (, end-col)))))
  (aset dismal-range-buffer 0 nil) ; set to nil, 'cause now nothing is there
  (aset dismal-range-buffer 1 nil)
  (setq dismal-cell-buffer result))))

(defconst dismal-nsr-prompt "Selected range from %s%d to %s%d")

;   (dismal-note-selected-range "Deleting %s%s:%s%d...")
(defun dismal-note-selected-range (&optional prompt)
  (if dismal-interactive-p
      (message (or prompt dismal-nsr-prompt)
        (dismal-convert-number-to-colname (range-1st-col dismal-cell-buffer))
        (range-1st-row dismal-cell-buffer)
        (dismal-convert-number-to-colname (range-2nd-col dismal-cell-buffer))
        (range-2nd-row dismal-cell-buffer))))

(defun dismal-show-selected-range ()
  (if (not (eq (first dismal-cell-buffer) 'dismal-range))
      (error "No range selected")
    (let ((r1r (range-1st-row dismal-cell-buffer))
          (r1c (range-1st-col dismal-cell-buffer))
          (r2r (range-2nd-row dismal-cell-buffer))
          (r2c (range-2nd-col dismal-cell-buffer)) )
    (if (and (= r1r r2r) (= r1c r2c))
        nil
    (dismal-visit-cell (if (= dismal-current-row r1r) r2r r1r)
                       (if (= dismal-current-col r1c) r2c r1c))
    ;; could put scroll in here...
    (sit-for 1)
    (dismal-visit-cell dismal-current-row dismal-current-col)))))

;(defun dismal-generate-range (fromcell tocell)
;  "Return a list of the addresses a range refers to.  FROMCELL is
;a cell reference to the upper left corner of the range, and TOCELL
;is refers to the lower right corner.  Both are in the
;form (dismal-r-c- row col)."
;  ;; Scan cells backwards so it ends up forwards.
;  (let ((list nil)
;        (row (nth 1 tocell))
;        (row1 (nth 1 fromcell))
;        (col (nth 2 tocell))
;        (col1 (nth 2 fromcell)))
;    (while (>= col col1)
;      (while (>= row row1)
;        (setq list (cons (cons row (cons col nil)) list))
;        (setq row (1- row)))
;      (setq col (1- col))
;      (setq row (nth 1 tocell)))
;    list))

;(defun dismal-range (fromcell tocell)
;  "Return a list of the values of a range of cells.  FROMCELL and
;TOCELL are cell references, in the form (dismal-r-c- row col)."
;  (mapcar 'dismal-evaluate-cellref
;          (dismal-generate-range fromcell tocell)))

(defun dismal-range-is-rows-or-columns ()
  (dismal-select-range)
  (if (not (eq (first dismal-cell-buffer) 'dismal-range))
      (error "No range selected")
    (let ((r1r (range-1st-row dismal-cell-buffer))
          (r1c (range-1st-col dismal-cell-buffer))
          (r2r (range-2nd-row dismal-cell-buffer))
          (r2c (range-2nd-col dismal-cell-buffer)) )
      (cond ( (= r1r r2r) 'rows)
            ( (= r1c r2c) 'columns)
            (t nil)))))


;;;
;;;	VIIa.	Date functions: variables and inits
;;;

;;; 	Provides functions to insert the date and time into buffers.
;;; Original code by Erik Altmann.
;;;

(defvar dis-insert-date-with-month-namep t
  "*Print out the month in insert-date-string as letters, and in
30-Oct-91 order, rather than as 10-30-91.")

;; 3-6-91 -  tested only for march:
(defconst dismal-date-table
    '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
      ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
      ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  "Maps into numbers the month strings returned by current-time-string.")

(defvar dismal-national-weekday-table nil
  "*Alist that consists of weekday and its local representation.
Replace the second item in each list to use it.")
;; Shipped with English version, that is, no table.

;; Rough approximation table for Spanish use
;; '( ("Sun"  "Dom") ("Mon"  "Lun") ("Tue"  "Mar") ("Wed"  "Mie")
;;    ("Thu"  "Whe") ("Fri"  "Vie") ("Sat"  "Sab"))

(defvar dis-use-yymmdd-rep t
  "*Use yy/mm/dd format to represent a day." )

(defvar dis-insert-weekday-first t
  "*" )



;;;
;;;	VIIb.	Erik's insert-date-string & insert-time-string
;;;
;;; Code from Erik Altmann on a quick hack to insert date and time on 
;;; headerless files.  Not really necc. but what the heck.

;; 3-6-91 -
;; (current-time-string)
;; -> "Wed Mar  6 10:31:12 1991"
;;     012345678901234567890123

(defun insert-time-string ()
  "Inserts an Al-like time-stamp after point."
  (interactive)
  (insert-before-markers
   (format "%s%s" (substring (current-time-string) 11 13)
       (substring (current-time-string) 14 16))))

(defun insert-current-time-string ()
  "Inserts a full time-stamp after point."
  (interactive)
  (insert-before-markers
   (format "%s" (current-time-string))) )

;; (dis-insert-date-string nil)
;; (setq dis-insert-date-with-month-namep t)
(defun dis-insert-date-string (arg)
  "Inserts the current date after point, in d-m-y format.  With prefix
argument, inserts the month first."
  (interactive "P")
  (insert-before-markers (dis-current-date arg)) )



;;;
;;; 	VIII.	Changed movement functions
;;;

;; used to use
;;(require 'dismal-mouse-x)

;; moved down here so they would load, 19-Jun-96 -FER
(load "dismal-mouse3.el")
(load "dismal-menu3.el")


;; 2-8-93 - EMA: behaves just like move-to-window-line:
(defun dis-move-to-window-line (arg)
  "Position point relative to dismal window.
With no argument, position point at center of window.
An argument specifies screen line; zero means top of window,
negative means relative to bottom of window."
  (interactive "P")
  (let* ((distance-to-move
	  (cond ((null arg)		; go to middle row
                 (- (/ (window-height) 2) (current-line-in-window)))
	        ((minusp arg)		; displacement from bottom
                 (- (+ (1- (window-height)) arg) (current-line-in-window)))
                (t			; displacement from top
                  (- arg (current-line-in-window))))))
    (dismal-jump-to-cell (max 0 (+ dismal-current-row distance-to-move))
			 dismal-current-col)))

(defun dismal-scroll-in-place (arg)
  (let ((lines-from-top (current-line-in-window)))
  (dismal-undraw-ruler-rows)
  (let ((dis-show-ruler nil))
    (dismal-visit-cell arg dismal-current-col)
    (recenter lines-from-top)
    (setq dismal-current-row arg))
  (dismal-draw-ruler arg)  ))

;; 2-8-93 - EMA fix: "or arg" to "if arg (car arg)", to allow C-u to
;;   specify the number of lines to scroll.
(defun dis-scroll-up-in-place (arg)
  "Scroll cells of dismal window upward ARG lines or nearly a full screen if
no ARG.  When calling from a program, supply a number as argument or
nil.  Leaves point in same row and column of window [which seems wrong]."
  (interactive "P")
  (let* ((addition (if arg (car arg) (- (window-height) 2)))
         (scroll-to (+ dismal-current-row addition)))
    (dismal-scroll-in-place (min dismal-max-row scroll-to))
    (if (> scroll-to dismal-max-row)
        (progn (beep)
               (message "End of buffer")))))

;; you could make the -2 here an arg for how much to bump up
(defun dis-scroll-down-in-place (arg)
  "Scroll cells of dismal window down ARG lines or nearly a full screen if
no ARG.  When calling from a program, supply a number as argument or
nil.  Leaves point in same row and column of window [which seems wrong]."
  (interactive "P")
  (dismal-scroll-in-place (max 0 (- dismal-current-row (window-height) -2))))

(defun dis-forward-column (cols)
  "Move forward COLS columns."
  (interactive "p")
  (dismal-move-columns cols)
  (dismal-visit-cell dismal-current-row dismal-current-col))

;; moves over hidden columns
(defun dismal-move-columns (arg)
 (let ((n (abs arg))
       (direction (signp arg)))
 (while (and (> n 0)
             (or (> dismal-current-col 0)
                 (plusp direction)))
   (setq dismal-current-col (+ dismal-current-col direction))
   (if (> (dismal-column-width dismal-current-col) 0)
       (setq n (1- n))) )) )

;  (cond
   ;((and (= dismal-current-col 0)  ; this doesn't work as nicely
   ;as I hoped...FER
   ;      (> dismal-current-row 0))
   ; (setq dismal-current-col dismal-max-col)
   ; (setq dismal-current-row (- dismal-current-row 1)))
;   (t (setq dismal-current-col (max 0 (- dismal-current-col cols)))))

(defun dis-first-column ()
  "Move to first column."
  (interactive)
  (setq dismal-current-col 0)
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dis-last-column ()
  "Move to last column."
  (interactive)
  (setq dismal-current-col dismal-max-col)
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dis-end-of-row ()
  "Move to last column with a value."
  (interactive)
  (setq dismal-current-col dismal-max-col)
  (while (and (>= dismal-current-col 1)
              (not (dismal-get-exp dismal-current-row dismal-current-col)))
     (setq dismal-current-col (1- dismal-current-col)))
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dis-previous-filled-row-cell (rows)
  "Move upward ROWS filled row cells (i.e., skip empty cells on the way up)."
  (interactive "p")
  (dis-next-filled-row-cell (- rows)))

(defun dis-next-filled-row-cell (rows)
  "Move down ROWS filled row cells (i.e., skip empty cells on the way down)."
  (interactive "p")
  (let ((old-row dismal-current-row)
        (old-col dismal-current-col)
        (direction (signp rows))
        (number (abs rows))  )
  (while (and (> number 0) (dismal-find-next-fill-row direction))
     (setq number (1- number)))
  (if (> number 0)
      (progn (setq dismal-current-row old-row)
             (setq dismal-current-col old-col)))
  (dismal-visit-cell dismal-current-row dismal-current-col)))

;; increment must be +/-1
(defun dismal-find-next-fill-row (increment)
 (let ((start-row dismal-current-row))
 ;; initial move
 (cond ((and (plusp increment) (= dismal-current-row dismal-max-row))
        (setq dismal-current-row 0))
       ((and (minusp increment) (= dismal-current-row 0))
        (setq dismal-current-row dismal-max-row))
       (t (setq dismal-current-row (+ increment dismal-current-row))))
 (while (and (not (dismal-get-exp dismal-current-row dismal-current-col))
             (not (= start-row dismal-current-row)))
    (cond ((and (plusp increment) (= dismal-current-row dismal-max-row))
           (beep) ;; leave this beep without a t
           (and dismal-interactive-p 
                (message "Wrapping around forwards...") (sit-for 1))
           (setq dismal-current-row 0))
          ((and (minusp increment) (= dismal-current-row 0))
           (beep) 
           (and dismal-interactive-p
                (message "Wrapping around backwards...") (sit-for 1))
           (setq dismal-current-row dismal-max-row))
          (t (setq dismal-current-row (+ increment dismal-current-row)))))
 (if (= start-row dismal-current-row)
     (error "No (other) filled cell to move to in this column."))
 (dismal-get-exp dismal-current-row dismal-current-col)))

(defun dis-backward-filled-column (cols)
  "Move backward COLS filled columns (i.e., skip empty columns)."
  (interactive "p")
  (dis-forward-filled-column (- cols)))

(defun dis-forward-filled-column (cols)
  "Move forward COLS filled columns (i.e., skip empty columns)."
  (interactive "p")
  (let ((old-row dismal-current-row)
        (old-col dismal-current-col)
        (direction (signp cols))
        (number (abs cols))  )
  (while (and (> number 0) (dismal-find-next-fill-column direction))
     (setq number (1- number)))
  (if (> number 0)
      (progn (setq dismal-current-row old-row)
             (setq dismal-current-col old-col)))
  (dismal-visit-cell dismal-current-row dismal-current-col)))

;; increment must be +/-1
(defun dismal-find-next-fill-column (increment)
 ;; initial move
 (cond ((or (and (minusp increment) (dismal-bobp))
            (and (plusp increment) (dismal-eobp)))
        nil)
       ((and (plusp increment) (= dismal-current-col dismal-max-col))
        (setq dismal-current-row (+ 1 dismal-current-row))
        (setq dismal-current-col 0))
       ((and (minusp increment) (= dismal-current-col 0))
        (setq dismal-current-row (+ -1 dismal-current-row))
        (setq dismal-current-col dismal-max-col))
       (t (setq dismal-current-col (+ increment dismal-current-col))))
 (while (and (not (and (dismal-get-exp dismal-current-row dismal-current-col)
                       (< 0 (col-format-width (dismal-get-column-format dismal-current-col)))))
             ;; not stuck at either end
             (not (or (and (minusp increment) (dismal-bobp))
                      (and (plusp increment) (dismal-eobp)))))
    (cond ((and (plusp increment) (= dismal-current-col dismal-max-col))
           (setq dismal-current-row (+ increment dismal-current-row))
           (setq dismal-current-col 0))
          ((and (minusp increment) (= dismal-current-col 0))
           (setq dismal-current-row (- dismal-current-row 1))
           (setq dismal-current-col dismal-max-col))
          (t (setq dismal-current-col (+ increment dismal-current-col)))))
 (dismal-get-exp dismal-current-row dismal-current-col))

(defun dis-start-of-col ()
  "Move to first row in current column."
  (interactive)
  (setq dismal-current-row 0)
  (dismal-goto-cell dismal-current-row dismal-current-col t)
  (dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun dis-end-of-col ()
  "Move to last row with a value in current column."
  (interactive)
  (dismal-end-of-col-non-interactive)
  (dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun dismal-end-of-col-non-interactive ()
  (setq dismal-current-row dismal-max-row)
  (while (and (not (dismal-get-exp dismal-current-row dismal-current-col))
              (> dismal-current-row 0))
     (setq dismal-current-row (1- dismal-current-row)))
  (dismal-goto-cell dismal-current-row dismal-current-col t))

(defun dis-backward-column (cols)
  "Move backward COLS columns."
  (interactive "p")
  (dismal-move-columns (- cols))
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dis-forward-row (rows)
  "Move down ROWS rows."
  (interactive "p")
  (dismal-move-rows rows)
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dis-backward-row (rows)
  "Move up ROWS rows."
  (interactive "p")
  (dismal-move-rows (- rows))
  (dismal-visit-cell dismal-current-row dismal-current-col))

;; moves over hidden rows (none currently)
(defun dismal-move-rows (arg)
 (let ((n (abs arg))
       (direction (signp arg)))
 (while (and (> n 0)
             (or (> dismal-current-row 0)
                 (plusp direction)))
   (setq dismal-current-row (+ dismal-current-row direction))
   (setq n (1- n))) ))

(defun dismal-visit-cell (row column)
  ;; Move cursor to ROW, COLUMN and display the contents of the cell
  ;; in the status line.
  (if (not (= dismal-auto-save-counter 0))
      (progn (setq dismal-auto-save-counter (1- dismal-auto-save-counter))
         (if (= dismal-auto-save-counter 1)
             (dismal-do-auto-save))))
  (dismal-goto-cell row column t)
  (if dismal-interactive-p
     (dismal-display-current-cell-expr row column)))


(defsubst dismal-goto-column (column)
  ;; Move the cursor to the last character of COLUMN.
  (let* ((col (dismal-get-column-position (1+ column)))
         (width (dismal-column-width column))
         (original-modified-p (buffer-modified-p))
         (chars-missing (- col (move-to-column col))))
    (if (> chars-missing 0)
        (progn 
          (insert-char ?\040 chars-missing)     ; Insert some blanks
          (end-of-line)
          (set-buffer-modified-p original-modified-p)))
    ;; this ins't quite right, but is pleasent n.t.l.
    ;; (setq aa (list 'last-col column 'cc (current-column)
    ;;               'hscroll (window-hscroll)
    ;; 'going2 (> (+ 3 (current-column)) (+ (window-hscroll) (window-width)))))
    (if (> (+ 3 (current-column)) (+ (window-hscroll) (window-width)))
        (scroll-left (- (current-column) -4
                        (+ (window-hscroll) (window-width))))
        (if (< (- (current-column) width dismal-first-printed-column)
               (window-hscroll))
            (scroll-right (+ width dismal-first-printed-column
                             (- (window-hscroll)
                                (current-column))))))
       ;; (set-window-hscroll) may also work
       ;; Set number columns WINDOW is scrolled from l. margin to NCOL.
    (backward-char 1) ))

(defun dismal-goto-cell (row column interactivep)
  ;; Move cursor to the end of the cell at ROW, COLUMN.
  ;; does not set dismal-current-row, etc.
  (dismal-goto-row row interactivep)
  (dismal-goto-column column))

;(setq spot (list  interactivep current-window-row row window-rows raw-offset))
;(if interactivep
;   (progn (message "%s crow %s torow %s wrows %s raw-offset %s"
;             interactivep current-window-row row window-rows raw-offset)
;          (sit-for 2)))


;;  the number of columns by which WINDOW is scrolled from left margin.

(defun dis-beginning-of-buffer ()
  "Move dismal point to the beginning of the buffer."
  (interactive)
  (dismal-jump-to-cell 0 0))

(defun dis-end-of-buffer ()
  "Move dismal point to the end of the buffer."
  (interactive)
  (dismal-jump-to-cell dismal-max-row dismal-max-col))

(defun dis-jump (&optional r c)
  "Jump to ROW, COLUMN and display the contents of the cell
in the status line."
  (interactive "P") ;"nRow to goto: \nnColumn to goto (0 is A): "
  (if (not r)
      (setq r (dismal-read-minibuffer "Row to goto: " nil dismal-current-row)))
                                      ;; (prin1-to-string dismal-current-row)
  (if (not c)
      (setq c (dismal-read-minibuffer "Column to goto: "  nil
                 (dismal-convert-number-to-colname dismal-current-col))))
  (cond ((numberp r))
        ((not r) (setq r dismal-current-row))
        ((not (numberp r))
         (setq r (dismal-convert-colname-to-number (prin1-to-string r)))))
  (cond ((numberp c))
        ((not c) (setq c dismal-current-row))
        ((and (not (numberp c)) (stringp c))
         (setq c (dismal-convert-colname-to-number c)))
        ((and (not (numberp c)) (not (stringp c)))
         (setq c (dismal-convert-colname-to-number (prin1-to-string c)))))
  (dismal-jump-to-cell r c))


;;;
;;;	IX.	Cell editing
;;;


;;;;;;;;;;;;;;;; copy-to-dismal.el ;;;;;;;;;;;;;;;;
;;;
;;; Robert J. Chassell    bob@gnu.ai.mit.edu
;;; 12 July 1992
;;;
;;; Change Log ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version 0.03 
;;; 12 July 1992 - Updated to work with Dismal version 0.62
;;;
;;; Version 0.02 
;;; 18 Nov 1991 - Handle both long and short text strings, such as
;;; month names. Handle blank lines.    
;;;
;;; Version 0.01
;;; 14 Nov 1991 - Initial.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dis-copy-column-separator " ")

;; does not keep point and mark clean in original buffer
;; does not keep point and mark clean in dismal buffer
(defun dis-copy-to-dismal (dismal-buffer-name beg end) 

"Copy column specified by point and mark to buffer DISMAL-BUFFER-NAME
starting at its current cell.  Point and mark must be within or at the
beginning of a column of text, delimited by blanks.  The column must
contain words without spaces or valid dismal numbers, which may
contain decimal points but not commas.  The variable 
dis-copy-column-seperator (default is space) is used to separate columns.

For example, in the following column, you would place point on 2 in
123 and mark on the 8 in 789 and only that middle column would be
copied to the spreadsheet.

Jan    123   555
Feb    456   666
Mar    789   777"

 (interactive "BDismal buffer name to copy to: \nr")
 (if (eq major-mode 'dismal-mode)
     (error "You must start in a non-dismal buffer.")
 (goto-char beg)
 ;; Record current column
 (save-excursion
 (let ((start-col (current-column))
       col match-start)
   ;; Go to beginning of column
   (if (re-search-backward dis-copy-column-separator
                       ;; "\\(^\\w\\|[ \t\n]+\\)"
                       (save-excursion (beginning-of-line) (point))
                       'just-move-to-beginning-on-fail)
        (forward-char 1))
   (setq col (current-column))
   (setq match-start (point))
   (while (< (point) end)
     (let ((item (progn
                      (if (re-search-forward dis-copy-column-separator
                                         (save-excursion (end-of-line) (point))
                                         'just-move-to-end-on-fail)
                          (forward-char -1))
                      (buffer-substring match-start (point)))))
       (save-excursion
          ;; dismal has to be in a window to readraw
          (switch-to-buffer (get-buffer dismal-buffer-name))
          (dis-edit-cell-plain item)
          (dis-forward-row 1))
        (forward-line 1)
        ;; Check if blank line
        (if (looking-at "[ \t]*$") (next-line 1))
        ;; Stay within the correct colum
        (move-to-column start-col)
        ;; Go to beginning of column
        (if (re-search-backward dis-copy-column-separator
                       ;; "\\(^\\w\\|[ \t\n]+\\)"
                       (save-excursion (beginning-of-line) (point))
                       'just-move-to-beginning-on-fail)
            (forward-char 1))
        (setq col (current-column))
        (setq match-start (point))))))))

;; 14.  Allow entering of numbers and strings not in quotes?
;;   Yes, you have to put quotation marks around a number it has a decimal
;;   point and nonzero digits after the decimal, e.g. "3.1416".  Try it.
;;   This is because 3.1416 is not a lisp object, it is three lisp
;;   objects, and dismal-edit-cell accepts its input as an s-expression.
;;   It may be possible to go through the input s-expression and find
;;   the dotted pairs of two numbers and convert them to floating point
;;   numbers (which are themselves dotted pairs of two numbers, but
;;   won't ever appear in the input.)  The function
;;   dismal-convert-cellexpr-to-string converts the cell's current value
;;   to a string so the use can edit its value.  If you change
;;   dismal-convert-input-to-cellexpr to handle numbers not in quotes,
;;   you must change dismal-convert-cellexpr-to-string so it doesn't
;;   put the numbers in quotes.
;; 

(defun dis-clear-cell (arg &optional kill-flag)
  "Clear the following ARG cells (previous, with negative arg).
Does not save cell contents in kill range buffer unless kill-flag is set.      
Interactively, ARG is the prefix arg, and KILLFLAG is set if
ARG was explicitly specified. dis-kill-cell saves."
  (interactive "P")
  (dismal-save-excursion
  ;; (message "got arg %s with %s" arg (interactive-p)) (sit-for 2)
  (if (and arg (interactive-p))
      (setq kill-flag t))
  (setq arg (prefix-numeric-value arg))
  (dismal-set-mark dismal-current-row dismal-current-col)
  (dis-forward-column (1- arg))
  (let ((dis-show-selected-ranges nil))
  (if kill-flag
     (dis-kill-range)
    (dis-erase-range)))))

(defun dis-kill-cell (arg)
  "Kill ARG cells backward."
  (interactive "p")
  (dismal-save-excursion
    (dismal-set-mark dismal-current-row dismal-current-col)
    (dis-backward-column (1- arg))
    (let ((dis-show-selected-ranges nil))
      (dis-kill-range))))

(defun dis-backward-kill-cell (arg)
  "Kill ARG cells backward."
  (interactive "p")
  (dismal-save-excursion
    (dismal-set-mark dismal-current-row dismal-current-col)
    (dis-backward-column (1- arg))
    (let ((dis-show-selected-ranges nil))
      (dis-kill-range)))
  (dis-backward-column 1))

(defun dis-edit-cell-center (sexp)
  "Read a cell value, convert it to internal format, and make that the
current cell's value."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression (center): " t
           (dismal-convert-cellexpr-to-string
              (dismal-get-exp dismal-current-row dismal-current-col)))))
  (dismal-edit-cell sexp 'center))

(defun dis-edit-cell-rightjust (sexp)
  "Read a right justified value into the current cell."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression (right): " t
           (dismal-convert-cellexpr-to-string
               (dismal-get-exp dismal-current-row dismal-current-col)))))
  (dismal-edit-cell sexp 'right))


(defun dis-edit-cell-default (sexp)
  "Read a default justified value into the current cell."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression (default): " t
           (dismal-convert-cellexpr-to-string
               (dismal-get-exp dismal-current-row dismal-current-col)))))
  (dismal-edit-cell sexp 'default))

(defun dis-edit-cell-leftjust (sexp)
  "Read a left justified value into the current cell."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression (left): " t
           (dismal-convert-cellexpr-to-string
               (dismal-get-exp dismal-current-row dismal-current-col)))))
  (dismal-edit-cell sexp 'left))

;; now redundant, other reads can handle strings.
;(defun dis-edit-cell-string (sexp)
;  "Read a left justified value into the current cell."
;  (interactive
;    (list
;  (let ((cell-exp (dismal-get-exp dismal-current-row dismal-current-col)))
;     (read-string "Enter string (no \"'s): "
;                     (if (stringp cell-exp) 
;                     cell-exp
;                     (dismal-convert-cellexpr-to-string cell-exp))))))
;  (dismal-edit-cell sexp (dismal-get-fmt dismal-current-row 
;                                         dismal-current-col)))

(defun dis-edit-cell-plain (sexp)
  "Read a cell value, convert it to internal format, and make that the
current cell's value."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression: " t
            (dismal-convert-cellexpr-to-string
                 (dismal-get-exp dismal-current-row dismal-current-col)))))
   (dismal-edit-cell sexp (dismal-get-fmt dismal-current-row
                                          dismal-current-col)))

(defun dismal-edit-cell (sexp alignment)
  ;;(dismal-save-excursion-quietly) used to be wrapped here
  ;; and save-excursion would not work on it's own?!
  (dismal-check-for-read-only)
  (dismal-save-excursion
  (let ((old-point (point))
        (old-val (dismal-get-exp dismal-current-row dismal-current-col))
        (dismal-interactive-p nil))
  ;; small optimization here, avoid doing what you know
  ;; strings and numbers will be the same, formula won't
  ;; (setq aa (list old-val sexp))
  (if (or (not (equal old-val sexp))
          (not (eq (dismal-get-cell-alignment dismal-current-row 
                                              dismal-current-col) alignment)))
     (dismal-set-cell dismal-current-row dismal-current-col
                      (dismal-convert-input-to-cellexpr sexp)
                      alignment))
  (if (and dis-auto-update
           (not (equal old-val sexp)))  ;; small optimization here
      (dismal-private-update-matrix) 
    (dismal-cleanup-long-string dismal-current-row dismal-current-col))
  (dismal-execute-delayed-commands)
  (dismal-display-current-cell-expr dismal-current-row dismal-current-col)
  (goto-char old-point)
  (dismal-hard-redraw-row-non-interactive))))

(defun dismal-execute-delayed-commands ()
  (while dismal-delayed-commands
    (eval (pop dismal-delayed-commands))))

; (dismal-read-minibuffer "gimme: " nil 34)
; (dismal-read-minibuffer "gimme: " t "34")
;   (dismal-convert-cellexpr-to-string (dismal-get-exp dismal-current-row dismal-current-col))

; (dismal-read-minibuffer "how are you" nil 'fine)
; (dismal-read-minibuffer "how are you" t "fine")

(defun dismal-read-minibuffer (prompt editable-default default)
 (if (not editable-default)
     (setq prompt (format "%s [%s]: " prompt default)))
 (if (not (stringp default)) (setq default (format "%s" default)))
 (let* ((minibuffer-local-map dismal-minibuffer-local-map)
        (dismal-buffer-using-minibuffer (current-buffer))
        (first-result (if editable-default
                          (read-string prompt default)
                          (read-string prompt))) )
  (cond ((string= "" first-result)
         (if editable-default
             nil
             default))
        ((formula-string-p first-result)
         (car (read-from-string first-result)))
        ((dismal-number-stringp first-result) 
         (car (read-from-string first-result)))
        (t first-result)  )))

;; This is called by most of the other functions on this page.

(defun dismal-set-cell (row column sexp format)
  ;; Assign cell at position ROW, COLUMN the expression SEXP.  FORMAT is the
  ;; function (or nil for default) to use to format the value for display.
  (if (> row dismal-max-row)
      (while (> row dismal-max-row)
        (setq dismal-max-row (1+ dismal-max-row))
        (dismal-set-first-printed-column)
        (dismal-draw-row-label dismal-max-row)))
  (if (> column dismal-max-col)
      (while (> column dismal-max-col)
        (setq dismal-max-col (1+ dismal-max-col))
        (dismal-draw-column-label dismal-max-col)))
  (dismal-set-cell-internals row column sexp format)
  (set-buffer-modified-p t))

;; in deleting a cell, remove dependencies?
(defun dismal-set-cell-internals (row column sexp format)
  (dismal-erase-dependencies row column (dismal-get-exp row column))
  (dismal-set-exp row column sexp)
  (if format (dismal-set-fmt row column format))
  ;; if cell could have a formula, add it to the formula list (else remove it)
  ;; and set mark that it is a non-formula cell
  (if (dismal-possible-live-sexp sexp)
      (progn
        (dismal-set-val row column nil)
        (dismal-record-dependencies row column sexp)
        (vector-push-unique dismal-formula-cells 
                            (dismal-make-address row column))
        ;; invalidate it if its a formula
        (dismal-invalidate-cell (dismal-make-address row column)))
    ;; if its not live, make sure its not on list
    (dismal-set-val row column sexp)
    (vector-remove dismal-formula-cells (cons row column))
    (dismal-set-mrk row column nil)
    ;; invalidate its references if its just a value
    (dismal-map-apply 'dismal-invalidate-cell (dismal-get-deps row column))))

(defun dis-capitalize-cell (arg)
  "Capitalize the current cell (or ARG cells), moving over if arg >1 (default).
This gives the cell(s) first character in upper case and the rest lower case."
  (interactive "p")
  (while (> arg 0)
    (dismal-save-excursion
    (let* ((cell-exp (dismal-get-exp dismal-current-row dismal-current-col))
           start)
    (if (stringp cell-exp)
        (progn (setq start (1+ (string-match "[^ ]" cell-exp) ))
          (dismal-set-exp dismal-current-row dismal-current-col
          (dismal-set-val dismal-current-row dismal-current-col
                          (concat (capitalize (substring cell-exp 0 start))
                                  (downcase (substring cell-exp start)))))))
    (dismal-redraw-cell dismal-current-row dismal-current-col t)))
    (if (>= arg 0) (dis-forward-column 1))
    (setq arg (1- arg))  ))

(defun dis-downcase-cell (arg)
  "Downcase the current cell (or ARG cells), moving over if arg >= 2.
This gives the cell(s) all lower case characters."
  (interactive "p")
  (while (> arg 0)
    ;; dismal-save-excursion
    (let ((cell-exp (dismal-get-exp dismal-current-row dismal-current-col)))
    (if (stringp cell-exp)
        (dismal-set-exp dismal-current-row dismal-current-col
        (dismal-set-val dismal-current-row dismal-current-col
                        (downcase cell-exp))))
    (dismal-redraw-cell dismal-current-row dismal-current-col t)
    ;; if you have dependencies, should update them here...
    (if (not (= arg 0)) (dis-forward-column (signp arg)))
    (setq arg (1- arg))    )))

(defun dis-upcase-cell (arg)
  "Upcase the current cell (or ARG cells), moving over if arg >= 1.
This gives the cell(s) characters all in upper case."
  (interactive "p")
  (while (> arg 0)
    (let ((cell-exp (dismal-get-exp dismal-current-row dismal-current-col)))
      (if (stringp cell-exp)
          (dismal-set-exp dismal-current-row dismal-current-col
          (dismal-set-val dismal-current-row dismal-current-col
                          (upcase cell-exp))))
    (dismal-redraw-cell dismal-current-row dismal-current-col t)
    ;; if you have dependencies, should update them here...
    (if (not (= arg 0)) (dis-forward-column (signp arg)))
    (setq arg (1- arg))    )))


;;;
;;;	X.	Cell re-evaluation 
;;;

(defun dis-update-matrix ()
  "Recalculate the dirty cells in the spreadsheet."
  (interactive)
  (dismal-save-excursion
  (if (not dis-auto-update)
      (message "Updating the matrix..."))
  (dismal-private-update-matrix)
  (message "Updating the matrix...Finished.")))

(defun dismal-private-update-matrix ()
  ;; actually recalculate the cells in the invalid heap
  (let ((temp nil)
        (i 1))
    (while (and (<= i dis-iteration-limit)
                (not (heap-empty dismal-invalid-heap)))
      (message "Starting to update cycle ... %d (%s cells)" i 
               (heap-last dismal-invalid-heap))
      (dismal-update-cycle)
      (setq i (1+ i))
      (setq temp dismal-invalid-heap)
      (setq dismal-invalid-heap dismal-invalid-heap-not)
      (setq dismal-invalid-heap-not temp))
    ;; check to see how long you did this...
    (if (not (heap-empty dismal-invalid-heap))
        (message "Update stopped due to exceeding max cycles of %s."
                 dis-iteration-limit)
        (message "Updated %s times." (1- i)) )    ))

;; (heap-aref dismal-invalid-heap 0)
(defun dismal-update-cycle ()
  (let ((prev nil))
  (while (not (heap-empty dismal-invalid-heap))
    (let* ((addr (heap-deletemin dismal-invalid-heap))
           (r (dismal-address-row addr))
           (c (dismal-address-col addr))
           (new-val nil)
           (old-val (dismal-get-val r c)))
      (if dis-show-update 
          (message "Starting with old-val of %s:%s of %s" r c old-val))
      (if (equal addr prev)
          nil
        (setq new-val (dismal-set-val r c (dismal-eval (dismal-get-exp r c))))
        ;;(message "updat'n %s, got [%s] had [%s] equal= %s" 
        ;;        addr new-val old-val (equal old-val new-val))  (sit-for 1)
        (if (not (equal old-val new-val))
            (let ((dismal-invalid-heap dismal-invalid-heap-not))
               (dismal-invalidate-cell addr)))
        (dismal-redraw-cell r c t))
      (setq prev addr)))))

(defun dismal-invalidate-cell (addr)
  ;; Mark the cell at ADDR invalid (if necessary) and (recursively) all cells 
  ;; that depend on it, by inserting their addresses into dismal-invalid-heap.
  (let ((r (dismal-address-row addr))
        (c (dismal-address-col addr)) )
  ;(message "invalidating %s %s" r c)
  ;; only invalidate cells that can be updated
  (if (vector-member dismal-formula-cells addr)
      (heap-insert dismal-invalid-heap addr)
     (dismal-redraw-cell r c t))
  ;;is this necessary? seems to lead to problems...
  ;(dismal-set-val row col nil)  
  (if (eq 'visited (dismal-get-mrk r c))
      ()
    (dismal-set-mrk r c 'visited)
    (dismal-map-apply 'dismal-invalidate-cell (dismal-get-deps r c))
    (dismal-set-mrk r c 0))))

(defun dis-recalculate-matrix ()
  "Recalculate and redraw the whole matrix."
  (interactive)
  ;; could use dismal-formula-cells if all are caught, which they aren't
  (matrix-map-rc 
     (function (lambda (cell dummy) 
     ;; (message "doing %s %s %s" cell 
     ;;          (dismal-get-exp (car cell) (cadr cell))
     ;;   (dismal-possible-live-sexp (dismal-get-exp (car cell) (cadr cell))))
     ;;            (sit-for 1)
                 (if (dismal-possible-live-sexp
                          (dismal-get-exp (car cell) (cadr cell)))
                     (heap-insert dismal-invalid-heap
                                  (cons (car cell) (cadr cell))))))
      dismal-matrix)
  (dis-update-matrix)
  (dis-redraw nil))


;;;
;;;	XI.	Cell evaluation
;;;

(defun dismal-evaluate-cell (row column)
  ;; Look for a current value for this cell at ROW, COLUMN.  If it has
  ;; none evaluate the associated expression and return the result.
  (if (or (< row 0) (< column 0))
      (error "Accessing an illegal pair: row %s column %s" row column))
  (let ((value (dismal-get-val row column)))
    (if (null value)
        (let ((sexp (dismal-get-exp row column)))
          (if (not sexp)
              ()
;; This is where code to check recursion depth should go.  Be careful,
;; because the -mrk field is used by the invalidation code, which sets
;; it to 'visited and then resets it to nil when it is done.
;           (let ((recursion-depth (dismal-get-mrk row column)))
;             (if (>= recursion-depth dis-recursion-limit)
;                 (message (concat "Recursion depth exceeded on "
;                                  (prin1-to-string (list row column))))
;               (dismal-set-mrk row column (1+ recursion-depth))
                (setq value (dismal-eval sexp))
;               (dismal-set-mrk row column (1- recursion-depth))))
            (dismal-set-val row column value))))
    value))

;; Note that these all do the same thing.  We distinguish between them
;; when we are inserting or deleting rows and columns.  At that time
;; we look at the content of the cell expressions and change the indices
;; depending on which of these functions is used.  See "CELL REFERENCES".
;; f stands for fixed.

(defsubst dismal-evaluate-cellref (addr)
  (let ((value (dismal-evaluate-cell (dismal-address-row addr)
                                     (dismal-address-col addr))))
    (or value 0)))

(defun dismal-r-c- (row column) (dismal-evaluate-cellref (cons row column)))
(defun dismal-rfc- (row column) (dismal-evaluate-cellref (cons row column)))
(defun dismal-r-cf (row column) (dismal-evaluate-cellref (cons row column)))
(defun dismal-rfcf (row column) (dismal-evaluate-cellref (cons row column)))


;;;
;;;	XIIa.	Insertion - of rows and columns.
;;;

;; insert the text for a new, blank column
(defsubst dismal-insert-blank-col (ncols)
 (dismal-insert-blank-range 0 dismal-current-col 
             (+ 1 dismal-max-row) ncols nil))

(defun dis-kill-line (arg)
  "Kill the rest of the current line;  [rest not implemented in dismal]
if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg."
  (interactive "P")
  (dismal-save-excursion
    (dis-set-mark)
    (if (not arg)
        (progn (dis-end-of-row)
               (dis-kill-range))
       (error "Can't do that yet in dis-kill-line."))))

(defun dis-insert-range (arg)
  "Insert arg rows or cols of cells.  If mark=point, insert a cell.
If range is along a row, insert blank cells, moving other cells down.
If range is along a column, insert blank cells, moving other cells left.
If range is 2d, signal an error."
  (interactive "p")
  (dismal-save-excursion
    (dismal-select-range)
    (dismal-show-selected-range)
    (let ((start-row (range-1st-row dismal-cell-buffer))
          (start-col (range-1st-col dismal-cell-buffer))
          (end-row (range-2nd-row dismal-cell-buffer))
          (end-col (range-2nd-col dismal-cell-buffer))  )
    (if (not (or (= start-row end-row) (= start-col end-col)))
        (error "Only row or column can vary in range, not both.")
      (dismal-insert-range-cells start-row start-col end-row end-col arg)))))



;;; EMA changes  ;;; EMA changes  ;;; EMA changes  ;;; EMA changes  
;;; EMA changes  ;;; EMA changes  ;;; EMA changes  ;;; EMA changes  

;;; 3-7-93 - EMA - the next four functions fix a problem in which
;;; inserting and deleting rows throws off formulas.
;;; dismal-formula-cells, which contains the addresses of formulas
;;; with flexible references, was getting updated haphazardly.  the
;;; same sort of fix might be needed for columns.  changes are marked
;;; with "EMA".

;; 3-7-93 - EMA - didn't remove the call to dismal-change-row-references.
;;   see dismal-insert-range-cells.
(defun dis-insert-row (nrow)
  "Insert NROW new rows, moving current row down."
  (interactive "p")
  (dismal-save-excursion
   (if dismal-interactive-p
       (message "Inserting %d row(s)..."
                nrow))
   ;; 3-7-93 - it seems necessary to do this first, rather
   ;; than in dismal-insert-range-cells:
   (dismal-change-row-references dismal-current-row nrow)
   (dismal-insert-range-cells dismal-current-row 0
                              dismal-current-row  dismal-max-col nrow)))

(defun dismal-insert-range-cells (start-row start-col end-row end-col arg)
  ;; We have to search the expressions for non-fixed column references
  ;; greater or equal to dismal-current-col and add ncol to them. 
  ;; (dismal-change-column-references start-row start-col end-row end-col arg)
  ;; (message "with %s %s %s %s" start-row start-col end-row end-col)
  ;; 3-7-93 - EMA - removed a call to dismal-change-column-references,
  ;;   because it duplicates the call in dis-insert-row
  (let ((dismal-interactive-p nil)
        (cols-to-insert (1+ (- end-col start-col)) ))
    (dismal-jump-to-cell-quietly start-row  start-col)
    (cond ;; Insert just at a single spot
     ((and (= start-row end-row) (= start-col end-col))
      (setq dismal-max-row (+ dismal-max-row arg))
      (dis-insert-cells arg (if (not (aref dismal-mark 0)) 'rows)))
     
        ;;; Special case: insert whole row
     ((and (= start-row end-row)
           (= start-col 0) (= end-col dismal-max-col))
      (setq dismal-max-row (+ dismal-max-row arg))
      (dismal-set-first-printed-column)
      (dismal-insert-blank-rows arg)
      (matrix-insert-nil-rows dismal-matrix start-row arg)
      (dismal-increment-ruler start-row arg)
      ;; 3-7-93 - EMA - this has already been done in dis-insert-row:
      ;;(dismal-change-row-references dismal-current-row arg)
      )
     
        ;;; Insert partial row moving cells down
     ((= start-row end-row)
      (setq dismal-max-row (+ dismal-max-row arg))
      (while (<= start-col end-col)
        (dismal-jump-to-cell-quietly start-row start-col)
        (dismal-insert-column-cells-logical arg)
        (dismal-insert-column-cells-graphical arg)
        (setq start-col (1+ start-col)))
      ;; matrix will grow ncols*arg, hold to 1*arg amount
      (matrix-delete-rows dismal-matrix 
                          (1+ dismal-max-row) ;0 centered reference
                          (- (matrix-height dismal-matrix)
                             (1+ dismal-max-row)))
      (dismal-add-row-labels-at-end arg))
     
        ;;; Special case: insert whole column
     ((and (= start-col end-col) 
           (= start-row 0) (= end-row dismal-max-row))
      (setq dismal-max-col (+ dismal-max-col arg))
      (matrix-insert-nil-cols dismal-matrix start-col arg)
      (dismal-insert-blank-col arg)
      (dismal-change-column-references start-col arg)
      (vector-insert dismal-column-formats start-col arg)
      (while (<= start-row end-row)
        (if (or (and (>= start-col 1) ;avoid looking too far left
                     (consp (dismal-get-mrk start-row 
                                            (1- start-col)))
                     (not (dismal-get-exp start-row 
                                          (1- start-col))))
                (and (consp (dismal-get-mrk start-row 
                                            (1+ start-col)))
                     (not (dismal-get-exp start-row 
                                          (1+ start-col)))))
            (dismal-redraw-row start-row t))
        ;; should cleanup in here
        (setq start-row (1+ start-row))) )
     
     ;; Insert partial col moving cells left
     ((= start-col end-col)
      (setq dismal-max-col (+ dismal-max-col arg))
      (matrix-insert-nil-column-cells dismal-matrix start-row start-col 
                                      arg)
      (vector-insert dismal-column-formats start-col arg)
      (while (<= start-row end-row)
        (dismal-jump-to-cell-quietly start-row start-col)
        (dismal-insert-row-cells arg)
        (setq start-row (1+ start-row)))         )
     (t  (error "Must choose a row or col to do range insertion, not both.")))
    (dismal-erase-all-dependencies)
    (dismal-record-all-dependencies)))

(defun dismal-change-row-references (minrow number)
  ;; This function changes in all cells any non-fixed row 
  ;; references at or beyond MINROW by NUMBER.
  ;; 3-7-93 - EMA - added ADDR in call to *-reference,
  ;;   dis-addrs-to-update for it to push ADDR onto, and a mapcar to
  ;;   update the ADDRs in dismal-formula-cells.
  ;; (setq aa (list minrow number dismal-max-row))
  (let (dis-addrs-to-update)
    (vector-mapl (function (lambda (addr)
                             (let* ((r (dismal-address-row addr))
                                    (c (dismal-address-col addr))
                                    (cell (dismal-get-cell r c)) )
                               (dismal-set-cell-exp cell
                                    (dismal-change-row-reference
                                                    (dismal-get-cell-exp cell)
                                                     minrow number addr)))))
                 dismal-formula-cells)
    (mapcar (function (lambda (old-new) ; have to watch for duplicates
                    (vector-remove dismal-formula-cells (car old-new))
                    (vector-push-unique dismal-formula-cells (cadr old-new))))
            dis-addrs-to-update)))

(defun dismal-change-row-reference (expr minrow number addr)
  ;; This function changes the non-fixed row reference in the cell
  ;; EXPR to a row at or beyond MINROW by NUMBER.
  ;; 3-7-93 - EMA - added ADDR (an element of dismal-formula-cells)
  ;;   as a parameter, and added a push.
  (if expr
      (if (listp expr)
          (if (or (eq (car expr) 'dismal-r-c-)
                  (eq (car expr) 'dismal-r-cf))
              (let* ((row (car (cdr expr)))
                     (new-row (if (>= row minrow) (max 0 (+ row number)) row))
                     (r (car addr))
                     (new-r (if (>= r minrow) (max 0 (+ r number)) r))
                     (col (car (cdr (cdr expr)))))
                ;; 3-7-93 - EMA - push the old addr and the new addr
                ;; onto an update list, as (OLD . NEW):
                ;; dis-addrs-to-update is a bound variable at this point
                (push (list addr (cons new-r (cdr addr))) dis-addrs-to-update)
                ;;(setcar addr new-r)  ; this could generate duplicates
                (list (car expr) new-row col)) ; return value
            (cons (dismal-change-row-reference (car expr) minrow number addr)
                  (dismal-change-row-reference (cdr expr) minrow number addr)))
        expr)))


;;; EMA changes  ;;; EMA changes  ;;; EMA changes  ;;; EMA changes  end
;;; EMA changes  ;;; EMA changes  ;;; EMA changes  ;;; EMA changes  end

;; old FER version
;; (defun dis-insert-row (nrow)
;;   "Insert NROW new rows, moving current row down."
;;   (interactive "p")
;;   (dismal-save-excursion
;;     (if dismal-interactive-p
;;         (message (if (= nrow 1) "Inserting %d row..." "Inserting %d rows...")
;;                  nrow))
;;     (dismal-change-row-references dismal-current-row nrow)
;;     (dismal-insert-range-cells dismal-current-row 0
;;                                dismal-current-row  dismal-max-col nrow)))


;; This function inserts the cells.  It is responsible for (a) updating 
;; max-col and row, (b) updating dependencies, (c) (not implemented yet)
;; updating row/col references

;; (dismal-insert-range-cells 0              dismal-current-col
;;                            dismal-max-row dismal-current-col 1)

;; (setq start-row 0)
;; (setq start-col dismal-current-col)
;; (setq end-row  dismal-max-row)
;; (setq end-col  dismal-current-col)

;; old FER version
;; (defun dismal-insert-range-cells (start-row start-col end-row end-col arg)
;;   ;; We have to search the expressions for non-fixed column references
;;   ;; greater or equal to dismal-current-col and add ncol to them. 
;;   ;; (dismal-change-column-references start-row start-col end-row end-col arg)
;;   ;; (message "with %s %s %s %s" start-row start-col end-row end-col)
;;   (let ((dismal-interactive-p nil)
;;         (cols-to-insert (1+ (- end-col start-col)) ))
;;   (dismal-jump-to-cell-quietly start-row  start-col)
;;   (cond ;; Insert just at a single spot
;;         ((and (= start-row end-row) (= start-col end-col))
;;          (setq dismal-max-row (+ dismal-max-row arg))
;;          (dis-insert-cells arg))
;; 
;;         ;;; Special case: insert whole row
;;         ((and (= start-row end-row)
;;               (= start-col 0) (= end-col dismal-max-col))
;;          (setq dismal-max-row (+ dismal-max-row arg))
;;          (dismal-set-first-printed-column)
;;          (dismal-insert-blank-rows arg)
;;          (matrix-insert-nil-rows dismal-matrix start-row arg)
;;          (dismal-increment-ruler start-row arg)
;;          (dismal-change-row-references dismal-current-row arg))
;; 
;;         ;;; Insert partial row moving cells down
;;         ((= start-row end-row)
;;          (setq dismal-max-row (+ dismal-max-row arg))
;;          (while (<= start-col end-col)
;;            (dismal-jump-to-cell-quietly start-row start-col)
;;            (dismal-insert-column-cells-logical arg)
;;            (dismal-insert-column-cells-graphical arg)
;;            (setq start-col (1+ start-col)))
;;          ;; matrix will grow ncols*arg, hold to 1*arg amount
;;          (matrix-delete-rows dismal-matrix 
;;                              (1+ dismal-max-row) ;0 centered reference
;;                              (- (matrix-height dismal-matrix)
;;                                 (1+ dismal-max-row)))
;;          (dismal-add-row-labels-at-end arg))
;; 
;;         ;;; Special case: insert whole column
;;         ((and (= start-col end-col) 
;;               (= start-row 0) (= end-row dismal-max-row))
;;          (setq dismal-max-col (+ dismal-max-col arg))
;;          (matrix-insert-nil-cols dismal-matrix start-col arg)
;;          (dismal-insert-blank-col arg)
;;          (dismal-change-column-references start-col arg)
;;          (vector-insert dismal-column-formats start-col arg)
;;          (while (<= start-row end-row)
;;           (if (or (and (>= start-col 1) ;avoid looking too far left
;;                        (consp (dismal-get-mrk start-row 
;;                                               (1- start-col)))
;;                        (not (dismal-get-exp start-row 
;;                                             (1- start-col))))
;;                   (and (consp (dismal-get-mrk start-row 
;;                                               (1+ start-col)))
;;                        (not (dismal-get-exp start-row 
;;                                             (1+ start-col)))))
;;               (dismal-redraw-row start-row t))
;;            ;; should cleanup in here
;;            (setq start-row (1+ start-row))) )
;; 
;;         ;; Insert partial col moving cells left
;;         ((= start-col end-col)
;;          (setq dismal-max-col (+ dismal-max-col arg))
;;          (matrix-insert-nil-column-cells dismal-matrix start-row start-col 
;;                                          arg)
;;          (vector-insert dismal-column-formats start-col arg)
;;          (while (<= start-row end-row)
;;            (dismal-jump-to-cell-quietly start-row start-col)
;;            (dismal-insert-row-cells arg)
;;            (setq start-row (1+ start-row)))         )
;;       (t  (error "Must choose a row or col to do range insertion, not both.")))
;;   (dismal-erase-all-dependencies)
;;   (dismal-record-all-dependencies)))
;; 

(defun dis-insert-column (ncol)
  "Insert NCOL new columns, moving current column to right."
  (interactive "p")
  (dismal-save-excursion
    (if dismal-interactive-p 
        (progn (message "Inserting %d column(s)..." ncol) (sit-for 1)))
    (dismal-insert-range-cells 0                  dismal-current-col
                               dismal-max-row dismal-current-col ncol)
    (dismal-draw-column-labels)
    (dismal-make-ruler)
    (dismal-draw-ruler dismal-current-row)
    (dismal-display-current-cell-expr dismal-current-row dismal-current-col)))


;; done after new max-row is set
(defun dismal-insert-blank-rows (nrow)
  ;; Insert NROW new rows in display, moving current row down.
  ;; (break)
  (dismal-save-excursion
    (beginning-of-line)
    (open-line nrow)
    ;; first cut the labels as a rectangle
    (let (start saved-labels end)
      (forward-line 1)
      (setq start (point))
      (forward-line (- (- dismal-max-row 1 nrow) dismal-current-row))
      (forward-char dismal-first-printed-column)
      (setq saved-labels (delete-extract-rectangle start (setq end (point))))
      ;; now paste them in
      (goto-char start)
      (forward-line (- nrow))
      (insert-rectangle saved-labels)
      ;; Insert leading spaces, we don't insert whole rows at this time
      ;; I'm not sure if this works or how to test it.  If it does, 
      ;; it could be improved by either not inserting the spaces, or 
      ;; by inserting them faster
      (dismal-insert-blank-box end nrow dismal-first-printed-column ? )
      ;; insert new lower labels here
      (dismal-add-row-labels-at-end (- nrow 1))      )))

(defun dis-insert-cells (arg &optional direction)
  "Insert some new cells into the spreadsheet."
  (interactive "p")
  (dismal-save-excursion
  (if (not direction)
      (setq direction (run-menu 'dismal-row-or-column-menu
                                 (dismal-range-is-rows-or-columns))))
  (if (eq direction 'rows)
      (progn (dismal-insert-column-cells-logical arg)
             (dismal-insert-column-cells-graphical arg)
             (dismal-add-row-labels-at-end arg))
    (dismal-insert-row-cells arg)  ))
  (dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun dismal-insert-column-cells-logical (nrow)
  (matrix-insert-nil-column-cells dismal-matrix
        dismal-current-row dismal-current-col nrow))

(defun dismal-insert-column-cells-graphical (nrow)
  (dismal-save-excursion-quietly
  (let (cut-start saved-rect cc max-real-row)
    (forward-char (- 1 (dismal-column-width dismal-current-col)))
    (setq cut-start (point))
    (dismal-end-of-col-non-interactive)
    (setq max-real-row dismal-current-row)
    (if (not (> (point) cut-start))
        nil
      (forward-char 1)
      (setq saved-rect (delete-extract-rectangle cut-start (point)))
      (goto-char cut-start)
      (setq cc (current-column))
      (dismal-insert-blank-box (point) nrow
          (dismal-column-width dismal-current-col) ? )
      (forward-line nrow)
      (move-to-column cc)
      (insert-rectangle saved-rect)
      (goto-char cut-start))
  (dismal-set-first-printed-column) )))

;; this does not increment dismal-max-col or the dismal-column-formats, 
;; the caller must do so, because there may be many calls in a block
(defun dismal-insert-row-cells (ncol)
  (dismal-save-excursion-quietly
  (let ((old-mrk (dismal-get-mrk dismal-current-row dismal-current-col)))
    (matrix-insert-nil-row-cells dismal-matrix
          dismal-current-row dismal-current-col ncol)
    ;(dismal-erase-all-dependencies)  ;(dismal-record-all-dependencies)
    ;; insert some space
    (dismal-insert-blank-range dismal-current-row  dismal-current-col
             1 ncol nil)
    ;; cleanup the cell the dirtied you up
    (if (and (consp old-mrk) old-mrk)
        (dismal-cleanup-long-string (car old-mrk) (cdr old-mrk)))    )))

(defun dismal-cleanup-long-string (row col)
 ;; cleanup the mrks
 ;(my-short-message "cleaning up a long string")
 (let ((alignment (dismal-get-cell-alignment row col)))
   (cond ((eq  'right alignment)
          (dismal-cleanup-mrks row col -1))
         ((or (eq  'left alignment)
              (eq  'default alignment))
          (dismal-cleanup-mrks row col 1))
         ((eq  'center alignment)
          (dismal-cleanup-mrks row col -1)
          (dismal-cleanup-mrks row col 1)))
 ;; redraw
 (dismal-redraw-cell row col t)))

(defun dismal-cleanup-mrks (row col increment)
 (let ( (old-col col)
        (neighbor-mrk nil) (done nil) )
 (setq col (+ increment col))
 (while (and (not done) (>= col 0) (<= col dismal-max-col))
   (setq neighbor-mrk (dismal-get-mrk row col))
   ;(-message "cleaning up in %s %s" row col)
   (if (and neighbor-mrk
            (consp neighbor-mrk)
            (= (car neighbor-mrk) row)
            (= (cdr neighbor-mrk) old-col))
       (progn (dismal-set-mrk row col nil)
              (dismal-redraw-cell row col t))
     (setq done t))
   (setq col (+ increment col))  )))

(defun dis-open-line (arg)
  "Insert a new row and leave point before it.
With arg, inserts that many newlines."
  (interactive "p")
  (let ((dismal-interactive-p nil))
    (dis-forward-row 1))
  (dis-insert-row arg) )



;;(defun dismal-insert-blank-range (start-row start-col rows cols compute-width)
;;  ;; compute-width means insert blanks based on the actual col width
;;  (let ((i 0))
;;   (dismal-goto-cell start-row start-col nil)
;;   (while (< i rows)
;;     (forward-char (- 1 (dismal-column-width start-col)))
;;     (dismal-insert-n-times " "
;;        (if compute-width
;;            (dismal-sum-column-widths start-col cols)
;;          (* cols dis-default-column-width)))
;;     (setq i (1+ i))
;;     (dismal-goto-cell (+ i start-row) start-col t))))

;; if this works, take let out
(defun dismal-insert-blank-range (start-row start-col rows cols compute-width)
  ;; compute-width means insert blanks based on the actual col widths
  (dismal-goto-cell start-row start-col nil)
  (string-rectangle (- (point) (dismal-column-width start-col))
                    (save-excursion (dismal-goto-cell (+ -1 rows start-row)
                                                      start-col t)
                                    ;; (beep) (sit-for 2)
                                    (point))
                    (make-string (if compute-width
                                     (dismal-sum-column-widths start-col cols)
                                   (* cols dis-default-column-width))
                                 32)))
;; space is 32

;;(let ((i 0))
;; (make-string 2 32)
;;   (while (< i rows)
;;     (forward-char (- 1 (dismal-column-width start-col)))
;;     (dismal-insert-n-times " "
;;        (if compute-width
;;            (dismal-sum-column-widths start-col cols)
;;          (* cols dis-default-column-width)))
;;     (setq i (1+ i))
;;     (dismal-goto-cell (+ i start-row) start-col t))


;;;
;;;	XIIb.	Deletion - of rows, columns & ranges
;;;

(defun dismal-erase-buffer ()
  ;; Delete the entire contents of the current dismal-buffer.
  ;; values are not saved, but matrix shell is.
  (dis-beginning-of-buffer)
  (let ((row 0) (dismal-interactive-p nil))
    (while (<= row dismal-max-row)
      (dis-end-of-row)
      (dis-clear-cell (- (1+ dismal-current-col)) nil)
      (dis-forward-row 1)
      (setq row (1+ row))    )  ))

(defun dis-delete-blank-rows (start-row end-row)
 "Delete any blank rows from START-ROW to END-ROW."
 (interactive 
    (list (dismal-read-minibuffer "Delete blank rows starting at: " t
                   (format "%s" (min dismal-current-row (dismal-mark-row))))
          (dismal-read-minibuffer "Delete blank rows ending with: " t
                 (format "%s" (max dismal-current-row (dismal-mark-row))))))
 (setq start-row (max start-row 0)) ; a guard
 (setq end-row (min end-row dismal-max-row)) ; a guard
 (dismal-save-excursion
 (while (> end-row start-row)
   (and dismal-interactive-p
        (message "Deleting blank rows (looking at %s on the way to %s)..."
                 end-row start-row))
   (let ((previous-interactive-p dismal-interactive-p)
         (dismal-interactive-p nil)
         block-start looking-for-block-end)
     ;; find next blank row
     (dismal-goto-row end-row nil)
     (setq dismal-current-row end-row)
     (dis-end-of-row)
     (if (dismal-get-exp dismal-current-row dismal-current-col)
         nil
       (setq block-start end-row)
       (setq looking-for-block-end t)
       ;; find how far it goes back
       (while (and (>= end-row start-row) looking-for-block-end
                   (>= end-row 1))
         (setq end-row (1- end-row))
         (setq dismal-current-row end-row)
         (message "Deleting blank rows (blank at %s on the way to %s)..."
                  end-row start-row)
         (dis-end-of-row)
         (if (dismal-get-exp dismal-current-row dismal-current-col)
             (setq looking-for-block-end nil)))
       (setq end-row (1+ end-row))
       ;; go there
       (dismal-goto-row end-row nil)
       (setq dismal-current-row end-row)
       ;; delete row(s)
       (and previous-interactive-p
            (message "Deleting block of %s blank row(s) starting at row %s..."
                      (1+ (- block-start end-row)) end-row))
       (dis-delete-row (1+ (- block-start end-row))))
   (setq end-row (1- end-row))))
  (and dismal-interactive-p 
       (message "Deleting blank rows %s down to %s...Done" end-row start-row))))

(defun dis-delete-range (direction)
  "Delete a the current range of cells.  If mark=point, delete just a cell.
If direction is rows, move cells up to fill.
If direction is columns, move cells left to fill."
  (interactive (list (run-menu 'dismal-row-or-column-menu
                               (dismal-range-is-rows-or-columns))))
  (dismal-save-excursion
  (dismal-select-range)
  (dismal-note-selected-range "Deleting %s%s:%s%d...")
  (let ((start-row (range-1st-row dismal-cell-buffer))
        (start-col (range-1st-col dismal-cell-buffer))
        (end-row (range-2nd-row dismal-cell-buffer))
        (end-col (range-2nd-col dismal-cell-buffer))
        (dismal-interactive-p nil)
        (dis-show-selected-ranges nil))
  (dismal-delete-range-cells start-row start-col end-row end-col direction))))

;(dismal-delete-range-cells dismal-current-row 0
;                       (+ (1- nrow) dismal-current-row) dismal-max-col 'rows)

(defun dismal-delete-range-cells (start-row start-col 
                                  end-row end-col direction)
  (cond ;; special case: delete whole row
        ((and (= start-col 0) (= end-col dismal-max-col))
         (dismal-delete-column-cells start-row start-col end-row end-col)
         (dismal-increment-ruler start-row (- (1+ (- end-row start-row)))))

        ;; remove cells moving up
        ((eq direction 'rows) 
         (dismal-delete-column-cells start-row start-col end-row end-col))

        ;; special case: delete whole column
        ;; no duplication with dismal-delete-column, this is delete/clearing
        ;; not killing function
        ((and (= start-row 0) (= end-row dismal-max-row))
         (while (<= start-row end-row)
           (dismal-jump-to-cell-quietly start-row start-col)
           (dismal-delete-row-cells (1+ (- end-col start-col)))
           (setq start-row (1+ start-row))))

        ((eq direction 'columns)  ;; remove cells moving left
         (while (<= start-row end-row)
           (dismal-jump-to-cell-quietly start-row start-col)
           (dismal-delete-row-cells (1+ (- end-col start-col)))
           (setq start-row (1+ start-row)))   )
        (t  (error "Must choose row or col to do range insertion, not %s."
                    direction)))
  ;; have to have cell references update here too...
  (dismal-erase-all-dependencies)
  (dismal-record-all-dependencies))

(defun dismal-delete-column-cells (start-row start-col end-row end-col)
  (dismal-save-excursion
  (let (cut-start cut2-start saved-rect cc i (nrow (- end-row start-row)))
  ;; delete the dead rectangle; move up the live; put filler in.
    (dismal-jump-to-cell-quietly start-row start-col)
    (forward-char (- 1 (dismal-column-width dismal-current-col)))
    (setq cut-start (point))
    (setq cc (current-column))
    (dismal-jump-to-cell-quietly end-row end-col)
    (forward-char 1)
    (delete-extract-rectangle cut-start (point))
    (dismal-jump-to-cell-quietly end-row start-col)
    (dis-forward-row 1)
    (forward-char (- 1 (dismal-column-width dismal-current-col)))
    (setq cut2-start (point))
    (dismal-visit-cell dismal-max-row end-col)
    (forward-char 1) 
    (setq saved-rect (delete-extract-rectangle cut2-start (point)))
    (goto-char cut-start)
    (insert-rectangle saved-rect)
    (forward-line 1)    (move-to-column cc)
    (dismal-insert-blank-box (point) (- dismal-max-row end-row)
          (dismal-column-width dismal-current-col) 32)
  ;; cleanup the matrix
  (setq i 0)
  (if (and (= start-col 0) (= end-col dismal-max-col))
      (matrix-delete-rows dismal-matrix start-row (1+ nrow))
    (while (<= (+ start-col i) end-col)
      (matrix-delete-column-cells dismal-matrix
            start-row (+ i start-col) (1+ nrow))
      (setq i (1+ i)))
    (matrix-funcall-rc
          (function (lambda (r c dummy) (dismal-cleanup-long-string r c)))
           start-row (max 0 (1- start-col))
           end-row (min dismal-max-col (1+ end-col)) dismal-matrix)))))

(defun dismal-delete-row-cells (ncol)
  (dismal-save-excursion
    (let (cut-start saved-rect cc)
    (matrix-delete-row-cells dismal-matrix
          dismal-current-row dismal-current-col ncol)
    (dismal-redraw-row dismal-current-row t)
    (dismal-display-current-cell-expr dismal-current-row dismal-current-col))))

;; doesn't redraw the changed cells if any
(defun dis-delete-column (ncol)
  "Delete NCOL columns starting with the current column and moving right."
  (interactive "p")
  (let (del-start
        (dismal-interactive-p nil))
    (dismal-save-excursion
    (if (> (+ (1- ncol) dismal-current-col) dismal-max-col) ;you want to cut too much
        (progn (setq ncol (1+ (- dismal-max-col dismal-current-col)))
               (message (if (= ncol 1) "Can only delete %d column..."
                                       "Can only delete %d columns...")
                         ncol))
      (message "Deleting %d column(s)..."
               ncol))
    (dismal-change-column-references dismal-current-col (- ncol))
    (matrix-delete-cols dismal-matrix dismal-current-col ncol)
    (setq dismal-max-col (- dismal-max-col ncol))
    (set-buffer-modified-p t)
      (dismal-goto-cell -2 dismal-current-col nil)
      (forward-char (- 1 (dismal-column-width dismal-current-col)))
      (setq del-start (point))
      (dismal-goto-cell dismal-max-row (+ (1- ncol)
                                              dismal-current-col) nil)
      (forward-char 1)
      (kill-rectangle del-start (point))
    (vector-delete dismal-column-formats dismal-current-col ncol)
    (dismal-draw-column-labels)
    (dismal-make-ruler)
    (dismal-draw-ruler dismal-current-row)))
    (message "Deleting %d column(s)...finished." ncol)
    (dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun dis-delete-row (nrow)
  "Delete NROW rows, moving remaining rows up."
  (interactive "p")
  (if dismal-interactive-p (message "Deleting %d row(s)..." nrow))
  (dismal-save-excursion
    (let ((dismal-interactive-p nil))
    ;; don't delete more rows than you have
    (if (> dismal-current-row 0)
        (if (> (+ dismal-current-row nrow -1) dismal-max-row)
            (setq nrow (- dismal-max-row dismal-current-row)))
      (if (> (+ dismal-current-row nrow) dismal-max-row)
          (setq nrow (- dismal-max-row dismal-current-row))) )
      ;; (my-message "Delete-row: done with endtest.") ;1
    (dismal-change-row-references dismal-current-row (- nrow))
      ;; (my-message "Delete-row: done with  change-row-references.") ;6
    (dismal-delete-range-cells dismal-current-row 0
                       (+ (1- nrow) dismal-current-row) dismal-max-col 'rows)
      ;; (my-message "Delete-row: done with  delete-range-cells.") ;20
    (dismal-remove-row-labels-at-end nrow)
      ;; (my-message "Delete-row: done with  remove-row-labels-at-end.") ;22
    (setq dismal-max-row (max 0 (- dismal-max-row nrow)))
    (dismal-set-first-printed-column))))

; (dismal-delete-range-cells dismal-current-row 0 dismal-current-row dismal-max-col 'rows)

;;;
;;;	XIIb.	Insertion and Deletion - Cell reference updating
;;;
;;;  When rows or columns are inserted or deleted the cell references 
;;;  must be changed so non-fixed references still refer to the same cell 
;;;  in its new location.

;; (dismal-change-indices
;;    (dismal-convert-input-to-cellexpr "(dis-sum e13:k13)")  -1 -11)

(defun dismal-change-indices (expr numrow numcol)
  ;; Return a version of EXPR moved by NUMROW rows and NUMCOL columns.
  ;; (setq aa (list expr numrow numcol))
  (dismal-change-column-reference 
     (dismal-change-row-reference-expr expr 0 numrow) ; 0 is minrow
     0 numcol))

;; this is where relative and absolute cell references get changed
;; -- or not.
(defun dismal-change-column-references (mincol number)
  ;; This function changes any non-fixed column references in the cell
  ;; matrix to columns at or beyond MINCOL by NUMBER.
  (matrix-mapl
   (function (lambda (cell)
      (dismal-set-cell-exp cell
                           (dismal-change-column-reference
                              (dismal-get-cell-exp cell) 
                               mincol number))))
   dismal-matrix))

;; old FER version
;; (defun dismal-change-row-references (minrow number)
;;   ;; This function changes in all cells any non-fixed row 
;;   ;; references at or beyond MINROW by NUMBER.
;;   (vector-mapl (function (lambda (addr)
;;                    (let* ((r (dismal-address-row addr))
;;                           (c (dismal-address-col addr))
;;                           (cell (dismal-get-cell r c)) )
;;                       (dismal-set-cell-exp cell
;;                             (dismal-change-row-reference
;;                                 (dismal-get-cell-exp cell)
;;                                 minrow number)))))
;;                dismal-formula-cells))
;;   (matrix-mapl
;;    '(lambda (cell)
;;       (dismal-set-cell-exp cell
;;                            (dismal-change-row-reference
;;                                (dismal-get-cell-exp cell)
;;                                minrow number)))
;;   dismal-matrix)

;; (dismal-change-column-reference
;;  '(dis-sum (quote (dismal-range (dismal-r-c- 13 4) (dismal-r-c- 13 10))))
;;   -1 -11)


(defun dismal-change-column-reference (expr mincol number)
  ;; This function changes the non-fixed column reference in the cell
  ;; EXPR to a column at or beyond MINCOL by NUMBER.
  (if (null expr)
      ()
    (if (listp expr)
        (if (or (eq (car expr) 'dismal-r-c-)
                (eq (car expr) 'dismal-rfc-))
            (let ((col (car (cdr (cdr expr)))))
              (list (car expr)
                    (car (cdr expr))
                    (if (>= col mincol) (max 0 (+ col number)) col)))
          (cons (dismal-change-column-reference (car expr) mincol number)
                (dismal-change-column-reference (cdr expr) mincol number)))
      expr)))

;; old FER version
;; (defun dismal-change-row-reference (expr minrow number)
;;   ;; This function changes the non-fixed row reference in the cell
;;   ;; EXPR to a row at or beyond MINROW by NUMBER.
;;   (if expr
;;      (if (listp expr)
;;          (if (or (eq (car expr) 'dismal-r-c-)
;;                  (eq (car expr) 'dismal-r-cf))
;;              (let ((row (car (cdr expr))))
;;                (list (car expr)
;;                      (if (>= row minrow) (max 0 (+ row number)) row)
;;                      (car (cdr (cdr expr)))))
;;            (cons (dismal-change-row-reference (car expr) minrow number)
;;                  (dismal-change-row-reference (cdr expr) minrow number)))
;;           expr)))

;; (dismal-change-row-reference-expr
;;  '(dis-sum (quote (dismal-range (dismal-r-c- 9 4) (dismal-r-c- 9 10))))
;;   0 -1)

(defun dismal-change-row-reference-expr (expr minrow number)
  ;; This function changes the non-fixed row references in the EXPR 
  ;; to a row at or beyond MINROW by NUMBER.
  (if expr
     (if (listp expr)
         (if (or (eq (car expr) 'dismal-r-c-)
                 (eq (car expr) 'dismal-r-cf))
             (let ((row (car (cdr expr))))
               (list (car expr)
                     (if (>= row minrow) (max 0 (+ row number)) row)
                     (car (cdr (cdr expr)))))
           (cons (dismal-change-row-reference-expr (car expr) minrow number)
                 (dismal-change-row-reference-expr (cdr expr) minrow number)))
          expr)))


;;;
;;;	XIII.	Cell dependencies
;;;

;; this will not catch references to internal data structures or 
;; internal functions 5-Jan-97 -FER
;; also cleaned up like erase-dependencies....2-Mar-92 -FER
(defun dismal-record-dependencies (row col sexp)
  ;; Inform cells (by recording the fact in their dep field) that ROW COL 
  ;; holds a SEXP that refers to them, so that if they change the cell in 
  ;; ROW COL can be recalculated.  If sexp ends up referencing a cell, 
  ;; then put it on the dismal-formula-cells vector.
  (if (not (dismal-possible-live-sexp sexp)) ; can be called recursively
      ()                                    ; so test each time
    (let ((depaddr (dismal-make-address row col)))
    (if (rangep sexp)
        (progn
          ;; (vector-push-unique dismal-formula-cells depaddr)
          (dismal-do (function (lambda (row2 col2 dummy)
                         (dismal-set-deps row2 col2
                                          (cons depaddr
                                                (dismal-get-deps row2 col2)))))
                      sexp nil))
      (if (dismal-cellp sexp)
          (let ((drow (dis-cell-row sexp)) 
                (dcol (dis-cell-col sexp)))
            ;; (vector-push-unique dismal-formula-cells depaddr)
            (dismal-set-deps drow dcol
                   (cons depaddr (dismal-get-deps drow dcol))))
        ;; else recurse
        (dismal-record-dependencies row col (car sexp))
        (dismal-record-dependencies row col (cdr sexp)))))))


(defun dismal-erase-dependencies (row col sexp)
  ;; Remove any dependencies implied by the cell at DEPADDR whose
  ;; definition is SEXP.
  (if (not (dismal-possible-live-sexp sexp))
      (vector-remove dismal-formula-cells (cons row col))
    (if (listp sexp)
        (if (eq (car sexp) 'dismal-range)
            (dismal-do (function (lambda (row2 col2 dummy)
                            (dismal-set-deps row2 col2
                                 ;; used to be dismal-del
                                 (delete (dismal-make-address row col)
                                         (dismal-get-deps row2 col2)))))
                        sexp nil)
          (if (memq (car sexp) dismal-cell-types)
              (dismal-set-deps row col 
                               ;; used to be dismal-del
                               (delete (dismal-make-address row col)
                                           (dismal-get-deps row col)))
            (dismal-erase-dependencies row col (car sexp))
            (dismal-erase-dependencies row col (cdr sexp)))))))

;; 2-Mar-92 -FER slightly cluncky version
; (defun dismal-erase-dependencies (depaddr sexp)
;   "Remove any dependencies implied by the cell at DEPADDR whose
; definition is SEXP."
;   (if (null sexp)
;       ()
;     (if (listp sexp)
;         (if (eq (car sexp) 'dismal-range)
;             (let ((cells (dismal-generate-range (nth 1 (nth 1 sexp))
;                                                      (nth 1 (nth 2 sexp)))))
;               (dismal-map-apply '(lambda (addr)
;                        (dismal-set-deps addr
;                                         (dismal-del depaddr
;                                             (dismal-get-deps addr)))) cells))
;           (if (memq (car sexp) '(dismal-r-c- dismal-rfc-
;                                              dismal-r-cf dismal-rfcf))
;               (let ((addr (list (nth 1 sexp) (nth 2 sexp))))
;           (dismal-set-deps addr (dismal-del depaddr (dismal-get-deps addr))))
;             (dismal-erase-dependencies depaddr (car sexp))
;            (dismal-erase-dependencies depaddr (cdr sexp)))))))

(defun dismal-erase-all-dependencies ()
  ;(message "In dismal-erase-all-dependencies")
  (matrix-mapl (function (lambda (cell) (dismal-set-cell-dep cell nil))) 
               dismal-matrix)) 

(defun dismal-record-all-dependencies ()
  ;(message "In dismal-record-all-dependencies")
  ;; use vector-mapl across already identified cells
  ;; insertion has to note that a cell has a function
  (vector-mapl (function (lambda (addr)
                   (let ((r (dismal-address-row addr))
                         (c (dismal-address-col addr)))
                     (dismal-record-dependencies r c (dismal-get-exp r c)))))
               dismal-formula-cells))


;;;
;;;	XIVa.	File I/O - reading and writing
;;;

(defvar dismal-saving-file nil "set to t when saving a file")

;; this version uses the main buffer to save with, but it doesn't appear
;; to keep write permissions consistent.  but this may be unix.
(defun dismal-write-buffer (filename)
  ;; Save the current spreadsheet in file FILENAME.
  (dismal-save-excursion 
  (setq dismal-saving-file t)
  (let ((real-buffer (current-buffer))
        (save-compression dismal-save-compression)
        (backup-file-name (concat filename "~"))
        (require-final-newline nil) )
  ;; (if (file-exists-p filename) (rename-file filename backup-file-name t))

  ;; Save your image
  (set-buffer (get-buffer-create "*Dismal-saving-buffer*"))
  (erase-buffer)
  (buffer-disable-undo (current-buffer))  ;; used to be: buffer-flush-undo
  (insert-buffer real-buffer)
  ;; Now insert the real stuff in the buffer that you need to save
  (set-buffer real-buffer)
  (erase-buffer)
  (dismal-file-header mode-name)
  (mapc (function (lambda (x)
           (let ((real-x (save-excursion (set-buffer real-buffer)
                                         (eval x))))
           (insert "(setq " (prin1-to-string x) " "
                   (prin1-to-string real-x) ")\n"))))
        dismal-saved-variables)
  (if (interactive-p) (message "Dismal saving %s ~20%% finished." filename))
  (if dismal-save-image               ;DBL
      (progn
        (insert "\n;image\n")
        (insert-buffer "*Dismal-saving-buffer*")
       (while (re-search-forward "^.*$" nil t)
           (replace-match ";\\&" nil nil))         ))
  (if (interactive-p) (message "Dismal saving %s ~70%% finished." filename))
  (if save-compression
     (dismal-compress-region
         (save-excursion (goto-char 0)
             (search-forward "setq dismal-matrix") (point))
         (point-max) nil))
  (if (interactive-p) (message "Dismal saving %s ~90%% finished." filename))
  (message "Dismal saving %s ~90%% finished." filename)
  ;; pays attention to make-backup-files, 28-May-97 -FER
  (if make-backup-files
     (save-buffer)
     (save-buffer 0))
  (erase-buffer)
  (insert-buffer "*Dismal-saving-buffer*")

  (setq dismal-auto-save-counter dis-auto-save-interval)
  (kill-buffer "*Dismal-saving-buffer*")
  (set-buffer-modified-p nil)
  (clear-visited-file-modtime)
  (setq dismal-saving-file nil))))


;; commands used to compress files
(defvar dismal-compress-command "compress")
(defvar dismal-uncompress-command "compress -d")

(defconst compress-magic-regexp "\037\235\220"   ;; may need to delete \220
  "Regexp that matches the magic number at the beginning of files created
by the compress(1) command.")

;; stolen from crypt.el
(defun dismal-compress-region (start end &optional undo)
  "Compress the text in the region.
From a program, this function takes three args: START, END and UNDO.
When called interactively START and END default to point and mark
\(START being the lesser of the two).
Prefix arg (or optional second arg non-nil) UNDO means uncompress."
  (interactive "*r\nP")
  ;; (setq aa (cons start end))
  (save-point
   (call-process-region start end shell-file-name t t nil "-c"
	(if undo dismal-uncompress-command dismal-compress-command))
   (cond ((not undo)
	  (goto-char start)
	  (let (case-fold-search)
	    (if (not (looking-at compress-magic-regexp))
		(error "%s failed!" (if undo
					"Uncompression"
				      "Compression"))))))))

(defmacro save-point (&rest body)
  "Save value of point, evalutes FORMS and restore value of point.
If the saved value of point is no longer valid go to (point-max).
This macro exists because, save-excursion loses track of point during
some types of deletions."
  (let ((var (make-symbol "saved-point")))
    (list 'let (list (list var '(point)))
	  (list 'unwind-protect
		(cons 'progn body)
		(list 'goto-char var)))))

;; pre-DBL version 8-17-94 - FER
;; (defun dismal-write-buffer (filename)
;;   ;; Save the current spreadsheet in file FILENAME.
;;   ;; most writing functions call this
;;   (save-excursion
;;     (let ((real-buffer (current-buffer))
;;           (mode-name-to-write mode-name) ;; might be spa or such
;;           (backup-file-name (concat filename "~"))
;;           (require-final-newline nil) )
;;     (if (file-exists-p filename) (rename-file filename backup-file-name t))
;;     (set-buffer (get-buffer-create "*Dismal-saving-buffer*"))
;;     (erase-buffer)
;;     (buffer-flush-undo (current-buffer))
;;     (dismal-file-header mode-name-to-write)
;;     (insert "\n")
;;     (mapc (function (lambda (x)
;;              (let ((real-x (save-excursion (set-buffer real-buffer)
;;                                            (eval x))))
;;              (insert "(setq " (prin1-to-string x) " '"
;;                      (prin1-to-string real-x) ")\n"))))
;;           dismal-saved-variables)
;;     (write-file filename)
;;     (setq dismal-auto-save-counter dis-auto-save-interval)
;;     (kill-buffer (current-buffer))))
;;   (setq buffer-file-name filename)
;;   (clear-visited-file-modtime))


(defun dis-save-file ()
  "Save the current spreadsheet."
  (interactive)
  (if (not (buffer-modified-p))
      (message "(No dismal changes need to be saved.)")
   (message "DisSaving %s..." buffer-file-name)
   (dismal-write-buffer buffer-file-name)
   (if (file-exists-p dismal-buffer-auto-save-file-name)
       (delete-file dismal-buffer-auto-save-file-name))
   (set-buffer-modified-p nil)
   (message "DisSaved %s." buffer-file-name)))

(defun dis-write-file (filename)
  "Save the current spreadsheet."
  (interactive "FDis Save to file: ")
  (message "DisSaving %s..." filename)
  (if (equal (file-name-nondirectory filename) (buffer-name))
      nil
    (rename-buffer (file-name-nondirectory filename)))
  (setq buffer-file-truename filename)
  (setq buffer-file-name filename)
  (setq default-directory (file-name-directory filename))
  (setq dismal-buffer-auto-save-file-name (make-auto-save-file-name))
  (dismal-write-buffer filename)
  (set-buffer-modified-p nil)
  (message "DisWrote %s" filename))

;; test code, spring 97
;; (defun dis-write-file-stub (filename)
;;   "Save the current spreadsheet."
;;   (interactive "FDis Save to file: ")
;;   (message "DisSaving %s..." filename)
;;   (if (equal (file-name-nondirectory aa) (buffer-name))
;;       nil
;;     (rename-buffer (file-name-nondirectory aa)))
;;  (setq aa filename))


(defun dismal-do-auto-save ()
  (message "Auto-saving %s ..." (buffer-name)) (sit-for 2)
  (setq dismal-auto-save-counter dis-auto-save-interval)
  (if (buffer-modified-p)
      (let ((old-buffer-file-name buffer-file-name))
        (dismal-write-buffer dismal-buffer-auto-save-file-name)
        (setq buffer-file-name old-buffer-file-name))))

;; 2-16-93 -EMA fix: "save-some-buffers" clobbers dismal file format, so
;; need a dis-save-some-buffers.  ideally it would be more
;; sophisticated than this.

;; (defun dis-save-some-buffers (&optional arg exiting)
;;  "Dings."
;;  (interactive "P")
;;  (message "Not implemented.  Use dis-save-file.")
;;  (ding))

;; here may be Erik's fix:
(defun dismal-write-file-hook ()
   (if (and (or dismal-matrix (eq mode-name "dismal"))
            (not dismal-saving-file))
       (dis-save-file)))

;; Written to run in 19
(add-hook 'write-file-hooks 'dismal-write-file-hook)


;;;
;;;    XIVb.	File I/O - Translation functions between Excel and Forms
;;;

(defun dis-insert-file (filename)
  "Insert contents of file FILENAME into buffer starting at the current cell.  
Fields (cells) are seperated by dis-field-sep.
Cells are overwritten rather than pushed down.
Set mark after the inserted text."
  (interactive "FDis insert file: ")
  (let ((buffer-exists-already nil))
  (if (get-file-buffer filename) (setq buffer-exists-already t))
  (let ((read-col dismal-current-col)
        (read-row dismal-current-row)
        last-read-col
        (dismal-interactive-p nil)
        (original-buffer (current-buffer)) )
  (save-excursion
    (if buffer-exists-already
      (set-buffer (get-file-buffer filename))
      (find-file filename))
    (goto-char (point-min))
    (while (not (eobp))
       (message "Reading into row ... %d" read-row)
       (setq last-read-col (dismal-read-row original-buffer read-row read-col))
       (setq read-row (+ 1 read-row))
       (forward-line 1))
    (if buffer-exists-already
        nil
      (kill-buffer (current-buffer))))
  (if dis-auto-update
      (progn
         (message "Updating matrix...")
         (dismal-private-update-matrix)
         (message "Updating matrix...Finished.")))
  (dismal-set-mark (1- read-row) last-read-col)
  (dismal-visit-cell dismal-current-row dismal-current-col))))

(defun dismal-read-row (original-buffer read-row read-col)
  ;; returns how far it got
  (let ((eol (save-excursion (end-of-line) (point)))
        (new-item nil)  (done nil)
        (start (point))  (end nil)   )
  (while (not done)
    (setq end (if (search-forward dis-field-sep eol t)
                  (point)
                (setq done t)
                eol))
    ;;this strips leading blanks, which is hard on strings
    ;;(setq new-item(string-trim dismal-blank-bag(buffer-substring start end)))
    (setq new-item
          (buffer-substring start (if done end (1- end)))) ;don't read tabs
    (setq start end)  ;set up for next item
    (if (string= new-item "") (setq new-item nil))
    (save-excursion (set-buffer original-buffer)
      (dismal-set-cell read-row read-col
                      (dismal-convert-input-to-cellexpr new-item)
                      nil))
    (setq read-col (+ 1 read-col))   )
  (save-excursion (set-buffer original-buffer)
           (dismal-redraw-row read-row nil))
  (1- read-col)))

(defun dismal-insert-tabs ()
  (interactive)
  (while (not (eobp))
    (if (y-or-n-p "Tab this field?")
        (progn (forward-word 1) 
               (insert "\t")))
    (forward-line 1)))

(defun dis-set-dis-field-sep (initial-field-sep)
 "Set the field separator to use in insert-file."
 (interactive "P")
 (let ((new-sep (or initial-field-sep
                (read-string
                  (format "New field seperator value [was %s]: " 
                          dis-field-sep)) )))
  (cond ( (stringp new-sep)
          (setq dis-field-sep new-sep)
          (set-buffer-modified-p t)
          (message "dis-field-sep set to >>%s<<" dis-field-sep))
       ( t (error "dis-field-sep must be a string of char(s).")))))

;; (dis-set-dis-field-sep nil)


;;;
;;;	XIVc.	File I/O - Report functions
;;;

(defvar dismal-report-display-buffer nil "Where dismal reports are dumped.")

(defun dismal-make-print-file-name (file-name buffer-name extension)
  (concat (file-name-directory file-name)
          (concat (substring buffer-name 0 
                             (string-match ".[^.]*$" buffer-name))
          extension)))

(defun dis-print-report ()
 "Print out a copy of the current dismal sheet."
 ;; .dp stands for dismal printout
 (interactive)
 (if (not dis-print-command)
     (error "You must first set up to print.")
 (save-excursion
 (save-window-excursion
 (let* ((funny-file-name (dismal-make-print-file-name buffer-file-name 
                                                      (buffer-name) ".dp"))
        (dismal-interactive-p nil)
        (print-out-buffer (get-buffer-create funny-file-name)))
   (dis-make-report print-out-buffer t)
   (sit-for 0)
   (message "Printing...")
   (shell-command (format "%s %s" dis-print-command funny-file-name))
   (kill-buffer print-out-buffer)
   (message "Printing...Finished"))))))

(defun dis-make-report (&optional rbuffer report-header)
  "Print to RBUFFER a plain file all the visible cols of all the visible 
rows.  Must be called from a dismal buffer."
  (interactive)
  ;; set variables you need to use while in other buffer
  (let ((current-buffer (current-buffer))
        (current-buffer-file-name buffer-file-name)
        (current-buffer-name (buffer-name))
        (page-size dis-page-length)
        (ruler dismal-ruler)
        (report-buffer (or rbuffer
                           (if dismal-report-display-buffer
                               dismal-report-display-buffer)
                           (get-buffer-create "*Dismal-Report*"))))
    (dismal-undraw-ruler-rows)
    (pop-to-buffer report-buffer)
    (if (not buffer-file-name)
        (setq buffer-file-name
              (dismal-make-print-file-name current-buffer-file-name
                                           current-buffer-name ".dp")))
    (setq truncate-lines t)
    (erase-buffer)
    (if report-header (dismal-report-header current-buffer-file-name))
    (insert-buffer current-buffer)
    (pop-to-buffer current-buffer)
    (if report-header (dismal-draw-ruler dismal-current-row))
    (pop-to-buffer report-buffer)
    (if report-header (dismal-insert-report-rulers page-size ruler))
    (set-buffer-modified-p nil)
    (goto-char (point-max))
    (insert "\n")
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (write-file buffer-file-name) ))

(defun dis-write-report (&optional rbuffer)
  "Print to RBUFFER a plain file all the visible cols of all the visible 
rows.  Must be called from a dismal buffer."
  (interactive)
  (dis-make-report rbuffer nil))

(defun dismal-insert-report-rulers (page-size ruler)
  (goto-char (point-min))
  ;; no ruler on first page
  (forward-line  page-size)  ; you are starting on line 1
  (if (not (eobp)) (insert "" ruler "\n"))
  (while (not (eobp))
    (forward-line (- page-size 2)) ; 2 is size of ruler
    (if (not (eobp)) (insert "" ruler "\n"))))

(defun dismal-report-header (forms-file)
  (insert-current-time-string)
  (insert " - Dismal (" dismal-version ") report for user ")
  (insert (getenv "USER"))
  (insert "\nFor file " forms-file "\n\n")
  (insert (format "To print use  \"%s\"\n"
                  (format "%s %s" dis-print-command buffer-file-name)))
  (insert "-------------------------------------------------------------\n\n"))

(defun dis-print-setup ()
  (interactive)
  (call-interactively 'dis-set-ruler)
  (let ((old-dis-page-length dis-page-length))
    (setq dis-page-length (dismal-read-minibuffer "Printed page size: " t
                                    (prin1-to-string dis-page-length)) )
    (if (not (= old-dis-page-length dis-page-length))
        (set-buffer-modified-p t))
    ;; add 2: for the ruler lines
    (setq dis-print-command (format dis-raw-print-command
                                     (+ 2 dis-page-length)))
   (message "Finished dis-print-setup.")))

(defun dis-clean-printout ()
  "Strip header information and a set of leading digits from each line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (delete-region (point) (save-excursion (forward-line 8) (point)))
    ;; point of 4 is two digit numbe, could be smarter
    (kill-rectangle 4 (point-max))
    ;; now remove trailing whitespace
    (goto-char (point-min))
    (while (not (eobp))
       (end-of-line)
       (just-one-space)
       (forward-char -1)
       (delete-char 1)
       (forward-line 1))))

;; 2/93 EMA
(defun dis-unpaginate ()
  "Unpaginates a dismal report.  Call from within the report buffer."
  (interactive)
  (if (eq major-mode 'dismal-mode)
      (message "dis-unpaginate must be called within a report buffer as M-x dis-unpaginate.")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ".*\n.*\n" nil t)
      (replace-match "")))))

;;;
;;;	XIVd.	File I/O - Dumping tabbed regions
;;;

;;; Extensions to dis-dump by Stephen Eglen stephene@cogs.susx.ac.uk
;;; Tested to work in 20.2, 16-Nov-97 by Eglen.

(defun dis-tex-dump-range (filename)
  "Dump range in a TeX format."
  (interactive "FSave to tex file:") 
  ;; a bit naughty here, binding a global to get tex stuff to work
  (if (string= filename buffer-file-name)
      (setq filename 
            (dismal-make-print-file-name buffer-file-name
                                                (buffer-name) ".tex")))
  (let ( (dis-dump-end-row-marker dis-dump-tex-end-row-marker)
	 (dis-dump-between-col-marker dis-dump-tex-between-col-marker) )
    (dis-dump-range filename nil nil 'tex)))

(defun dis-tex-dump-range-file (filename)
  "Dump range in a TeX format with a tex header."
  (interactive "FSave to tex file:") 
  ;; a bit naughty here, binding a global to get tex stuff to work
  (if (string= filename buffer-file-name)
      (setq filename 
            (dismal-make-print-file-name buffer-file-name
                                                (buffer-name) ".tex")))
  (let ( (dis-dump-end-row-marker dis-dump-tex-end-row-marker)
	 (dis-dump-between-col-marker dis-dump-tex-between-col-marker) )
    (dis-dump-range filename nil nil 'tex-file) ))

(defun dis-html-dump-range (filename)
  "Dump range in a HTML format with a HTML header."
  (interactive "FSave to HTML file:") 
  ;; a bit naughty here, binding a global to get html stuff to work
  (if (string= filename buffer-file-name)
      (setq filename 
            (dismal-make-print-file-name buffer-file-name
                                                (buffer-name) ".html")))
  (let ( (dis-dump-start-row-marker "<tr><td>")
	 (dis-dump-between-col-marker "</td><td>")
         (dis-dump-end-row-marker "</td></tr>
") )
    (dis-dump-range filename nil nil 'html) ))

(defun dis-html-dump-file (filename)
  "Dump file in a HTML format with a HTML header."
  (interactive "FSave to HTML file:") 
  ;; a bit naughty here, binding a global to get html stuff to work
  (if (string= filename buffer-file-name)
      (setq filename 
            (dismal-make-print-file-name buffer-file-name
                                                (buffer-name) ".html")))
  (let ( (dis-dump-start-row-marker "<tr><td>")
	 (dis-dump-between-col-marker "</td><td>")
         (dis-dump-end-row-marker "</td>
") )
  (dismal-save-excursion
    (let ((mark-y  (aref dismal-mark 0))
          (mark-x (aref dismal-mark 1)))
      (dismal-set-mark 0 0)
      (dismal-jump-to-cell-quietly dismal-max-row dismal-max-col)
    (dis-dump-range filename nil nil 'html)
    (dismal-set-mark mark-y mark-x))) ))


(defun dis-write-tabbed-file (&optional formulas-p)
  "Dump the current buffer to a tabbed file.  If FORMULAS-P is t, then
write out formulas as s-expressions.  Writes an extra tab if last
field is empty for S." 
  (interactive)
  (let ((file-name (dismal-make-print-file-name buffer-file-name
                                                (buffer-name) ".dt")))
  (dismal-save-excursion
    (let ((mark-y  (aref dismal-mark 0))
          (mark-x (aref dismal-mark 1)))
      (dismal-set-mark 0 0)
      (dismal-jump-to-cell-quietly dismal-max-row dismal-max-col)
      (dis-dump-range file-name formulas-p)
      (dismal-set-mark mark-y mark-x)))
  (message "DisWrote tabbed file %s" file-name)))

 
(defun dis-dump-range (filename &optional formulas-p confirm type)
  "Dump current range to FILENAME as tabbed output.  If FORMULAS-P,
write out formulas as s-exps.  If CONFIRM, even when called non-interactively, 
will prompt if a file name already exists.
If type = TEX includes tex tabular environment using dis-tex-* variables. 
If type 'tex-file, also outputs 'begin{document}' and 'end{document}' so 
If type 'html, outputs '<table> and '</table>' so 
  that it can run as a complete html table.  
Writes an extra tab if last field is empty for use in other programs, like S."
  (interactive "FSave to file:")
  (if (file-exists-p filename)
      (if (or confirm (interactive-p))
	  (if (yes-or-no-p (format "Delete %s? " filename))
	      (delete-file filename)
	    (error "%s already exists" filename) )
	
	;; else no need to confirm
	(delete-file filename)))

  (if (interactive-p)
      (setq formulas-p (y-or-n-p "Write out a formula as a formula? ")))
  (dismal-select-range)
  (dismal-note-selected-range (format "Dumping %%s%%s:%%s%%d to %s" 
                                      (file-name-nondirectory filename)))
  (sit-for 1)
  (dismal-save-excursion
  (let ((start-row (range-1st-row dismal-cell-buffer))
        (start-col (range-1st-col dismal-cell-buffer))
        (end-row (range-2nd-row dismal-cell-buffer))
        (end-col (range-2nd-col dismal-cell-buffer))
        (dump-buffer (find-file-noselect filename))
        (old-buffer (current-buffer))
        (dm dismal-matrix)
        (dcf dismal-column-formats)
	(numwide nil)
        (dis-show-selected-ranges nil))
  (set-buffer dump-buffer)
  (let ((dismal-matrix dm)
        (dismal-column-formats dcf))
  (message "Dumping range...%s" type)

  ;; stuff to put on the front of the whole range
  (cond ((or (eq type 'tex) (eq type 'tex-file))
         (if (eq type 'tex-file)
	     (insert "\\documentclass{article}\n\\begin{document}\n"))
         (setq numwide (+ 1 (- end-col start-col)))
         ;; assume all entries are centred
         ;; may want to take alignment info from dismal values
         (insert (format "\\begin{tabular}{%s}\n" (make-string numwide ?c))))
        ((eq type 'html)
         (insert "<table>\n")))
  
  (matrix-funcall-rc
     (function (lambda (r c cell)
        ;; (my-message "formulas-p is %s, exp is: %s" formulas-p
        ;;            (dismal-get-exp r c))
        (let* ((format (dismal-get-column-format c))
               (expression (dismal-get-cell-exp cell))
               ;; (alignment (dismal-get-cell-alignment r c))
               ;; (width (dismal-column-width c))
               (string-value (dismal-flat-format
                                (if (and formulas-p
                                         expression
                                         (formula-p expression))
                                    ;; (dismal-get-cell-exp cell)
                                    expression
                                    (dismal-evaluate-cell r c))
                                (aref format 1))))
          ;; beginning of row stuff
          (cond ((= c start-col) (insert dis-dump-start-row-marker)))
          ;; main row stuff
          (cond ((stringp string-value) (insert string-value))
                (string-value (insert (format "%s" string-value)))
                ;; if at the end with no value, insert a tab for S
                ((= c end-col) (insert "\t")))
          ;; end of row stuff
          (cond ((= c end-col) (insert dis-dump-end-row-marker))
                (t (insert dis-dump-between-col-marker)))  )))
     start-row start-col end-row end-col dm))
  ;; Stuff to put on the end of the range.
  (cond ((or (eq type 'tex) (eq type 'tex-file))
         (insert "\\end{tabular}\n")
         (if (eq type 'tex-file)
             (insert "\\end{document}\n")))
        ((eq type 'html)
         (insert "</table>" "\n")))
  (write-file filename)
  (kill-buffer (current-buffer))
  (set-buffer old-buffer)
  (message "Dis Range dumped to %s." filename))))


;; old version as of 2-Jan-97 -FER
;;  (defun dis-dump-range (filename &optional formulas-p)
;;    "Dump the current range to a tabbed file.  If FORMULAS-P is t, then write out
;;  formulas as s-expressions.  Writes an extra tab if last field is empty for S."
;;    (interactive "FSave to file:")
;;    (if (file-exists-p filename)
;;        (if (yes-or-no-p (format "Delete %s? " filename))
;;            (delete-file filename)
;;          (error "%s already exists" filename)))
;;    (if (interactive-p)
;;        (setq formulas-p (y-or-n-p "Write out a formula as a formula? ")))
;;    (dismal-select-range)
;;    (dismal-note-selected-range (format "Dumping %%s%%s:%%s%%d to %s" 
;;                                        (file-name-nondirectory filename)))
;;    (sit-for 1)
;;    (dismal-save-excursion
;;    (let ((start-row (range-1st-row dismal-cell-buffer))
;;          (start-col (range-1st-col dismal-cell-buffer))
;;          (end-row (range-2nd-row dismal-cell-buffer))
;;          (end-col (range-2nd-col dismal-cell-buffer))
;;          (dump-buffer (find-file-noselect filename))
;;          (old-buffer (current-buffer))
;;          (dm dismal-matrix)
;;          (dcf dismal-column-formats)
;;          (dis-show-selected-ranges nil))
;;    (set-buffer dump-buffer)
;;    (let ((dismal-matrix dm)
;;          (dismal-column-formats dcf))
;;    (message "Dumping range...")
;;    (matrix-funcall-rc
;;       (function (lambda (r c cell)
;;          ;; (my-message "formulas-p is %s, exp is: %s" formulas-p
;;          ;;            (dismal-get-exp r c))
;;          (let* ((format (dismal-get-column-format c))
;;                 (expression (dismal-get-cell-exp cell))
;;                 (string-value (dismal-flat-format
;;                                  (if (and formulas-p
;;                                           expression
;;                                           (formula-p expression))
;;                                      ;; (dismal-get-cell-exp cell)
;;                                      expression
;;                                      (dismal-evaluate-cell r c))
;;                                  (aref format 1))))
;;            (cond ((stringp string-value) (insert string-value))
;;                  (string-value (insert (format "%s" string-value)))
;;                  ;; insert a tab if at the end with no value for S
;;                  ((= c end-col) (insert "\t")))
;;            (cond ((= c end-col) (insert "\n"))
;;                  (t (insert "\t")))  )))
;;       start-row start-col end-row end-col dm))
;;    (write-file filename)
;;    (kill-buffer (current-buffer))
;;    (set-buffer old-buffer)
;;    (message "Range dumped to %s" filename))))


;;;
;;;	XIVe.	File I/O - Working with gnuplot
;;;

;; This section written by Stephen Eglen <stephen@cns.ed.ac.uk>.
;; Feel free to contact him if you have problems getting gplot to
;; work (it does not work at Nottingham, alas).
;; Date: Tue, 7 Jan 97 12:17 GMT
;;
;;  As for the gnuplot code, that was much simpler, because of the gplot
;;  program that acts as a shell interface to gnuplot.  (This script can
;;  be retrieved from ftp://ftp.klab.caltech.edu/pub/holt/gplot-2.0.tar.gz
;;  (This gplot is in perl5, so you've got to get that as well, details 
;;   in gplot.)
;;  All that happens here is that a temporary file is created and then
;;  gplot called with this file.  If the user wants to change any way the
;;  plot looks, it can be done with a shell command, rather than having to
;;  provide this functionality through dismal.

; Usage (send-cmd-to-shell "gplot /tmp/disgnu.gp" t) Send the command
; line to the shell (using `shell-command' to process gplot commands
; didnt work.  gplot does fancy stuff with pipes, so I'm not sure if
; there was a problem with pipes.  So, now we create a buffer called 
; *dis-gnuplot* to process our gnuplot shell commands.
; (send-cmd-to-shell "ls" t)


(defvar dis-gnuplot-name "gplot" 
  "*Name of the gplot command name.")

(defun dis-gnuplot-range ()
  "Run gplot on current range."
  (interactive)
  (let ( (gnufile "/tmp/disgnu.gp"))
    (dis-dump-range gnufile nil nil)
    (dismal-send-cmd-to-shell (format "%s %s" dis-gnuplot-name gnufile) 
                           nil)))

(defvar dis-gnuplot-kill-gplot t
  "*Non-nil means run hook to quit gplot before killing gplot buffer.")

;;; Internal variables
;;;  (they start with "dismal-")
(defvar dismal-gnuplot-finish nil)
(defvar dismal-gnuplot-shell "dis-gnuplot")
(defvar dismal-gnuplot-shell-name "*dis-gnuplot*")

(defun dismal-send-cmd-to-shell (cmd visit)
  "Send CMD to the shell.  Non-nil VISIT will show the buffer it is sent to."
  (save-excursion
    (if (not (get-buffer dismal-gnuplot-shell))
	;; create the shell
	(save-excursion
	  (make-comint dismal-gnuplot-shell (or 
					  (getenv "SHELL")
					  "sh")
					  nil "-i")
	  (set-buffer dismal-gnuplot-shell-name)
	  ;; could use make-local-hook, but thats a relatively new
	  ;; function, so do it the hard way for now.
	  (make-local-variable 'dismal-gnuplot-finish)
	  (setq dismal-gnuplot-finish dis-gnuplot-kill-gplot)
	  (add-hook 'kill-buffer-hook 'dis-gnuplot-finished)
	  ;;(shell)
	  ))
    (let ((shell-process (get-buffer-process dismal-gnuplot-shell-name))
	  (beg) (end) (text-to-send))
      (beginning-of-line)
      (setq beg (point))
      (end-of-line 1)
      (setq end (point))
      ;(switch-to-buffer send-buffer-name)
      (goto-char (point-max))
      (setq text-to-send (concat cmd "\n") )
      (process-send-string shell-process text-to-send)

      ;; check to see if we want to make the send buffer visible 
      ;; in another window?
      (if visit
	  (progn
	    (switch-to-buffer-other-window  dismal-gnuplot-shell-name)
	    (other-window 1)))          )    )  )

(defun dis-gnuplot-finished ()
  "Kill the gplot process.
This should be executed in the *dis-gnuplot* buffer 
either interactively or via the kill-buffer-hook for that buffer."
  (interactive)
  (if (and  (boundp dismal-gnuplot-finish) dismal-gnuplot-finish)
      (progn
	(dismal-send-cmd-to-shell "gplot quit" t)
	;; need a little delay to kill gplot
	(sleep-for 2))))


;;;
;;;	XV.	Redrawing the screen
;;;

(defun dis-redraw-range (&optional min-row max-row)
  "Redraw the current range between point and mark."
  (interactive)
  (dismal-save-excursion
  (let ((min-row (or min-row (min (dismal-mark-row) dismal-current-row)))
        (max-row (or max-row (max (dismal-mark-row) dismal-current-row))))
  (while (<= min-row max-row)
    (if dismal-interactive-p (message "Redrawing range, line ... %s" min-row))
    (dismal-jump-to-cell-quietly min-row 0)
    (dismal-hard-redraw-row-non-interactive)
    (setq min-row (1+ min-row))) )))

(defun dis-quoted-insert ()
  "Insert a quoted char only after querying the user, insertion may 
mess up the display so that it is unparsable."
  (interactive)
  (if (y-or-n-p "Are you sure you want to insert a raw character? ")
      (progn (message "Character to insert: ")
             (call-interactively 'quoted-insert))))

(defun dis-recenter (arg)
  "Center row in window and redisplay screen.  With ARG, put point on line ARG.
The desired position of point is always relative to the current window.
Just C-u as prefix means put point in the center of the screen.
No arg (i.e., it is nil) erases the entire screen and then
redraws with point in the center.  Adjusts somewhat for rulers."
  (interactive "P")
  (if (and dis-show-ruler
           (numberp dismal-current-first-ruler-row)
           (numberp arg))
      (setq arg (+ arg 2)))
  (dismal-undraw-ruler-rows)
  (recenter arg)
  (dis-forward-row 0)
  (dismal-draw-ruler dismal-current-row))

(defun dismal-display-current-cell-expr (row column)
  (let ((cell-name (dismal-cell-name row column)))
  (setq dismal-current-cell cell-name)
  (if dismal-interactive-p
      (let ((message-log-max nil))
        (message (concat cell-name
                 ": "
                 (dismal-convert-cellexpr-to-string
                    (dismal-get-exp row column))
;; The rest of this function is for debugging
;           ", val: "
;           (dismal-convert-cellexpr-to-string
;            (dismal-get-val dismal-current-row dismal-current-col))
;           ", dep: "
;           (prin1-to-string
;            (dismal-get-deps (list dismal-current-row dismal-current-col)))
;           ", rct: "
;           (prin1-to-string
;            (dismal-get-mrk dismal-current-row dismal-current-col))
            ))))))

(defun dis-redraw (hard-redraw)
  "Redraw all the cells in the spreadsheet. If HARD-REDRAW, clear lines first."
  (interactive "P")
  (if (and (interactive-p)
           (not hard-redraw))
      (setq hard-redraw
            (y-or-n-p "Do hard redraw (y), or fast(n)? (y/n) ")))
  (message "Redrawing spreadsheet...")
  ;; if cleanup worked right, this could go.
  (matrix-funcall-rc
          (function (lambda (r c dummy)
             (let ((mrk (dismal-get-mrk r c)))
             (if (and mrk (consp mrk))
                 (dismal-set-mrk r c nil)))))
           0 0 dismal-max-row dismal-max-col dismal-matrix)
  (let ((buffer-originally-clean (not (buffer-modified-p))))
  (dismal-save-excursion (erase-buffer)
    (dismal-draw-labels)
    (let ((rowno 0)
          (nrow dismal-max-row))
      (while (<= rowno nrow)
         (dismal-redraw-row rowno hard-redraw)
         (setq rowno (1+ rowno)))))
  (dismal-make-ruler)
  (dismal-draw-ruler dismal-current-row)
  (if buffer-originally-clean (set-buffer-modified-p nil))
  (message "Redrawing spreadsheet...finished.")))

(defun dis-redraw-column (&optional column)
  (interactive)
  ;; Redraw all the cells in a column of the spreadsheet.
  (dismal-redraw-column (or column dismal-current-col)))

(defun dismal-redraw-column (column)
  ;; Redraw all the cells in a column of the spreadsheet.
  (save-excursion
    (dismal-draw-column-label column)
    (let* ((rowno 0))
      (while (< rowno dismal-max-row)
        (progn (dismal-redraw-cell rowno column t)
          (setq rowno (1+ rowno)))))))

(defun dismal-hard-redraw-row-non-interactive ()
  (beginning-of-line)
  (delete-region (point) (save-excursion (end-of-line) (point)))
  (dismal-redraw-row dismal-current-row t))

(defun dis-hard-redraw-row (number-of-rows)
  "Redraw the current row and move down a line."
  (interactive "p")
  (let ((buffer-originally-clean (not (buffer-modified-p))))
    (if (> number-of-rows 1)
        (progn 
          (dis-hard-redraw-row 1)
          (dis-hard-redraw-row (1- number-of-rows)))
      (dismal-save-excursion
        (beginning-of-line)
        (delete-region (point) (save-excursion (end-of-line) (point)))
        (dismal-redraw-row dismal-current-row t))
    ;; (message " moving %s %s" number-of-rows dismal-current-row) (sit-for 1)
      (dis-forward-row 1))
    (if buffer-originally-clean (set-buffer-modified-p nil))))

;; old way, 17-Jun-97 -FER
;; improvement suggested by Dan Nicolaescu <done@ece.arizona.edu>
;;(defun dismal-redraw-row (rowno reset-marks)
;;  (dismal-draw-row-label rowno)
;;  (let* ((row (vector-ref dismal-matrix rowno))
;;         (colno 0)
;;         (ncol (if row (max dismal-max-col (vector-length row)) 0)))
;;    (if dismal-interactive-p
;;        (message "Redrawing row %s of %s" rowno dismal-max-row))
;;    (if reset-marks
;;       (while (< colno ncol)
;;          (if (and (not (dismal-get-exp rowno colno))
;;                   (dismal-get-mrk rowno colno))
;;              (dismal-set-mrk rowno colno nil))
;;          (setq colno (1+ colno))))
;;    (setq colno 0)
;;    (while (< colno ncol)
;;       (progn (dismal-redraw-cell rowno colno t)
;;              (setq colno (1+ colno))))))

(defun dismal-redraw-row (rowno reset-marks)
  (dismal-draw-row-label rowno)
  (let* ((row (vector-ref dismal-matrix rowno))
         (colno 0)
         (ncol (if row (max dismal-max-col (vector-length row)) 0)))
    (if dismal-interactive-p
        (message "Redrawing row ... %s of %s" rowno dismal-max-row))
    (if reset-marks
       (while (< colno ncol)
          (if (and (not (dismal-get-exp rowno colno))
                   (dismal-get-mrk rowno colno))
              (dismal-set-mrk rowno colno nil))
          (setq colno (1+ colno))))
    (setq colno 0)
    (delete-region (point) (save-excursion (end-of-line) (point)))
    ;; now just redraw what you have to
    (while (and (>= ncol 0)
                (not (dismal-get-exp dismal-current-row ncol)))
       ;;(message "doing %s %s with %s" dismal-current-row ncol
       ;;       (dismal-get-exp dismal-current-row ncol)) 
       ;;(sit-for 1)
     (setq ncol (1- ncol)))
    (while (<= colno ncol)
       (progn (dismal-redraw-cell rowno colno t)
              (setq colno (1+ colno))))))


;; * collapse two funs
;; * on insert range,
;; * on redraw-column, do guys that write into it
;; * on insertion, if mrk is held, redraw

(defun dismal-redraw-cell (row column hard-update)
 ;; hard update means to put in blanks if value is nil
 ;; and reevluating
 ;; otherwise, saves time by not drawing blanks.
 ;;Redraw one cell.
 ;; don't do it if you are blank and not a hard-update,
 ;; don't do it if you are blank and you are used (cons in mrk)
 ;; don't do it if you are in a 0 width col
 (if hard-update (dismal-evaluate-cell row column))
 (if (let* ((not-exp (not (dismal-get-exp row column))))
       (and not-exp (or (not hard-update)
                        (consp (dismal-get-mrk row column)))))
     nil ;return
   (let* ((format (dismal-get-column-format column))
          (set-width (col-format-width format)))
   (if (= set-width 0)
       nil ;return
   (dismal-goto-cell row column nil)
   ;; set up for doing the write
   (save-excursion
   (let* ((alignment (dismal-get-cell-alignment row column))
          (delete-b-width set-width)
          (delete-f-width 0)
          (delete-width set-width)
          (leading-spaces 0)  (trailing-spaces 0)
          (cell-value (dismal-get-val row column))
;; (setq cell-value (dismal-get-val dismal-current-row dismal-current-col))
          ;; probably don't need full-eval 13-Jul-92 -FER
          ;;(cell-value (dismal-evaluate-cell row column))
          (string (dismal-flat-format cell-value (aref format 1)))
          (slength (if (stringp string) (length string) 0)))
   (cond ((< slength set-width)
          (cond ((eq 'default alignment)
                 (if (numberp cell-value)
                     (setq leading-spaces (- set-width slength))
                   (setq trailing-spaces (- set-width slength))))
                ((eq 'right alignment)
                 (setq leading-spaces (- set-width slength)))
                ((eq 'left alignment)
                 (setq trailing-spaces (- set-width slength)))
                ((eq 'center alignment)
                 (let ((trim (- set-width slength)))
                 (setq leading-spaces (/ trim 2))
                 (setq trailing-spaces (- trim leading-spaces)) )) ))
         ((= slength set-width)
          (setq leading-spaces 0)
          (setq trailing-spaces 0))
         ((> slength set-width)
          (setq leading-spaces 0)
          (setq trailing-spaces 0)
          (cond 
            ((eq 'default alignment)
             (if (numberp cell-value)
                 (if (> slength delete-width)
                     (setq string (make-string set-width ?*)))
               (setq delete-f-width
                     (dismal-find-format-space (- slength set-width) 'right
                                               row column))
               (setq string (substring string 0 (+ delete-f-width
                                                   delete-b-width)))))
            ((eq 'right alignment)
             (setq delete-b-width
                   (+ delete-b-width
                      (dismal-find-format-space (- slength set-width) 'left
                                                row column)))
             (setq string (substring string (- slength delete-f-width
                                               delete-b-width))) )
            ((eq 'left alignment)
             (setq delete-f-width
                   (dismal-find-format-space (- slength set-width) 'right
                                             row column))
             (setq string (substring string 0 (+ delete-f-width 
                                                 delete-b-width))))
            ((eq 'center alignment)
             (setq delete-f-width
                   (dismal-find-format-space (- slength set-width) 'right
                                             row column))
             (setq delete-b-width
                   (+ delete-b-width
                      (dismal-find-format-space (- slength set-width) 'left
                                                row column)))
             ;(my-message "  break" (+ asdf adsf))
             (let* ((trim (- slength delete-b-width delete-f-width))
                    (start (if (= trim 0) 
                               0
                               (max 0 (/ 2 trim))) ))
             (setq string (substring string start (- slength start))))))  ))

            ;;(message "string is [%s]>>%s<<" slength string)

     ;; do the write
     (forward-char 1)
     (delete-char (- delete-b-width))
     ;(setq spot (list delete-b-width (min delete-f-width
     ;                  (- (save-excursion (end-of-line) (point)) (point)))
     ;                 leading-spaces string trailing-spaces))
     ;; have to do this politely
     (delete-char (min delete-f-width 
                       (- (save-excursion (end-of-line) (point)) (point))))
     (insert-char ?\040 leading-spaces)
     (if (and string (stringp string)) (insert string))
     (insert-char ?\040 trailing-spaces)
     ;; don't know where you are left in the window
     ))))))

(defun dismal-find-format-space (wished-for direction row col)
 ;(message "In find-space with %s %s %s %s" wished-for direction row col) 
 (let ((result 0)
       (original-col col)
       (done nil)
       (increment (if (eq direction 'right) 
                      1
                      -1)) )
    (setq col (+ increment col))
    (while (and (not done) (>= col 0))
       (let ((mrk (dismal-get-mrk row col)))
       (if (and ;; (not (dismal-evaluate-cell row col))
                (not (dismal-get-val row col))
                (or (not mrk)
                    (and (numberp mrk)
                         (= 0 mrk))
                    (and (listp mrk)
                         (= (car mrk) row)
                         (= (cdr mrk) original-col))) )
           (progn (setq result (+ result
                                  (aref (dismal-get-column-format col) 0)))
                  (dismal-set-mrk row col (cons row original-col))
                  (if (> col dismal-max-col) (setq dismal-max-col col))
                  (if (>= result wished-for)
                      (setq done t)
                   (setq col (+ increment col))))
         (setq done t))))
  ;(message "returning find-space %s %s" direction (min wished-for result))
  (min wished-for result)))

(defun dismal-resize-column (column old-width width)
  ;; Change the width of a column.
  (if dismal-interactive-p 
      (message "Resizing column from %s to %s..." old-width width))
  (dismal-save-excursion
    (let* ((rowno -2))
      (if (> old-width width) ; getting smaller
          (while (<= rowno dismal-max-row)
            (dismal-goto-cell rowno column nil)
            (backward-char (1- old-width)) ; Move to cell's left end
            (delete-char (- old-width width))
            (setq rowno (1+ rowno)))
        (if (< old-width width) ; getting larger
            (while (<= rowno dismal-max-row)
              (dismal-goto-cell rowno column nil)
              (if (= old-width 0) (forward-char 1))
              (insert-char ?\040 (- width old-width))
              (setq rowno (1+ rowno)))))))
  (dismal-make-ruler))

(defun dismal-draw-labels ()
  (dismal-draw-row-labels)
  (dismal-draw-column-labels))

(defun dismal-draw-row-labels ()
  ;; (message "Labeling rows...")
  (dismal-goto-cell -1 0 nil)
  (dismal-set-first-printed-column)
  (beginning-of-line)
  (delete-char dismal-first-printed-column)
  (insert-char ?\040 (1- dismal-first-printed-column))
  (insert-char ?+ 1)
  ;; (message "Relabeling rows 0 to %s" dismal-max-row)
  (let ((rowno 0))
    (while (<= rowno dismal-max-row)
      (dismal-draw-row-label rowno)
      (setq rowno (1+ rowno))))
  (message "Relabeling rows...Finished."))

(defun dismal-draw-row-label (row)
  ;; Draw the label for ROW and put a vertical bar to its right.
  (dismal-goto-cell row 0 nil)
  (beginning-of-line)
  (delete-char dismal-first-printed-column)
  (insert (format dismal-row-label-format row)))

(defun dismal-add-row-labels-at-end (add)
  (while (>= add 0)
    (dismal-draw-row-label (- dismal-max-row 1 add))
    (setq add (1- add)))  )

(defun dismal-remove-row-label (row)
  ;; Remove the label for line ROW, and the line itself
  (dismal-goto-cell row 0 nil)
  (beginning-of-line)
  ;(delete-char dismal-first-printed-column)
  (delete-region (1- (point)) (save-excursion (end-of-line) (point))))

(defun dismal-remove-row-labels-at-end (remove)
  (let ((i 0))
  (while (< i remove)
    (dismal-remove-row-label (- dismal-max-row i))
    (setq i (1+ i)))  ))

(defun dismal-draw-column-labels ()
  ;; makes assumptions about which line the labels go on.
  ;; (message "Relabeling columns...")
  (let ((colno 0)
        (numcol dismal-max-col)) ;; used to be (matrix-width
                                        ;; dismal-matrix) -FER
    ;; put on leading +
    (dismal-goto-cell -1 0 nil)
    (beginning-of-line)
    (delete-char dismal-first-printed-column)
    (insert-char ?\040 (1- dismal-first-printed-column))
    (insert-char ?+ 1)
    ;; do rest
    (while (<= colno numcol)
      (dismal-draw-column-label colno)
      (setq colno (1+ colno))))
  (delete-rectangle (point) 
                    (save-excursion (forward-line -1) (end-of-line) (point))))

(defun dismal-draw-column-label (column)
  ;; Draw and underline the label for COLUMN.
  (let ((label (dismal-convert-number-to-colname column))
        (width (dismal-column-width column)))
    (if (= width 0)
       nil
    (dismal-goto-cell -2 column nil)
    (backward-char (1- width))          ; Move to cell's left end
    (delete-char width)                 ; Delete what's there
    (insert-char ?\040 (/ (- width (length label)) 2))
    (insert label)
    (insert-char ?\040 (- width (+ (length label)
                                   (/ (- width (length label)) 2))))
    (dismal-goto-cell -1 column nil)
    (backward-char (1- width))
    (delete-char width)
    (insert-char ?- (1- width))
    (insert-char ?+ 1))))


;;;
;;;	XVII.	Cell expression conversions
;;;

; (dismal-convert-input-to-cellexpr sexp)
; (dismal-convert-input-to-cellexpr "23.3")
; (dismal-convert-input-to-cellexpr "Brown86")
; (dismal-convert-input-to-cellexpr " ")
; (dismal-convert-input-to-cellexpr ".")
; (dismal-convert-input-to-cellexpr "23")
; (dismal-convert-input-to-cellexpr "-")
; (dismal-convert-input-to-cellexpr "(dis-plus a23:b21)")
; (dismal-convert-input-to-cellexpr "(quote (6107956 . -18))")
;  (dismal-convert-input-to-cellexpr '(setq aaa (dis-sum e0:e2)))
;  (dismal-convert-input-to-cellexpr "(setq aaa (dis-sum e0:e2))")
; (dismal-convert-input-symbol 'aaa)
;(dismal-convert-input-to-cellexpr '(dis-count a1:a340))
; (dismal-convert-input-to-cellexpr "(dis-plus a23)")
; (dismal-convert-input-to-cellexpr "(if (> a23 2) 3 4)")
; (dismal-convert-input-to-cellexpr "a23")
; (dismal-convert-input-to-cellexpr " a23")
; (dismal-convert-input-to-cellexpr " e6")
; (setq aa (dismal-convert-input-to-cellexpr "a$23"))
; (dismal-convert-input-to-cellexpr "a$23$")
; (dismal-convert-input-to-cellexpr '(dismal-r-c- 23 0))
; (dismal-convert-input-to-cellexpr "(dis-count-if-regexp-match B1:B3 \"B\\+$\")")

(defun dismal-convert-input-to-cellexpr (sexp)
  ;; Recursively replace symbols in SEXP that look like cell names with
  ;; expressions that access that cell.  
  ;; If in a setq use variables as defined, otherwise use the 
  ;; equivalent string
  (setq aa sexp)
  (cond ((and sexp (listp sexp))
         (if (eq (car sexp) 'setq)
             (nconc (list 'setq (cadr sexp))  
             ; safe b/c we are getting a list call here, otherwise use append
                   (dismal-convert-input-to-cellexpr (cddr sexp)))
             (cons (dismal-convert-input-to-cellexpr (car sexp))
                   (dismal-convert-input-to-cellexpr (cdr sexp)))))
        ((numberp sexp) sexp)
        ((or (and (stringp sexp)
                  (<= (length sexp) 6)  ; this allows ZZ9999
                  (string-match dismal-cell-name-regexp sexp 0))
             (symbolp sexp) )
         (dismal-convert-input-symbol sexp))
        ((dismal-number-stringp sexp) (dismal-convert-string-to-number sexp))
        ;; ((float-stringp sexp)
        ;;  (dismal-convert-string-to-float sexp))
        ((formula-string-p sexp)
         (dismal-convert-input-to-cellexpr (car (read-from-string sexp))))
        (t sexp)))

; (dismal-convert-cellexpr-to-string (dismal-get-exp dismal-current-row dismal-current-col))
; (setq sexp (dismal-get-exp dismal-current-row dismal-current-col))
; (dismal-convert-cellexpr-to-string "%")
; (message (concat "how are you" "%%" ))

;; (setq astring "aaaa%aaaaa")
; (dismal-percentager "aaaa%aaaaa")
(defun dismal-percentager (astring)
 ;; returns any %'s doubled
 (setq match-start (string-match "%" astring))
 (if match-start
    (concat (substring astring 0 (1+ match-start)) "%"
            (dismal-percentager (substring astring (1+ match-start))))
    astring))

; (dismal-convert-cellexpr-to-string (+ 2.3 3.4))
; (dismal-convert-cellexpr-to-string '(1 . 4))
; (setq sexp (+ 2.3 3.4))

(defsubst dismal-convert-cellexprlist-to-string (sexp)
  (concat (dismal-recursive-convert-cellexpr-to-string (car sexp))
          (if (null (cdr sexp))
              ""
            (concat " " (dismal-convert-cellexprlist-to-string (cdr sexp))))))

(defun dismal-convert-cellexpr-to-string (sexp)
 ;; Print the s-expression SEXP but convert numbers, strings, and cell
 ;; references to their printed representations.
 (cond ((null sexp) "")
       ((stringp sexp) (dismal-percentager sexp))  ; makes % printable
       ((numberp sexp) (int-to-string sexp))
       ;;((dismal-float-expr-p sexp)
        ;;(concat "\"" (dismal-convert-cellexpr-to-string (nth 1 sexp)) "\"")
       ;; (dismal-convert-cellexpr-to-string (nth 1 sexp)))
       ;; ((apply dismal-number-p sexp nil)
       ;; (apply dismal-number-to-string sexp nil))
       ;; trickyness here sets up printing ranges nicely??
       ;; has leading quote
       ((and (listp sexp) (listp (cdr sexp)) (rangep (cadr sexp)))  
        (concat 
         (dismal-convert-cellexpr-to-string (range-1st-cell (cadr sexp)))
         ":" (dismal-convert-cellexpr-to-string (range-2nd-cell (cadr sexp)))))
       ((and (listp sexp) (memq (car sexp) dismal-cell-types))
        (dismal-convert-cellref-to-cellname sexp))
       ((listp sexp)
        (concat "(" (dismal-convert-cellexprlist-to-string sexp) ")"))
       (t (prin1-to-string sexp))))

(defun dismal-recursive-convert-cellexpr-to-string (sexp)
  ;; Print the s-expression SEXP but convert numbers, strings, and cell
  ;; references to their printed representations.
 (cond ((null sexp) "")
       ((stringp sexp) (prin1-to-string sexp)) ; big-change here
       ((numberp sexp) (int-to-string sexp))
       ;; ((dismal-float-expr-p sexp)
        ;;(concat "\"" (dismal-convert-cellexpr-to-string (nth 1 sexp)) "\"")
       ;; (dismal-convert-cellexpr-to-string (nth 1 sexp)))
       ((apply dismal-number-p sexp nil)
        (apply dismal-number-to-string sexp nil))
       ;; trickyness here sets up printing ranges nicely??
       ;; has leading quote
       ((and (listp sexp) (listp (cdr sexp)) (rangep (cadr sexp)))  
        (concat 
         (dismal-convert-cellexpr-to-string (range-1st-cell (cadr sexp)))
         ":" (dismal-convert-cellexpr-to-string (range-2nd-cell (cadr sexp)))))
       ((and (listp sexp) (memq (car sexp) dismal-cell-types))
        (dismal-convert-cellref-to-cellname sexp))
       ((listp sexp)
        (concat "(" (dismal-convert-cellexprlist-to-string sexp) ")"))
       (t (prin1-to-string sexp))))



;; tricky way to get int-to-string and truncate to look like a real round
;; (dismal-smart-round 0.0 2)

(defsubst dismal-smart-round (anumber rightspace)
   (if (not (= 0 anumber))
       (+ anumber (* (if (>= 0 anumber) -1
                         1)
                   .5 (expt 10.0 (- rightspace))))
       ;; this is a crock to get 0.000 etc. out.
       0.0))

;; (dismal-flat-format-float -50.52 2)
;; (dismal-flat-format-float 0.0 2)
;; (dismal-smart-round 0.0 2)

;; this does not do what it should for rounding numbers by padding with 0!
;; 23-Mar-96 -FER
;; (setq rightspace 2)
;; (setq anumber 0.0)
;; (setq string (int-to-string (dismal-smart-round anumber rightspace)))
;;   (string-match floating-point-regexp string)
;; (setq decimal (if (> rightspace 0) "." ""))
;; (setq leftstart (match-beginning 1))
;;  (setq leftend (match-end 2))
;;  (setq rightstart (min (1+ (match-beginning 3)) (match-end 3)))
;;  (setq rightend (min (match-end 3) (+ rightstart rightspace)))
;;  (setq rightdigits (- rightend rightstart))

(defun dismal-flat-format-float (anumber rightspace)
  ;; Given the string returned by float-to-string of ANUMBER, 
  ;; return a string formatted according to the value of the decimal
  ;; in RIGHTSPACE.  The SPACE locals refer to the space in the 
  ;; formatted string, the START and END locals refer to positions in 
  ;; the argument STRING.  The DIGITS locals are equal to END - START.
  (let ((string (int-to-string (dismal-smart-round anumber rightspace))))
  (string-match floating-point-regexp string) ;; sets up match
  (let* ((decimal (if (> rightspace 0) "." ""))
         (leftstart (match-beginning 1))
         (leftend (match-end 2))
         (rightstart (min (1+ (match-beginning 3)) (match-end 3)))
         (rightend (min (match-end 3) (+ rightstart rightspace)))
         (rightdigits (- rightend rightstart)))
      (concat (substring string leftstart leftend)
              decimal
              (substring string rightstart rightend)
              (make-string (- rightspace rightdigits) ?0)))))
;;              (make-string (- rightspace rightdigits) ?\040)

;; this should be dead code (29-Aug-95), but is still used in 
;; some sheets somehow...
;(dismal-flat-format-float-string (float-to-string _f1) 2)
;(dismal-flat-format-float-string (float-to-string (float -1)) 2)
; (setq string (float-to-string (float -1)))
; (setq string "   -1.0000")
; (setq rightspace 2)

;; not longer necessary, convereted to all native floats 2-Jan-97 -FER
;; but used by convervsion programs.
(defun dismal-flat-format-float-string (string rightspace)
  ;; Given the STRING returned by float-to-string, return a string formatted
  ;; according to the value of the  decimal in rightspace.
  ;; The SPACE locals refer to the space in the formatted string, the
  ;; START and END locals refer to positions in the argument STRING.
  ;; The DIGITS locals are equal to END - START.
  (string-match floating-point-regexp string) ;; sets up match
  (let* ((decimal (if (> rightspace 0) "." ""))
         (leftstart (match-beginning 1))
         (leftend (match-end 2))
         (rightstart (min (1+ (match-beginning 3)) (match-end 3)))
         (rightend (min (match-end 3) (+ rightstart rightspace)))
         (rightdigits (- rightend rightstart)))
      (concat (substring string leftstart leftend)
              decimal
              (substring string rightstart rightend)
              (make-string (- rightspace rightdigits) ?\040))))


;;
;; Functions to do conversions on cell names
;;

(defun dismal-convert-input-symbol (symbol)
  ;; Convert string NAME to a cell access expression if it refers to a cell,
  ;; and replace symbols that look like numbers with floats.
  (let ((name (if (stringp symbol) 
                  symbol
                  (symbol-name symbol))))
    ;; (my-message "symbol is %s" name)
    (cond ((string-match dismal-cell-range-regexp name 0)
           (list 'quote (dismal-string-to-range name)))
          ((string-match dismal-cell-name-regexp name 0)
           (dismal-convert-cellname name))
          ((or (boundp symbol) (fboundp symbol)) symbol)
          (t (prin1-to-string symbol)))))

(defun dismal-convert-cellname (cellname)
  ;; Convert string NAME to a cell access expression.
  (string-match dismal-cell-name-regexp cellname)
  (let* ((row-fixed (< (match-beginning 4) (match-end 4)))
         (col-fixed (< (match-beginning 2) (match-end 2))))
    (list (if (and row-fixed col-fixed) 
              'dismal-rfcf
            (if row-fixed 'dismal-rfc-
                (if col-fixed 'dismal-r-cf 'dismal-r-c-)))
          (string-to-int (extract-match cellname 3))
          (dismal-convert-colname-to-number (extract-match cellname 1)))))

(defun dismal-convert-cellref-to-cellname (cellref)
  (concat
   (dismal-convert-number-to-colname (nth 2 cellref))
   (if (or (eq (car cellref) 'dismal-r-cf) (eq (car cellref) 'dismal-rfcf))
       "$")
   (prin1-to-string (nth 1 cellref))
   (if (or (eq (car cellref) 'dismal-rfc-) (eq (car cellref) 'dismal-rfcf))
       "$")))

(defun dismal-convert-string-to-number (string)
  ;; Convert the string to a float if it looks like one.
  ;; We add a quote here so when we eval the expression the floats
  ;; are left as they are and passed unchanged as arguments.
  ;;(if (string-match floating-point-regexp string 0)
  ;;    (list 'quote (apply dismal-string-to-number string nil))
  ;;  (car (read-from-string string)))
  (car (read-from-string string)))

;; (defun dismal-convert-string-to-integer (sexp)
;;  ;; Convert a string to an integer.  ;; You assume it matches
;;  (car (read-from-string sexp)))


; (dismal-convert-colname-to-number "a")
; (dismal-convert-colname-to-number 'a)
; (dismal-convert-colname-to-number 10)
; (dismal-convert-colname-to-number nil)
(defun dismal-convert-colname-to-number (name)
  ;; The inverse of dismal-convert-number-to-colname.
  (cond ((numberp name) name)
        ((and (not (stringp name)) (char-or-string-p name))
         (setq name (char-to-string name)))
        ((not name) nil)
        ((symbolp name) (setq name (prin1-to-string name))
         (dismal-convert-colname-to-number name))
        (t
  (let ((name-length (length name))
        (index 0)
        (column -1))
    (while (< index name-length)
      ;; !! Bob added `downcase' and changed ?A to ?a in following line:
      (setq column (+ (* (1+ column) 26) (- (aref (downcase name) index) ?a)))
      (setq index (1+ index)))
    column))))


;;;
;;;	XVIII.	Column formating commands
;;;

(defun dis-set-alignment (range-or-col alignment-style)
  (interactive (list (sm-run-menu 'dismal-range-or-col-menu 'col)
                     (run-menu 'dismal-alignment-style-menu "Default")))
  (message "Setting new alignment style...")
  (dismal-save-excursion
  (cond ((eq range-or-col 'column)
         (dismal-set-column-alignment dismal-current-col alignment-style)
         (dismal-redraw-column dismal-current-col))
        ((eq range-or-col 'range)
         (dismal-select-range)
         (dismal-show-selected-range)
         (dismal-note-selected-range "Aligning range %s%s:%s%d")
         (let ((start-row (range-1st-row dismal-cell-buffer))
               (start-col (range-1st-col dismal-cell-buffer))
               (end-row (range-2nd-row dismal-cell-buffer))
               (end-col (range-2nd-col dismal-cell-buffer))  )
          (matrix-funcall-rc (function (lambda (r c dummy)
                                (dismal-set-fmt r c alignment-style)))
                   start-row start-col end-row end-col dismal-matrix)
          ;; redraw here -FER
          (let ((dismal-interactive-p nil))
            (dis-redraw-range start-row end-row)))
         (dismal-note-selected-range "Aligning range %s%s:%s%d...Done")  )
        (t (error "Error in dismal-set-alignment with %s" range-or-col)))))

(defvar dismal-set-width-prompt
   (format "Enter column width (default is %d): " dis-default-column-width))

(defun dis-read-column-format (width)
  "Read in the format of the current column and redraw the ruler."
  (interactive
    (list (read-minibuffer dismal-set-width-prompt
             (prin1-to-string (dismal-column-width dismal-current-col)))))
  (if (and (> width dismal-normal-max-column-width)
           (not (y-or-n-p (format "Do you really want a column %d wide? " 
                                  width))))
      (error "Not making a wide column."))
  (dismal-save-excursion
    (message "Redrawing column %s..." 
             (dismal-convert-number-to-colname dismal-current-col))
    (dismal-set-column-format dismal-current-col width
                  (dismal-column-decimal dismal-current-col)
                  (dismal-column-alignment dismal-current-col))
    (dismal-make-ruler)
    (dismal-draw-ruler dismal-current-row)
    (message "Redrawing column %s...Done" 
             (dismal-convert-number-to-colname dismal-current-col))))

(defun dis-auto-column-width (arg)
  "Make column as wide as widest element plus ARG (default 0)."
  (interactive "P")
  (if (not arg) (setq arg 0))
  (dis-read-column-format (+ (dismal-max-column-width dismal-current-col)
                             arg)))

(defun dismal-max-column-width (cc)
  ;; find the maximum width element in cc
  (let ((result 1)
        (i 0))
    (while (<= i dismal-max-row)
      (let* ( (value (dismal-get-val i cc))
              (ilength (if (numberp value) ;; gosh, I hate numbers
                           (length (dismal-flat-format value 
                                       (aref (dismal-get-column-format cc) 1)))
                           (length value))))
      (if (> ilength result)
          (setq result ilength))
      (setq i (+ 1 i))))
  result))

;; used to use decimal
;(defun dismal-read-column-format (width decimal)
;  "Read in the format of the current column."
;  (interactive
;   (list (read-minibuffer dismal-set-width-prompt
;            (prin1-to-string (dismal-column-width dismal-current-col)))
;         (read-minibuffer "Enter decimal width: "
;            (prin1-to-string (dismal-column-decimal dismal-current-col)))))
;  (dismal-save-excursion
;    (message "Redrawing column %s..." 
;             (dismal-convert-number-to-colname dismal-current-col))
;    (dismal-set-column-format dismal-current-col width decimal
;                  (dismal-column-alignment dismal-current-col))
;    (dismal-make-ruler)
;    (dismal-draw-ruler dismal-current-row)
;    (message "Redrawing column %s...Done" 
;             (dismal-convert-number-to-colname dismal-current-col))))


; (set-col-format-width (dismal-get-column-format 1) 1)

(defun dis-expand-cols-in-range (arg)
  "Make all columns with width=0 in range have width arg."
  (interactive "p")
  (dismal-select-range)
  (dismal-save-excursion
    (let ((start-row (range-1st-row dismal-cell-buffer))
          (start-col (range-1st-col dismal-cell-buffer))
          (end-row (range-2nd-row dismal-cell-buffer))
          (end-col (range-2nd-col dismal-cell-buffer))
          (expanded-a-col nil)  )
     (message "Expanding columns between %s and %s ..." 
           (dismal-convert-number-to-colname start-col)
           (dismal-convert-number-to-colname end-col))
     (while (<= start-col end-col)
        (let ((format (dismal-get-column-format start-col)))
        (if (= 0 (col-format-width format))
            (progn (setq expanded-a-col t)
                   (dismal-resize-column start-col 0 arg)
                   (set-col-format-width format arg)
                   (dismal-redraw-column start-col))))
        (setq start-col (1+ start-col)))
     (if expanded-a-col 
         (progn (dismal-draw-column-labels)
                (dismal-make-ruler) (dismal-draw-ruler dismal-current-row)))
     (message "Expanding columns...Finished."))))

;(defun dismal-set-column-width (width)
;  "Set the width for the current column."
;  (interactive
;   (list (read-minibuffer dismal-set-width-prompt
;               (prin1-to-string (dismal-column-width dismal-current-col)))))
;  (if (and (> width dismal-normal-max-column-width)
;           (y-or-n-p (format "Do you really want a column %d wide? " width)))
;  (dismal-set-column-format dismal-current-col
;                            width
;                            (dismal-column-decimal dismal-current-col)
;                            (dismal-column-alignment dismal-current-col))))

(defun dis-set-column-decimal (decimal)
  "Set the decimal format for the current column."
  (interactive
   (list (read-minibuffer "Enter decimal width: "
            (prin1-to-string (dismal-column-decimal dismal-current-col)))))
  (dismal-set-column-format dismal-current-col
                            (dismal-column-width dismal-current-col)
                            decimal
                            (dismal-column-alignment dismal-current-col)))

;(setq format (make-vector 5 nil))
; (setq decimal 0)
; (setq align 'center)

;; Do resize b4 changing dismal-column-formats so dismal-goto-cell still works
(defun dismal-set-column-format (column width decimal align)
  (let* ((format (dismal-get-create-column-format column))
         (old-width (aref format 0))
         (old-decimal (aref format 1))
         (old-align (aref format 2)) )
    (dismal-resize-column column old-width width)
    (aset format 0 width)
    (aset format 1 decimal)
    (aset format 2 align)
    (if (or (not (equal old-width width))
            (not (equal old-decimal decimal))
            (not (equal old-align align)))
        (dismal-redraw-column column))))

(defun dismal-get-column-format (column)
  ;; Compute the format of the given COLUMN from dismal-column-formats.
  (let ((format (vector-ref dismal-column-formats column)))
    (if format
        format
      dismal-default-column-format)))

;; clean this up using dismal-column-alignment
(defun dismal-get-cell-alignment (row column)
  ;; Compute the alignment of the given COLUMN from dismal-column-formats.
  (let* ((alignment0 (dismal-get-fmt row column))
         (format (vector-ref dismal-column-formats column))
         (alignment1 (if format (aref format 2))) )
    (or alignment0
        alignment1
        (aref dismal-default-column-format 2))))


(defun dismal-sum-column-widths (start-col cols)
  ;; compute the sum of widths of cols start-col to (start-col + cols)
   (let ((i 0) (results 0))
   (while (<= i cols)
      (setq results (+ results (dismal-column-width (+ start-col i))))
      (setq i (1+ i)))
   results))

;; (dismal-raw-column-to-dismal-column 2)
;; (dismal-raw-column-to-dismal-column 0)
;; (dismal-raw-column-to-dismal-column 8)

;; some speed could be gained here with caching
;; only appears to be used by mouse click on cell
(defsubst dismal-raw-column-to-dismal-column (raw)
  (let ((sum 0) (dcol 0))
  ;; Update sum to include row numbers, plus 1 space
  (setq sum (+ 1 1 (truncate (log10 (max 1 dismal-max-row)))))
  (while (>= raw sum)
    (setq sum (+ sum (dismal-column-width dcol)))
    (setq dcol (+ dcol 1)))
  (max 0 (- dcol 1))))

(defun dismal-get-column-position (column)
  ;; Compute the position of the beginning the the given COLUMN.
  (if (= column 0)
      dismal-first-printed-column
    (+ (dismal-column-width (1- column))
       (dismal-get-column-position (1- column)))))


;;;
;;;	XIXa.	Utility functions - Date functions
;;;
;;; These should be made a little tighter and cleaner with conds and such,
;;; and pay head to common leap years. -FER


(defun dis-compute-command ()
  "This function computes the time for each command, as indicated by CRs, 
in a file created by the log program."
  (interactive)
  (let (initial-i initial-j final-i command-name)

  (setq initial-i dismal-current-row)
  (setq initial-j dismal-current-col)

  ;; starts at a good cell
  ;; copy time over
  (dis-backward-column 1)
  (setq time1 (dismal-get-val dismal-current-row dismal-current-col))

  ;; Now search for control-m and save its time
  (dismal-search "comint-send-input" 1)
  (setq final-i dismal-current-row)

  (dis-backward-column 2)
  (setq time2 (dismal-get-val dismal-current-row dismal-current-col))

  ;; Insert the difference in a cell.
  (dis-end-of-row)
  (dis-forward-column 1)
  (dis-edit-cell-plain (format "(- %s %s)" time2 time1))

  ;; Now create the string.
  (dismal-jump-to-cell initial-i initial-j)
  (setq command-name (dismal-get-val dismal-current-row dismal-current-col))
  (while (< dismal-current-row final-i)
    (dis-forward-row 1)
    (setq val (dismal-get-val dismal-current-row dismal-current-col))
    (if (numberp val) (setq val (format "%s" val)))
    ;; trim of the extra char
    (setq val (substring val 0 1))
    (setq command-name (concat command-name val))  )

  ;; Now insert it.
  (dis-end-of-row)
  (dis-forward-column 1)
  (dis-edit-cell-plain command-name)

  ;; Go back into the sheet to get in position for next command
  (dis-forward-row 1)
  (dis-first-column)
  (dis-forward-column 1)))

(defun current-line-in-window ()
  ;; taken from the gnu-emacs manual entry on count-lines, p. 377
  ;; so not necc. to add dismal- to front
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun dis-no-op (arg)
  (interactive "p")
  (error "%s is not defined for dismal-mode." (this-command-keys)))


;; old way, changes inspired by Dan Nicolaescu <done@ece.arizona.edu>
;; 17-Jun-97 -FER
;;(defun dismal-insert-blank-box (start-point rows cols text)
;;  ;; Starting at START-POINT insert ROW lines of COLS copys of TEXT.
;;  ;; The column is taken from that of START.
;;  ;; A rough inverse of this function is kill-rectangle.
;; (save-excursion
;; (let ((i 0) (cc nil))
;;   (goto-char start-point)
;;   (setq cc (current-column))
;;   (while (< i rows) 
;;     (dismal-insert-n-times text cols)
;;     (setq i (1+ i))
;;     (forward-line 1)
;;     (move-to-column cc)))))

(defun dismal-insert-blank-box (start-point rows width text)
  ;; Starting at START-POINT insert ROW lines of WIDTH copys of TEXT.
  ;; The column is taken from that of START.
  ;; A rough inverse of this function is kill-rectangle.
 (save-excursion
   (let ((cc (current-column)))
   (string-rectangle start-point 
                     (save-excursion (forward-line rows)
                                     (move-to-column cc)
                                     (point))
                     (make-string width text)))))

;; changes done here suggested by Dan Nicolaescu <done@ece.arizona.edu>
;; 17-Jun-97 -FER
;; (dismal-insert-n-times 32 10)
;; this is now a candidate for cutting 18-Jun-97 -FER
;; cut 18-Oct-97 -FER
;; (defsubst dismal-insert-n-times (item N)  ;(dismal-insert-n-times "a" t)
;;   ;; Insert ITEM (abs N) (t=1) times.
;;   (if (numberp N)
;;       (insert (make-string (abs N) item)) 
;;     (if N (insert item))))


;; (dis-current-date )
(defun dis-current-date (&optional month-first)
  "Insert current date as string. If MONTH-FIRST is t, do that."
  (let ((time-string (current-time-string)))
    (if month-first
     ;; insert month second:
     (if dis-insert-date-with-month-namep
	 (format "%s-%s-%s"
		 (substring time-string 4 7)
		 (if (string-equal " " (substring time-string 8 9))
		     (substring time-string 9 10)
		   (substring time-string 8 10))
		 (substring time-string  -2 nil))
	 (format "%s-%s-%s"
		 (if (string-equal " " (substring time-string 8 9))
		     (substring time-string 9 10)
		   (substring time-string 8 10))
		 (car (cdr (assoc (substring time-string 4 7)
                           dismal-date-table)))
		 (substring time-string  -2 nil)))
        ;; insert day before date:
     (if dis-insert-date-with-month-namep
	 (format "%s-%s-%s"
		 (if (string-equal " " (substring time-string 8 9))
		     (substring time-string 9 10)
		   (substring time-string 8 10))
		 (substring time-string 4 7)
		 (substring time-string  -2 nil))
	 (format "%s-%s-%s"
		 (car (cdr (assoc (substring time-string 4 7)
                                  dismal-date-table)))
		 (if (string-equal " " (substring time-string 8 9))
		     (substring time-string 9 10)
		   (substring time-string 8 10))
		 (substring time-string  -2 nil))))))

;; (dis-days-to-date (dis-date-to-days  "10-feb-1980"))
(defun dis-days-to-date (days &optional startdate)
  "Returns the date that DAYS after from 1 Jan 1970."
  ;; does not take account of leap year
  ;; inefficient algorithm
  (interactive)
  (let ((styear 1970)
	(stday 1)
	(stmonth 0)
        (month nil)) ; scratch var
  (if startdate
      (progn
	  (setq styear (string-to-int (substring startdate 7 nil)))
	  (setq stday (string-to-int (substring startdate 0 2)))
	  (setq stmonth
                (dis-get-month-int (substring startdate 3 6)))))
  (while (> days 366)
    (cond ((= 0 (% styear 4)) ; leap year
           (setq days (- days 366))
           (setq styear (+ 1 styear)))
          (t ; not leap year
           (setq days (- days 365))
           (setq styear (+ 1 styear)))))
  ;; this is awkard, but should work....
    (if (<= days 31) (setq month "Jan")
      (setq days (- days 31))
      (if (or (and (= 0 (% styear 4))
                   (<= days 29))
              (<= days 28))
          (setq month "Feb")
        (if (= 0 (% styear 4))
            (setq days (- days 29))
          (setq days (- days 28)))
        (if (<= days 31) (setq month "Mar")
          (setq days (- days 31))
          (if (<= days 30) (setq month "Apr")
            (setq days (- days 30))
            (if (<= days 31) (setq month "May")
              (setq days (- days 31))
              (if (<= days 30) (setq month "Jun")
                (setq days (- days 30))
                (if (<= days 31) (setq month "Jul")
                  (setq days (- days 31))
                  (if (<= days 31) (setq month "Aug")
                    (setq days (- days 31))
                    (if (<= days 30) (setq month "Sep")
                      (setq days (- days 30))
                      (if (<= days 31) (setq month "Oct")
                        (setq days (- days 31))
                        (if (<= days 30) (setq month "Nov")
                          (setq days (- days 30))
                          (if (<= days 31) (setq month "Dec"))
                          (setq days (- days 31)))))))))))))
    (format "%02d-%s-%02d" days month styear)))

;; (dis-date-to-days "01-feb-1970")
;; (setq date 	"01-feb-1970")
;; updated by Amy Ludlam
(defun dis-date-to-days (date &optional startdate)
  "Days between DATE ('dd-mmm-yyyy') and startdate (or 1 jan 1970).
Includes leap years."
  (interactive)
  (let ((styear 1970)
	(stday 1)
	(stmonth 0)
	(year (string-to-int (substring date 7 nil)))
	(day  (string-to-int (substring date 0 2)))
	(month (dis-get-month-int (substring date 3 6)))
	(days 0)
	(hold 0)
	(leaps 0))
  (if startdate
      (progn
	  (setq styear (string-to-int (substring startdate 7 nil)))
	  (setq stday (string-to-int (substring startdate 0 2)))
	  (setq stmonth
                (dis-get-month-int (substring startdate 3 6)))))
  ;; I would allow negative numbers, so comment this out. -fer
  ;;  (if (> styear year)
  ;;        (error "Invalid date range.")
  ;;      (if (= styear year)
  ;;          (if (> stmonth month)
  ;;              (error "Invalid date range.")
  ;;            (if (= stmonth month)
  ;;                (if (> stday day)
  ;;                    (error "Invalid date range."))))))

  ;; days between the years
  (if (< month stmonth)
      (setq year (1- year)))
  (setq days (* (- year styear) 365))
  ;; leap year additions
  (if (>= (- year styear) (% year 4))
      (progn
        (if (= (% year 4) 0)
            (setq hold (- year 4))
          (setq hold (- year (% year 4))))
        ;; count leap years between start and end
        (while (> hold styear)
          (setq leaps (1+ leaps))
          (setq hold (- hold 4)))
        ;; count leap year for first year if applicable
        (if (and (<= stmonth 2) (= (% styear 4) 0))
            (setq leaps (1+ leaps)))
        (if (and (not (= styear year))
                 (> month 2)
                 (= (% year 4) 0))
            (setq leaps (1+ leaps)))))
  ;; days between the dates
  (if (> stmonth month)
      (setq days (+ days
                    (dis-days-to-eoy stmonth stday)
                    (dis-days-from-boy month day)))
    (setq days (+ days 1  ; double substraction here
                  (dis-days-from-boy month day)))
    (setq days (- days (dis-days-from-boy stmonth stday))))
  ;; return the total
  (+ days leaps)))

;; (dis-get-month-int "JAN")
;; (defun dis-get-month-int (month-string)
;;   "Turn a month string into a number (or index)."
;;   (interactive)
;;   (setq month-string (downcase month-string))
;;   (let ((months '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))
;; 	(ind 0))
;;     (while (and (< ind 12)
;; 		(not (string= (downcase (nth ind months)) 
;;                               month-string)))
;;       (setq ind (1+ ind)))
;;     (if (< ind 12)
;; 	ind
;;       (error "Invalid month name given: %s." month-string))))

;; faster, cleaner version by Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; (dis-get-month-int "Jan")
(defun dis-get-month-int (month-string)
 "Turn a month string into a number 1 to 12."
 (interactive)
 (or (cdr (assoc (capitalize month-string) dismal-date-table))
     (error "Invalid month name given: %s." month-string) ))
  



;; updated by Amy Ludlam
(defun dis-days-to-eoy (month day)
  "Return number of days to end-of-year."
  (interactive)
  (let ((daystr '(31 28 31 30 31 30 31 31 30 31 30 31))
	(ind 11)
	(days 0))
    (while (<= month ind)
      (setq days (+ days (nth ind daystr)))
      (setq ind (1- ind)))
    (+ 1 days (- (nth (1- month) daystr) day))))

;; updated by Amy Ludlam
(defun dis-days-from-boy (month day)
  "Return number of days from beginning-of-year."
  (interactive)
  (let ((daystr '(31 28 31 30 31 30 31 31 30 31 30 31))
	(ind 1)
	(days 0))
    (while (<= ind month)
      (setq days (+ days (nth (1- ind) daystr)))
      (setq ind (1+ ind)))
    (+ days day)))


;;;
;;;	XIXb.	Utility functions - List functions
;;;

(defun dismal-map-apply (function list)
  (if (null list)
      ()
    (apply function (car list) nil)
    (dismal-map-apply function (cdr list))))

;; (setq aa '(1 2 (1 . 2) 3 4))
;; (dismal-del 1 aa)
;; (dismal-del '(1 . 2) aa)
;; (delete 1 aa)
;; (delete '(1 . 2) aa)

;; bummed for speed, <minakaji@osaka.email.ne.jp> 3-Sep-97 -FER
;; (for it is just the new 'delete' subr.)
;; ;; this is a destructive version, I think 14-Mar-92 -FER
;; ;; need to setq the results since only first ele might be deleted
;; (defun dismal-del (elt alist)
;;   ;; Delete any elements equal to ELT in LIST.
;;   (let ((n nil)
;;         (copy-list nil))
;;     (while (eq (car alist) elt)
;;        (setq alist (cdr alist)))
;;     (setq n (length alist))
;;     (setq copy-list alist)
;;     (while (> n 0)
;;       (if (equal elt (cadr copy-list))
;;           (setcdr copy-list (cddr copy-list)))
;;       (pop copy-list)
;;       (setq n (1- n)) )   )
;;   alist)

(defun oldfloatp (fnum)
  "Returns t if the arg is an old (Rosenblatt) floating point number, 
nil otherwise."
  (and (consp fnum) (integerp (car fnum)) (integerp (cdr fnum))))



;;;
;;;	XIXc.	Utility functions - Math functions
;;;

(defvar *grade-cut-points* nil 
  "Cut points for grades used by dis-grader and dis-ungrader.")
;; They are assoc lists

(setq *grade-cut-points*
      '( (77 "A+")
         (74 "A")
         (70 "A-")
         (67 "B+")
         (64 "B")
         (60 "B-")
         (57 "C+")
         (54 "C")
         (50 "C-")
         (47 "D+")
         (45 "D")
         (40 "E")
         (35 "F")
         (25 "X")
         (15 "X-")
         (0 "Z")))

(defvar *grade-points* nil)
;; They are assoc lists
(setq *grade-points*
      '( (77 "A+")
         (75 "A")
         (72 "A-")
         (68 "B+")
         (65 "B")
         (62 "B-")
         (58 "C+")
         (55 "C")
         (52 "C-")
         (48 "D+")
         (45 "D")
         (42 "E")
         (35 "F")
         (25 "X")
         (15 "X-")))

(defun dis-grader (cell)
  "Converts CELL to a grade using *grade-cut-points*.
Also see dis-grader."
  ;; this makes sure cell is a cell
  ;; (dismal-get-val (dismal-address-row cell) (dismal-address-col cell))
  (let ((value cell)
        (grade nil) (cut-point nil)
        (cut-points *grade-cut-points*))
  (while (and (not grade) cut-points)
     (setq cut-point (pop cut-points))
     (if (>= value (car cut-point))
         (setq grade (second cut-point))))
   grade))

(defun dis-ungrader (cell)
  "Converts CELL to a number based on grade using *grade-points*.
Also see dis-grader."
  (let ((value (upcase cell))
        (grade nil) (cut-point nil)
        (cut-points *grade-points*))
  (if (not (stringp value)) (setq grade 0))
  (while (and (not grade) cut-points)
     (setq cut-point (pop cut-points))
     (if (string= value (cadr cut-point)) 
         (setq grade (first cut-point))))
   grade))

(defvar dismal-last-fill-range-start 0)
(defvar dismal-last-fill-range-increment 1)

(defun dis-fill-range (start-count increment)
  "Between point and mark, insert a range of numbers starting at START-COUNT."
  ;; someday you'll see this do decrements, etc.
 ;(interactive "nNumber to start counting from: \nnNumber to increment with: ")
  (interactive
    (list (read-minibuffer "Number to start counting from: "
             (prin1-to-string dismal-last-fill-range-start))
          (read-minibuffer "Number to increment with: "
             (prin1-to-string dismal-last-fill-range-increment))))
  (if (not start-count) dismal-last-fill-range-start)
  (setq dismal-last-fill-range-increment increment)
  (dismal-select-range)
  (dismal-save-excursion
    (let ((start-row (range-1st-row dismal-cell-buffer))
          (start-col (range-1st-col dismal-cell-buffer))
          (end-row (range-2nd-row dismal-cell-buffer))  )
    (dismal-jump-to-cell start-row start-col)
    (while (<= start-row end-row)
      (dismal-set-cell dismal-current-row dismal-current-col
                      start-count nil)
      (dismal-redraw-cell dismal-current-row dismal-current-col nil)
      (dis-forward-row 1)
      (setq start-count (+ start-count increment))
      (setq start-row (1+ start-row)))
    (setq dismal-last-fill-range-start start-count) )))

;;  (dismal-adjust-range "l52:l500")

(defun dismal-adjust-range (range)
  (cond ((rangep range) range)
        ((stringp range) (dismal-string-to-range range))
        (t (message "Using current range from point and mark...")
           (dismal-select-range))))

; (dis-count '(dismal-range (dismal-r-c- 0 0) (dismal-r-c- 3 0)))
; (dis-count "a0:a3")

(defvar dd-result nil "Where dismal-do stores its results.")

(defun dis-count (range)
  "Given a cell RANGE computes the count of filled cells."
  (interactive "P")
  (setq range (dismal-adjust-range range))
  (dismal-do (function (lambda (row col old-result)
                (setq dd-result
                      (dismal-safe-count old-result (dismal-get-val row col)))))
             range 0))

(defun dis-count-words-in-range (range)
  "Count the words in RANGE"
  (interactive "P")
  (setq range (dismal-adjust-range range))
  (dis-count-regexp-in-range range "\\(\\w\\)+"))

(defun dis-count-regexp-in-range (range regexp)
  "Given a cell RANGE computes the number of times REGEXP is matched."
  (interactive "P")
  (setq range (dismal-adjust-range range))
  (dismal-do (function (lambda (row col old-result)
                  ;; (my-message "Got old-result of %s" old-result)
               (setq dd-result
                     (+ dd-result
                        (count-regexp-in-string regexp (dismal-get-val row col))))))
               range 0))

; (count-regexp-in-string "\\(\\w\\)+" "the odg-sat down." 0)
; (count-regexp-in-string "\\(\\w\\)+" "17-aug-92" 0)

(defun count-regexp-in-string (regexp string &optional start)
 (cond ((numberp string) 1)
       ((or (not string) (not (stringp string))) 0)
        (t (if (not (numberp start))
               (setq start 0))
           (let ((start (string-match regexp string start))
                 (end (match-end 0))
                 (real-end (length string)))
             (cond ((not start) 0)
                   ((>= end real-end) 1)
                   (t (1+ (count-regexp-in-string regexp 
                                                  string (+ 1 end)))))))))

(defun dis-count-if-regexp-match (range regexp)
  "Given a cell RANGE computes the number of cells that match REGEXP."
  (interactive "P")
  (setq range (dismal-adjust-range range))
  ;; dismal-do has a local result that it uses and returns on its own
  (dismal-do (function (lambda (row col old-val)
                (let ((val (dismal-get-val row col)))
                (if (and (stringp val)
                         (string-match regexp val))
                    (setq dd-result (1+ old-val))))))
             range 0))

(defun dis-match-list (range regexps)
  "Given RANGE returns the cell references that match regexps in REGEXPs."
  (interactive "P")
  (setq range (dismal-adjust-range range))
  (let ((match-result nil))
  (dismal-do (function (lambda (row col old-val)
                (let ((val (dismal-get-val row col)))
                (if (and (stringp val)
                         (dis-string-match-regexps regexps val))
                    (setq match-result (push (cons row col) match-result))))))
             range 0)
  (reverse match-result)))


(defun dis-string-match-regexps (regexps val)
  ;; assumes that regexps is a list of regexps
  (cond ((null regexps) nil)
        ((stringp regexps)
         (string-match regexps val))
        (t (or (string-match (car regexps) val)
               (dis-string-match-regexps (cdr regexps) val)))))

;; old result comes first
(defun dismal-safe-count (arg1 arg2)
  (if arg2 (1+ arg1) arg1))

(defun dis-sum (range)
  "Given a cell RANGE computes the sum of filled cells."
  (interactive "P")
  (setq range (dismal-adjust-range range))
  (dismal-do (function (lambda (row col old-result)
                (let ((val (dismal-get-val row col)))
             ;(my-message "%s:%s Result is %s" row1 col1 
             ;           (if (floatp result) (float-to-string result) result))
                (if (numberp val) ;; (floatp val)
                    (setq dd-result (+ dd-result val))))))
             range 0))

(defun dis-mean (range)
  "Given a cell RANGE computes the sum of filled cells."
  (interactive "P")
  ;; this adjusts string ranges into real ranges
  (setq range (dismal-adjust-range range))
  (let ((num 0)
	(sum-it 0.0))
    (setq sum-it
	  (dismal-do 
           (function (lambda (row col old-result)
                       (let ((val (dismal-get-val row col)))
             ;(my-message "%s:%s Result is %s" row1 col1 
             ;           (if (floatp result) (float-to-string result) result))
                         (if (numberp val)
                             (progn
                               (setq dd-result (+ dd-result val 0.0))
                               (setq num (+ num 1)) ) ) )))
           range 0) )
    (/ sum-it num)    ))

(defun dis-product (range)
  "Given a cell RANGE computes the product of filled cells."
  (interactive "P")
  (setq range (dismal-adjust-range range))
  (dismal-do (function (lambda (row col old-result)
                (setq dd-result
                      (dismal-safe-* dd-result (dismal-get-val row col)))))
             range 1))

(defun dismal-map (function first-value list)
  "Return the product of mapping function across list in a mapconcat fashion."
  (let ((result nil))
    (setq result (funcall function first-value (pop list)))
    (while list
       (setq result (funcall function result (pop list))))
    result))

(defun dismal-do (function arange initial-value)
  "Iteratively call FUNCTION on cells in ARANGE.  We bind
result to INITIAL-VALUE for your use, and return DD-RESULT which function
can use."
  ;; function can use dd-result
  ;; can't be a macro, unless you keep the guard of 
  ;; dismal-max-row/col in somehow
  ;; changed all these variable names to avoid dynamic variables.
 (let* ( ;;(dd-from-cell (range-1st-cell arange))
        ;; (to-cell (range-2nd-cell arange))
        (dd-row1 (range-1st-row arange))
        (dd-row2 (min dismal-max-row (range-2nd-row arange)))
        (dd-col1 (range-1st-col arange))
        (dd-col2 (min dismal-max-col (range-2nd-col arange)))
        (dd-start-col dd-col1)
        (dd-result initial-value))
    (while (<= dd-row1 dd-row2)
      (while (<= dd-col1 dd-col2)
        (funcall function dd-row1 dd-col1 dd-result)
        ;(my-message "%s:%s Result is %s" dd-row1 dd-col1 dd-result)
        (setq dd-col1 (1+ dd-col1)))
      (setq dd-row1 (1+ dd-row1))
      (setq dd-col1 dd-start-col))
   dd-result))

;; replaced with +, 28/9/94-fer
;; (defun dis-simple-plus (arg1 arg2)
;;   "A two arg version of plus that knows about floats and ints."
;;   (cond ((and (numberp arg1) (numberp arg2)) (+ arg1 arg2))
;;         ((and (floatp arg1) (numberp arg2))
;;          (f+ arg1 (f arg2)))
;;         ((and (floatp arg2) (numberp arg1))
;;          (f+ arg2 (f arg1)))
;;         ((and (floatp arg1) (floatp arg2))
;;          (f+ arg1 arg2))
;;         (t (error "Tried to add together %s and %s" arg1 arg2))))

(defun dis-plus (&rest args)
  "A safe version of plus that knows about floats, ints, cells and ranges."
  (let ((result 0))  
  (mapc
    (function (lambda (x)       ;; (my-message "Adding %s" x)
       (setq result
         (cond ((rangep x)
                (+ (dis-sum x) result))
               ((dismal-addressp x)
                (+ result 
                   (dismal-get-val (dismal-address-row x)
                                   (dismal-address-col x))))
               ((stringp x) result)
               ((and (numberp x) (numberp result)) (+ x result))
               ;; ((and (floatp x) (numberp result))
               ;;  (f+ x (f result)))
               ;; ((and (floatp result) (numberp x))
               ;;  (f+ result (f x)))
               ;; ((and (floatp x) (floatp result))
               ;;  (f+ x result))
               ((and (boundp x) (numberp (eval x)))
                (+ (eval x) result))
               ((and (boundp x) (not x))  ;;; this traps nil as 0
                result)
               (t (error "Tried to add together %s and %s" x result))))))
     args)
  result))

(defun dis-div (arg1 arg2)
  "A two arg version of divide that knows about div by 0."
  (if (or ;; (equal arg2 _f0) ;; _f0 undefined 2-Jan-97 -FER
          (and (numberp arg2) (= arg2 0)))
      (progn (ding) (ding) 
             (message "Dividing %s by %s given value na" arg1 arg2)
             "NA")
  (cond ((and (numberp arg1) (numberp arg2))
         (/ arg1  arg2))
        (t (error "Tried to dis-div %s and %s" arg1 arg2)))))

;; bummed by Mikio Nakajima <minakaji@osaka.email.ne.jp>
(defun dismal-safe-* (arg1 arg2)
  "A safe version of * that gives non-numbers the value of 1."
  (* (if (numberp arg1) arg1 1)
     (if (numberp arg2) arg2 1)) )


;; (defun dismal-coerce-to-float (arg default)
;; (cond ((stringp arg) default)
;;       ((numberp arg) (f arg))
;;       ;; ((floatp arg) arg)
;;       (t default)   ))


;;;
;;;	XIXd.	Utility functions - misc
;;;

;; (mapc '(lambda (x) (my-message "got %s" x)) '(1 2 3 34))

(defun extract-match (str i)		; used after string-match
  (condition-case ()
      (substring str (match-beginning i) (match-end i))
    (error "")))

;; same as in simple-menu.el

;; (formula-p '(dis-count))
;; (formula-p '(34343 . 33))
;; (formula-p '(quote (34343 . 33)))
(defun formula-p (item)
  ;; (my-message "Calling formula-p on %s" item)
  (and (listp item)
       (not (eq (car item) 'quote))
       ;; (not (floatp item))
  ))

; (formula-string-p "(dis-count-if-regexp-match B1:B3 \"B\\+$\")")
; (formula-string-p "(if t nil 4)")
; (formula-string-p "(if (> 3 4) nil 4)")
; (setq item "(if (> 3 4) nil 4)")
(defun formula-string-p (item)  ;(formula-string-p "(* 34 34)")
  (and (stringp item)           ;(formula-string-p "(/ (float 3) (float 3))")
       (string-match "^([a-zA-Z .0-9:$---/^\"+=<>\\]*)$" item)
       (fboundp (car (car (read-from-string item))))))

; (dismal-char-col-to-dismal-col 50)
(defun dismal-char-col-to-dismal-col (char-col)
  (let ((i 0)
        (total dismal-first-printed-column))
    (while (> char-col (setq total 
                             (+ total (dismal-column-width i))))
      (setq i (1+ i)))
    i))

(defun dismal-bobp ()
  (and (= dismal-current-row 0)
       (= dismal-current-col 0)))

(defun dismal-eobp ()
  (and (= dismal-current-row dismal-max-row)
       (= dismal-current-col dismal-max-col)))

;; this needs to be split into dismal-cell and dismal-glbal prints
;; including cell dependencies, which are not right on a list of 
;;; aliases such that they get called twice.29-Apr-96 -FER
(defun dis-debug-cell (arg)
  "Tell a developer more about a cell, if arg, that many chars of exp."
  (interactive "p")
  (let* ((cell (matrix-ref dismal-matrix dismal-current-row 
                          dismal-current-col))
         (val (dismal-get-cell-val cell))
         (exp (prin1-to-string (dismal-get-cell-exp cell))) )
    (message "%s-%s:[%s %s %s %s %s] C:%s MR:%s MC:%s RR:%s MC:%s"
             dismal-current-col
             (dismal-cell-name dismal-current-row dismal-current-col)
             (if (>= arg 4)
                 (format "{%s..}" (substring exp 0 (min arg (length exp))))
               exp)
             ;; (if (floatp val)
             ;;    (float-to-string val) (prin1-to-string val))
             (prin1-to-string val)
             (dismal-get-cell-dep cell)
             (dismal-get-cell-mrk cell)
             (dismal-get-cell-fmt cell)
             (dismal-get-column-format dismal-current-col)
             ;; MR          MC             RR
             dismal-max-row dismal-max-col dismal-current-first-ruler-row
             ;; MC
             dis-middle-col   )))

;; force a move to column by adding spaces
(defsubst dismal-force-move-to-column (col)
       ;; will be an integer passed
  (insert-char 32 (- col (move-to-column col))))
;;  (insert (make-string (- col (move-to-column col)) 32))


(defun dis-show-functions ()
  "Show all the functions that dismal will let you use."
  (interactive)
  (let ((old-buffer (current-buffer)))
    (pop-to-buffer help-buffer)
    (erase-buffer)
    (insert "Available dismal functions:
(A RANGE takes the form like a23:e35)
(See Emacs help for regexp forms)\n\n")
    (mapc (function (lambda (x)
             (insert (prin1-to-string x))
             (dismal-force-move-to-column (max (+ 2 (current-column))
                                          18))
             (insert (documentation x) "\n")))
          dis-user-cell-functions)
    (goto-char (point-min))  ))


;;;
;;;	XX.	Testing functions
;;;

(defun dis-test-dismal ()
  "A function to test a few things."
  (interactive)
  (if (bufferp (get-buffer "test-dismal"))
      (kill-buffer (get-buffer "test-dismal")))
  (switch-to-buffer-other-window "test-dismal")
  (dismal-mode)
  (dis-edit-cell-plain 1)
  (dis-forward-row 2)
  (dis-edit-cell-plain "<Left just")
  (message "calling dis-insert-column")
  (call-interactively 'dis-insert-column)
  (dis-forward-row 1)
  (dis-edit-cell-plain '(+ 2 3))
  (dis-forward-column 1)
  (dis-read-column-format 15)
  (dis-edit-cell-plain "a full 15 widee")
  (dis-jump 2 0)
  (dis-edit-cell-rightjust "flush>")
  (dis-jump 4 0)
  (dis-edit-cell-center 'a2)
  ;; test references and updating them
  (dis-jump 10 0)
  (dis-edit-cell-plain 1)
  (dis-jump 11 0)
  (dis-edit-cell-plain 2)
  (dis-jump 12 0)
  (dis-edit-cell-plain 3)
  (dis-jump 13 0)
  (dis-edit-cell-plain 4)
  (dis-jump 14 0)
  (dis-edit-cell-plain 5)
  (dis-jump 15 0)
  (dis-edit-cell-plain 6)

  (dis-jump 10 1)
  (dis-edit-cell-plain 'a10)
  (dis-jump 11 1)
  (dis-edit-cell-plain 'a11)
  (dis-jump 12 1)
  (dis-edit-cell-plain 'a12)
  (dis-jump 13 1)
  (dis-edit-cell-plain 'a13)
  (dis-jump 14 1)
  (dis-edit-cell-plain 'a14)
  (dis-jump 15 1)
  (dis-edit-cell-plain 'a15)
  ;; copy down  
  (dis-jump 10 1)
  (dis-set-mark)
  (dis-jump 15 1)
  (dis-copy-range)
  (dis-forward-row 1)
  (dis-paste-range)
  (dis-forward-row 3)
  (dis-open-line 1)
)


;;;
;;;	N.	Final code
;;;

(run-hooks 'dis-load-hook)

;; Wait till end so you know it got loaded.
(provide 'dismal)


;;;
;;;	N+1.	History 
;;;
;;; 
;;; 25-Jul-97 -release 1.2.  refixed float. better menus. html output. 19.34
;;;             a few more defsubst. better box drawing.
;;; 2-Jan-97 - release 1.1.  Partial use of optimizing compiler.  Fixed float.
;;;             Added tex tabbed output files, mouse and menus.
;;;             Working in 19.28.
;;; 23-Apr-96 - release 1.04.  Ran with XEmacs, can open write protected files.
;;; 15-May-93 - release 0.93 for Tony Simon
;;; 30-Dec-93 - released 0.92, with improved makefile
;;; 8-Dec-93 -released 0.9 to the net
;;; 6-Aug-93 -FER 0.85 released so that it can compile with 18.59 and 
;;;   with 19.17 (but uses none of the 19 features). 
;;; V 0.8 13-jul-92 -FER added some code from Bob Chassell (bob@gnu.ai.mit.edu)
;;; V 0.61 19-Mar-92 -FER shipped down to David
;;; V 0.6   various       beat on a lot
;;; Dec 91 on  - FER being modified
;;; V 0.5  Dec-91 (?)     got version from David Fox


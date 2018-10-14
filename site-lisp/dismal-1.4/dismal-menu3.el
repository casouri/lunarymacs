;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              
;;;; File            : dismal-menu.el
;;;; Author          : Nigel Jenkins, nej@cs.nott.ac.uk
;;;;                                  lpyjnej@psyc.nott.ac.uk  
;;;; Created On      : 15th March 1996
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: 3-Jan-97
;;;; 
;;;; PURPOSE
;;;;     DISMAL - Dis Mode Ain't Lotus.
;;;; 	Spreadsheet program for gnu-emacs.
;;;;    
;;;;    This program supplies functionality for using a pop-up menu
;;;;    with DISMAL.
;;;;
;;;; TABLE OF CONTENTS
;;;;
;;;;	I.	Set up menus for use in Dismal-mode buffer.
;;;;	II.	Defining Menu-bar for Dismal.
;;;;	II.a	MODEL item on menu-bar and all sub-menus
;;;;	II.b	OPTIONS item on menu-bar and all sub-menus
;;;;	II.c	DOC item on menu-bar and all sub-menus
;;;;	II.d	FORMAT item on menu-bar and all sub-menus
;;;;	II.e	COMMANDS item on menu-bar and all sub-menus
;;;;	II.f	GO item on menu-bar and all sub-menus
;;;;	II.g	EDIT item on menu-bar and all sub-menus
;;;;	II.h	File item on menu-bar and all sub-menus
;;;;
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	Menu system for using with DISMAL spreadsheet
;;;;              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	I.	Set up of menu ready for use in Dismal-mode buffer
;;;                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (define-key dismal-map [menu-bar edit] ())
;; (define-key dismal-map [menu-bar file] ())

;; now done with the local map
;; Check if already in dismal-mode to put the correct menu up
;;(if (equal major-mode 'dismal-mode)
;;      (use-global-map dis-global-map)
;;    (use-global-map global-map))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	II.	Defining Menu-bar for Dismal.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;;	II.a	MODEL item on menu-bar and all sub-menus
;;;

(define-key dismal-map [menu-bar model]
  (cons "dModel" (make-sparse-keymap "Model")))

(define-key dismal-map [menu-bar model Utils]
  '("Utils" . dis-utils-menu))
(define-key dismal-map [menu-bar model Stats]
  '("Stats" . dis-stat))
(define-key dismal-map [menu-bar model Codes]
  '("Codes" . dis-code))
(define-key dismal-map [menu-bar model KLM]
  '("KL model" . dis-klm))


;; UTILS pop-up-menu
(defvar dis-utils-menu (make-sparse-keymap "Utilities"))
;; This is a common idiom.  It makes the keymap available as a function 
;; call, somehow.  It is done for all the submenus.
(fset 'dis-utils dis-utils-menu)

(define-key dis-utils-menu [auto-align2]
  '("Auto-Align2" . dis-align-columns))
(define-key dis-utils-menu [auto-align]
  '("Auto-Align" . dis-auto-align-model))


;; STATS pop-up-menu
(defvar dis-stat-menu (make-sparse-keymap "Statistics"))
(fset 'dis-stat dis-stat-menu)

(define-key dis-stat-menu [stats]
  '("Print Statistics (not defined yet)" . dis-no-op))
(define-key dis-stat-menu [count]
  '("Count Codes (not defined yet)" . dis-no-op))


;; CODES pop-up-menu
(defvar dis-code-menu (make-sparse-keymap "Codes"))
(fset 'dis-code dis-code-menu)

(define-key dis-code-menu [init]
  '("Initialize" . dis-initialize-operator-codes))
(define-key dis-code-menu [load]
  '("Load" . dis-load-op-codes))
(define-key dis-code-menu [code]
  '("Code" . dis-op-code-segment))
(define-key dis-code-menu [save]
  '("Save" . dis-save-op-code))

;; KLM pop-up-menu
(defvar dis-klm-menu (make-sparse-keymap "KLM"))
(fset 'dis-klm dis-klm-menu)

(define-key dis-klm-menu [init]
  '("Initialize" . init-make-aliases))
(define-key dis-klm-menu [dups]
  '("Display dups" . display-dup-aliases))


;;;
;;;	II.b	 OPTIONS item on menu-bar and all sub-menus
;;;

(define-key dismal-map [menu-bar options]
  (cons "dOpts" (make-sparse-keymap "Dis Options")))

(define-key dismal-map [menu-bar options zrange]
  '("Redraw Range" . dis-redraw-range))
(define-key dismal-map [menu-bar options ruler-redraw]
  '("Ruler Redraw" . dis-update-ruler))
(define-key dismal-map [menu-bar options row-redraw]
  '("Redraw Row" . dis-hard-redraw-row))
(define-key dismal-map [menu-bar options column-redraw]
  '("Redraw Column" . dis-redraw-column))
(define-key dismal-map [menu-bar options screen-redraw]
  '("Redraw Screen" . dis-redraw))
(define-key dismal-map [menu-bar options set-vari-menu]
  '("Set dismal Variables" . dis-setv))

;; SetV pop-up-menu

(defvar dis-setv-menu
  (make-sparse-keymap "Set Variables"))
(fset 'dis-setv dis-setv-menu)

(define-key dis-setv-menu [middle-col]
  '("Middle Column" . dis-set-metacolumn))
(define-key dis-setv-menu [auto-update]
  '("Auto Update" . dis-toggle-auto-update))
(define-key dis-setv-menu [2ruler]
  '("Toggle Ruler" . dis-set-ruler))
(define-key dis-setv-menu [ruler-row]
  '("Ruler Row" . dis-set-ruler-rows))
(define-key dis-setv-menu [auto-update]
  '("Show update" . dis-toggle-show-update))
;; changed to ruler-rowS, 25-May-96 -FER



;;;
;;;	II.c	DOC item on menu-bar and all sub-menus
;;;

(define-key dismal-map [menu-bar doc.]
  (cons "dDoc" (make-sparse-keymap "Dis Doc")))

(define-key dismal-map [menu-bar doc. show]
  '("Full Dismal Documentation" . dis-open-dis-manual))
(define-key dismal-map [menu-bar doc. about]
  '("About Dismal mode" . describe-mode))

(defun dis-open-dis-manual ()
  (interactive)
  (info (concat dismal-directory "/dismal.info"))
  (message "Use 'C-h m' to learn how to use info mode."))


;;;
;;;	II.d	FORMAT item on menu-bar and all sub-menus
;;;

(define-key dismal-map [menu-bar format]
  (cons "dFormat" (make-sparse-keymap "Dis Format")))

(define-key dismal-map [menu-bar format update-r]
  '("Update Ruler" . dis-update-ruler))
(define-key dismal-map [menu-bar format fonts]
  '("Set Font" .  mouse-set-font))
(define-key dismal-map [menu-bar format auto-width]
  '("Automatic Width" . dis-auto-column-width))
(define-key dismal-map [menu-bar format width]
  '("Set Col Width" . dis-read-column-width))
(define-key dismal-map [menu-bar format align]
  '("Alignment" . dis-set-alignment))
(define-key dismal-map [menu-bar format number]
  '("Decimal width" . dis-set-column-decimal))


;; (fset 'msf 'mouse-set-font)
;; (msf)
;; (mouse-set-font x-fixed-font-alist)
;; (call-interactively 'mouse-set-font)


;;;
;;;	II.e	COMMANDS item on menu-bar and all sub-menus
;;;

(define-key dismal-map [menu-bar commands]
  (cons "dComms" (make-sparse-keymap "Dis Commands")))

(define-key dismal-map [menu-bar commands 0log]
  '("Logging-Off" . log-quit))
(define-key dismal-map [menu-bar commands 1log]
  '("Logging-On" . log-initialize))
(define-key dismal-map [menu-bar commands deblnk]
  '("Del Blank Rows" . dis-delete-blank-rows))
(define-key dismal-map [menu-bar commands qrep]
  '("Query-Replace" . dis-query-replace))
(define-key dismal-map [menu-bar commands hupdt]
  '("Hard-Update" . dis-recalculate-matrix))
(define-key dismal-map [menu-bar commands updt]
  '("Update" . dis-update-matrix))
(define-key dismal-map [menu-bar commands lisfns]
  '("List dismal user functions" . dis-show-functions))
(define-key dismal-map [menu-bar commands filrng]
  '("Fill Range" . dis-fill-range))
(define-key dismal-map [menu-bar commands expand]
  '("Expand hidden cols in range" . dis-expand-cols-in-range))
(define-key dismal-map [menu-bar commands redrw]
  '("Redraw Display" . dis-redraw))
;;(define-key dismal-map [menu-bar commands dep-clean]
;;  '("Dependencies-clean" . dis-fix-dependencies))
(define-key dismal-map [menu-bar commands cp2dis]
  '("Copy text into Dismal" . dis-copy-to-dismal))
(define-key dismal-map [menu-bar commands align]
  '("Align Metacolumns" . dis-align-metacolumns))


;;;
;;;	II.f	GO item on menu-bar and all sub-menus
;;;

(define-key dismal-map [menu-bar go]
  (cons "dGo" (make-sparse-keymap "Dis Go")))
(define-key dismal-map [menu-bar go Jump]
  '("Jump to cell>" . dis-jump))
(define-key dismal-map [menu-bar go End]
  '("End of sheet" . dis-end-of-buffer))
(define-key dismal-map [menu-bar go Begin]
  '("Beginning of sheet" . dis-beginning-of-buffer))

;; These either don't work and/or aren't necessary
;; (define-key dismal-map [menu-bar go Scroll-Right]
;;   '("-->" . scroll-right))
;; (define-key dismal-map [menu-bar go Scroll-Left]
;;   '("<--" . scroll-left))
(define-key dismal-map [menu-bar go Row]
  '("Row" . dis-row))
(define-key dismal-map [menu-bar go Column]
  '("Column" . dis-column))


;; ROW pop-up-menu

(defvar dis-row-menu
  (make-sparse-keymap "Row"))
(fset 'dis-row dis-row-menu)

(define-key dis-row-menu [back]
  '("Back a row" . dis-backward-row))
(define-key dis-row-menu [forward]
  '("Forward a row" . dis-forward-row))
(define-key dis-row-menu [last]
  '("Goto Last row" . dis-last-row))
(define-key dis-row-menu [first]
  '("Goto First row" . dis-first-row))


;; COLUMN pop-up-menu

(defvar dis-column-menu
  (make-sparse-keymap "Column"))
(fset 'dis-column dis-column-menu)

(define-key dis-column-menu [back]
  '("Back a column" . dis-backward-column))
(define-key dis-column-menu [forward]
  '("Forward a column" . dis-forward-column))
(define-key dis-column-menu [last]
  '("Goto Last column" . dis-end-of-col))
(define-key dis-column-menu [first]
  '("Goto First column" . dis-start-of-col))


;;;
;;;	II.g	EDIT item on menu-bar and all sub-menus
;;;

;; Remove other edit, since it contains dangerous commands.
(define-key dismal-map [menu-bar edit] 'undefined)
(define-key dismal-map [menu-bar search] 'undefined)
(define-key dismal-map [menu-bar files] 'undefined)

(define-key dismal-map [menu-bar dedit]
  (cons "dEdit" (make-sparse-keymap "Dis Edit")))

(define-key dismal-map [menu-bar dedit modify]
  '("Modify cell justification" . dis-modify))
(define-key dismal-map [menu-bar dedit delete]
  '("Delete" . dis-delete))
(define-key dismal-map [menu-bar dedit insert]
  '("Insert" . dis-insert))
(define-key dismal-map [menu-bar dedit set]
  '("Edit cell" . dis-edit-cell-plain))
(define-key dismal-map [menu-bar dedit erase]
  '("Erase range" . dis-erase-range))
(define-key dismal-map [menu-bar dedit yank]
  '("Yank" . dis-paste-range))
(define-key dismal-map [menu-bar dedit copy]
  '("Copy range" . dis-copy-range))
(define-key dismal-map [menu-bar dedit kill]
  '("Kill range" . dis-kill-range))
;; (define-key dismal-map [menu-bar dedit undo]
;;  '("Undo" . dis-no-op))


;; MODIFY pop-up-menu

(defvar dis-modify-menu 
	(make-sparse-keymap "Modify"))
(fset 'dis-modify dis-modify-menu)

(define-key dis-modify-menu [e]
  '("Plain" . dis-edit-cell-plain))
(define-key dis-modify-menu [|]
  '("Center" . dis-edit-cell-center))
(define-key dis-modify-menu [=]
  '("Default" . dis-edit-cell-default))
(define-key dis-modify-menu [<]
  '("Left" . dis-edit-cell-leftjust))
(define-key dis-modify-menu [>]
  '("Right" . dis-edit-cell-rightjust))


;; DELETE pop-up-menu

(defvar dis-delete-menu
  (make-sparse-keymap "Delete"))
(fset 'dis-delete dis-delete-menu)

(define-key dis-delete-menu [marked-range]
  '("Marked-range" . dis-delete-range))
(define-key dis-delete-menu [column]
  '("Column" . dis-delete-column))
(define-key dis-delete-menu [row]
  '("Row" . dis-delete-row))


;; INSERT pop-up-menu
(defvar dis-insert-menu
  (make-sparse-keymap "Insert"))

(fset 'dis-insert dis-insert-menu)

(unless (string-match "XEmacs\\|Lucid" emacs-version)
  (define-key dis-insert-menu [z-box]
    '("Z-Box" . dis-insert-z-box))
  (define-key dis-insert-menu [marked-range]
    '("Marked-Range" . dis-insert-range))
  (define-key dis-insert-menu [lcells]
    '("Cells" . dis-insert-cells))
  (define-key dis-insert-menu [column]
    '("Column" . dis-insert-column))
  (define-key dis-insert-menu [row]
    '("Row" . dis-insert-row)))

;; SET pop-up-menu
(defvar dis-set-menu
  (make-sparse-keymap "Set Cell Parameters"))
(fset 'dis-set dis-set-menu)

(define-key dis-set-menu [center]
  '("Center Justified" . dis-edit-cell-center))
(define-key dis-set-menu [general]
  '("Plain" . dis-edit-cell))
(define-key dis-set-menu [left]
  '("Left Justified" . dis-edit-cell-leftjust))
(define-key dis-set-menu [right]
  '("Right Justified" . dis-edit-cell-rightjust))


;;;
;;;	II.h	File item on menu-bar and all sub-menus
;;;
;;; These are pushed on, it appears.

(define-key dismal-map [menu-bar Dfile]
  (cons "dFile" (make-sparse-keymap "Dis File")))

(define-key dismal-map [menu-bar Dfile Quit]
  '("Kill current buffer" . kill-buffer))
(define-key dismal-map [menu-bar Dfile Unpage]
  '("Unpaginate dismal report" . dis-unpaginate))

(define-key dismal-map [menu-bar Dfile TeXdump1]
  '("TeX Dump file (raw)" . dis-tex-dump-range))
(define-key dismal-map [menu-bar Dfile TeXdump2]
  '("TeX Dump file (with TeX header)" . dis-tex-dump-range-file))

(define-key dismal-map [menu-bar Dfile htmldumprange]
  '("Dump range as HTML table" . dis-html-dump-range))
(define-key dismal-map [menu-bar Dfile htmldumpfile]
  '("Dump file as HTML table" . dis-html-dump-file))

(define-key dismal-map [menu-bar Dfile Rdump]
  '("Range-Dump (tabbed)" . dis-dump-range))
(define-key dismal-map [menu-bar Dfile Tdump]
  '("Tabbed-Dump file" . dis-write-tabbed-file))

(define-key dismal-map [menu-bar Dfile PPrin]
  '("Paper-Print" . dis-print-report))
(define-key dismal-map [menu-bar Dfile FPrin]
  '("File-Print" . dis-make-report))
(define-key dismal-map [menu-bar Dfile 2Prin]
  '("Print Setup" . dis-print-setup))
(define-key dismal-map [menu-bar Dfile insert-file]
  '("Insert File..." . dis-insert-file))
(define-key dismal-map [menu-bar Dfile Write]
  '("Save buffer as..." . dis-write-file))
(define-key dismal-map [menu-bar Dfile Save]
  '("Save" . dis-save-file))
(define-key dismal-map [menu-bar Dfile Open]
  '("Open file" . find-file))
(define-key dismal-map [menu-bar Dfile New]
  '("New sheet" . dis-find-file))

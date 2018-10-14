;;;; -*- Mode: Emacs-Lisp -*-  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  
;;;; File            : log.el
;;;; Author          : Erik Altmann, Frank Ritter & Nick Jackson
;;;; Created On      : Sat Nov 14 19:24:27 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Fri Dec 2 1994
;;;; Update Count    : 755
;;;;  
;;;;  
;;;; PURPOSE 
;;;;  
;;;; Logs state information about an editing session, including
;;;; keystrokes (with timestamps, command names, but no longer
;;;; arguments), file snapshots, process output, and temporary
;;;; buffers.  Provides some parsing functions for the data.
;;;;
;;;; USE
;;;; 
;;;; 1. Compile the C code for the timer process (search for "main") and
;;;; set *log-timer-program* to point to the object code.  (At CMU,
;;;; the value of *log-timer-program* below will do for sun3_mach and
;;;; pmax_mach.) 
;;;; [e.g., cc timer.c -o timer.sun4]
;;;; 
;;;; 2. Put something appropriate in ./emergency.el (see
;;;; "log-load-emergency-file"). 
;;;; 
;;;; 3. Byte-compile ("M-x byte-compile log.el"). 
;;;; 
;;;; 4. Load ("M-x load-library log"). 
;;;; 
;;;; 5. Start logging ("M-x log-initialize"). 
;;;;
;;;;    State is written in "Log.<type>.timestamp", where <type> is
;;;;    "keys" (keystrokes), "temp-buffers" (an accumulation of temp 
;;;;    buffer contents), or a process name.  Change directory where 
;;;;    state files are written with M-x log-data-directory. 
;;;;
;;;;    A separate backup is made for a file every time it's saved,
;;;;    in the file's directory.  
;;;; 
;;;;    State files and backups are compressed (end in ".Z"). 
;;;; 
;;;; 6. Stop logging (exit emacs) or turn off logging with log-quit.
;;;; 
;;;; 7. Uncompress and visit a keystroke log file, then run
;;;; "log-time-episodes" on the buffer, and "log-spread-out" on the
;;;; output of that.  Then try "log-collapse" to collapse and count
;;;; series of the same character.
;;;;
;;;; 
;;;; THE KEYSTROKE LOG
;;;; 
;;;; Keystrokes accumulate in the buffer " *log*", one entry per line,
;;;; until there are lots of them, when they're written to a file and
;;;; compressed.  To make one big log with line numbers, "uncompress
;;;; Log.keys.*; cat -n Log.keys* > keystroke-log.txt".  (You might
;;;; first want to cut out the first few lines of the first log file,
;;;; which identify the system and user.)
;;;; 
;;;; Command arguments appear at the "effects column", at the right
;;;; end of each line.  Buffer switches log the destination buffer,
;;;; cursor line, and the window's location over the buffer.  Yanks
;;;; and kills log their contents, indented to effects column.
;;;; 
;;;; (old) Sample filename and log data:
;;;; 
;;;;    % head -18 Sat-Nov-28-04:48:22-1992
;;;;    User: Erik Altmann
;;;;    Creation-date: Sat Nov 28 04:44:17 1992
;;;;    System: SARCO.SOAR.CS.CMU.EDU
;;;;
;;;;    17642.234  [G]c       self-insert-command            
;;;;    17642.297  [G]u       self-insert-command            
;;;;    17642.437  [G]t       self-insert-command            
;;;;    17643.015  [G]^M      newline-and-indent             
;;;;    17643.594  [G]t       self-insert-command            
;;;;    17643.672  [G]e       self-insert-command            
;;;;    17643.937  [G]x       self-insert-command            
;;;;    17644.781  [G]t       self-insert-command            
;;;;    17647.703  ^[h        backward-kill-word             text
;;;;    17647.890  ^[h        backward-kill-word             cut
;;;;                                                         text
;;;;    17650.406  [G]^Y      yank                           cut
;;;;                                                         text
;;;;    % 
;;;;
;;;;    Column 1 - timestamp (seconds)
;;;;    Column 2 - [keymap]keystroke-sequence
;;;;    Column 3 - command name
;;;;    Column 4 - command effects
;;;;
;;;; New sample filename and log data:
;;;;
;;;;	User: Frank Ritter
;;;;	Creation-date: Fri Dec  2 12:34:04 1994
;;;;	System: shelley.psyc.nott.ac.uk
;;;;	Start-buffer: bob.dis
;;;;	71644.290   y          exit-minibuffer              
;;;;	71646.060   ^N         exit-minibuffer              
;;;;	71646.150   ^N         dis-forward-row              
;;;;	71646.650   ^P         dis-forward-row              
;;;;	71646.770   ^P         dis-backward-row             
;;;;	71648.100   e          dis-backward-row             
;;;;	71648.430   m          dis-edit-cell-plain          
;;;;	71648.660   y          self-insert-command          
;;;;	71648.860   __         self-insert-command          
;;;;
;;;;
;;;; DATA CODES
;;;;
;;;;   A space (" ") logs as a double underscore ("__"). 
;;;;
;;;;   [HERE: other codes...] 
;;;; 
;;;; NOTES AND BUGS
;;;; 
;;;; * should check before saving files.
;;;; * should write a header, of what it is and who it's for.
;;;; * 19.29 now has (current-time), which could replace much of this

;; Command usage log for Michael Hucka ("hucka" on krusty.eecs.umich.edu)
;; GNU Emacs 19.28.1 (sparc-sun-sunos4.1.3_U1, X toolkit) of Thu Nov  3 1994 
;; on bullwinkle.eecs.umich.edu
;; Started Tue Dec 13 16:01:11 1994
;; Ended Tue Dec 13 16:12:39 1994

;;;; Uses after-command-hook.
;;;; 
;;;; Loads either before or after other packages, and loads safely on
;;;; top of itself. 
;;;; 
;;;; If it wedges anyway, there's a back door
;;;; (log-load-emergency-file, bound always to C-x4l).
;;;; 
;;;; Uses kill-emacs-hook and temp-buffer-show-hook, but tries to be
;;;; smart about existing hooks (see log-reset-hook). 
;;;; 
;;;; The timer process sucks a little, but emacs doesn't have a fine-
;;;; grain timer.  A sun3_mach is too slow for this package to keep up
;;;; with this typist (who's of medium skill).  A pmax_mach is ok. 
;;;; Sometimes events appear to happen at the same time. 
;;;;
;;;; Doesn't deal with mouse events. 
;;;; 
;;;; The original "read-char" bypasses keymaps.  It's redefined here,
;;;; but it having a byte code means that compiled functions won't see
;;;; the new definition.  So far reloads only isearch.el. 
;;;; 
;;;; HISTORY:  see end of file. 
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *log-data-directory*
  (concat (getenv "HOME") "/")
  ;; HERE: should write all files here
  ;;"./"
  "*Directory in which to write the keys buffer.")

(defun log-data-directory (dir)
  "Prompts for a directory in which to write Log.stuff files."
  (interactive
   (list (expand-file-name
	  (read-file-name (format "Logging in %s.  Change to: " *log-data-directory*)
			  (concat (getenv "HOME") "/")
			  *log-data-directory* 'require))))
  (setq *log-data-directory* (expand-file-name dir)))

(defvar *log-timer-program*
  ;; "/afs/cs.cmu.edu/user/altmann/lib/timer/@sys/timer"
  "/aigr/staff/ritter/tools/emacs/log/755/timer.sun4"
  "*Pathname to compiled code for timer process.")

(defvar log-compress!  ;; -fer
  t  "*If t, compress before writing output.")

(defvar log-keys-buffer-name " *keys-log*"	; named to be hidden
  "Name of buffer in which keystrokes are logged.")

(defvar log-temp-buffer-name " *temp-buffer-log*"
  "Name of buffer in which output of temporary buffers is logged.")

(defvar log-auto-save-interval 500
  "*Number of keystrokes to record in one file.")

;; entry:
;; sorta more hidden, system globals
(defvar log-initialized nil "Has log been initialized?")
(defvar log-running nil "Is log currently running?")
(defvar log-auto-save-counter 0)	; set in log-initialize

(defvar log-timer-process nil)

(defvar log-timer-process-name "log-timer")


(defun log-initialize ()
  "Starts logging state information. To stop, M-x log-quit."
  (interactive)
  (if (and log-initialized log-running)
      (message (concat "Already logging, to "
		       *log-data-directory*))
    ;; else
    (if (not (and log-initialized (not log-running)
                  (string= emacs-version "^19")))

    (progn
    ;; now stuff you do once
    (setq log-running t)
    (let ((l (process-list))
          (process nil))
      (while l
	(setq process (car l))		; write existing process buffers
	(let ((buf (process-buffer process))) ; 12-22-93 - 
	  (if (and buf			; eg, log-timer has no buffer
		   (not (buffer-file-name buf)) ; all bufs may display-time
		   (buffer-modified-p))	; log temp-buffers elsewhere
	      (log-accumulation-file (process-name process)
				     (buffer-name buf))))
	(setq l (cdr l))))
    
    ;; this saves temp buffers before trouble (!) can starts
    ;; 10-3-93 - and other temp buffer output, future and current:
    ;; used to be (13-Jan-96) temp-buffer-show-hook.
    (log-reset-hook 'temp-buffer-show-function 'bufname)
    (let ((l (buffer-list)))
      (while l
	(log-temp-buffer (car l))
	(setq l (cdr l))))

    ;; set kill-emacs hook, to save stuff:
    (log-reset-hook 'kill-emacs-hook nil)
    
    ;; 9-27-93 - set version control to checkpoint every save:
    (log-redefine 'save-buffer)	; to always backup
    (log-redefine 'find-backup-file-name) ; to record backup in log
    (setq kept-old-versions 100	; lots and lots
	  kept-new-versions 100
	  version-control t)
    (save-some-buffers t) ; doesn't get called when after log-start*?
    
    ;; set up an accumulation buffer for temporary output:
    (log-accumulation-file "temp-buffers" log-temp-buffer-name)
    
    ;; interval to save keystrokes and clear the keys buffer:
    (setq log-auto-save-counter log-auto-save-interval)
    (setq log-initialized t)

    (let ((mess nil))
      (if (not (get-buffer "*log-warnings*"))
	  (setq mess (concat "Logging started in directory " 
                             *log-data-directory*))
	(setq mess "Logging started, but see buffer *log-warnings*."))
      (message mess)
      (sleep-for 1)
      
      ;; start the timer.  this call has a funny effect on anything that
      ;; comes after it, so put it last.
      (log-start-timer-process)
      mess)
    (if (string= emacs-version "^18")
        (log-modify-keymaps)
      (if (not (member 'log-stamp-date post-command-hook))
          (setq post-command-hook (cons 'log-stamp-date post-command-hook)))
    ))
   ;; else
    (progn 
          ;; in 19 use post-command-hook
          ;; (setq post-command-hook 'log-stamp-date)
      (if (not (member 'log-stamp-date post-command-hook))
          (setq post-command-hook (cons 'log-stamp-date post-command-hook)))
          (setq log-running t))
)))

;; 11-17-94 -fer
(defun log-quit ()
  "Turn off logging."
  (interactive)
  (setq post-command-hook
        (remove 'log-stamp-date post-command-hook))
  (setq log-running nil)
  (message "logging turned off."))

;; only interesting for 18
(defun log-modify-keymaps ()
;; ;; modify all maps recursively accessble from global map:
    (log-modify-keymap (current-global-map) "[G]")

    ;; 1-14-94 - modify the mouse keymap:
    (if (boundp 'mouse-map)  ;; -fer
        (log-modify-keymap mouse-map "[M]"))
    
    ;; then leave a hole, hoping no subsequent loads will stuff it, in
    ;; case something goes wrong:
    (global-set-key "\C-x4l" 'log-load-emergency-file)
    
    ;; minibuffer maps (give them better names than "[L]"):
    (log-modify-keymap minibuffer-local-completion-map "[MLCM]")
    (log-modify-keymap minibuffer-local-map "[MLM]")
    (log-modify-keymap minibuffer-local-must-match-map "[MLMMM]")
    
    ;; get local maps as they come along:
    (log-redefine 'use-local-map)
    
    ;; get new keybindings as they come along:
    (log-redefine 'define-key)
    (log-redefine 'global-set-key)
    (log-redefine 'local-set-key)
    
    ;; get some command arguments:
    (log-redefine 'yank)
    (log-redefine 'copy-region-as-kill)
    (log-redefine 'other-window)
    (log-redefine 'delete-window)
    (log-redefine 'delete-other-windows)
    (log-redefine 'pop-to-buffer)
    (log-redefine 'switch-to-buffer)
    (log-redefine 'scroll-up)		; print line scrolled to..
    (log-redefine 'scroll-down)
    (log-redefine 'end-of-buffer)
    (log-redefine 'beginning-of-buffer)
    (log-redefine 'delete-char)		; print deleted chars..
    (log-redefine 'delete-backward-char)
    
    ;; 9-28-93 - quietly save all process output:
    (log-redefine 'start-process)
    (log-redefine 'kill-buffer)		; write new process buffers
    (log-redefine 'save-some-buffers)
    (log-redefine 'set-process-filter)

    ;; redefine read-char, then make sure we get the non-compiled
    ;; version of routines that call it (so the call isn't compiled
    ;; out): 
    (log-redefine 'read-char)		; 8-17-93
    (fmakunbound 'isearch)
    (autoload 'isearch "isearch.el")
    
    ;; make sure new packages don't clobber our redefinitions:
    (log-redefine 'load)
)

;;     (log-modify-keymap (current-global-map) "[G]")
(defvar log-keymaps-modified nil)	; keymaps are a lattice...
(defun log-modify-keymap (keymap prefix)
  (if (and keymap			; locals can be nil
	   (not (memq keymap log-keymaps-modified)))
      (progn
	(setq log-keymaps-modified (cons keymap log-keymaps-modified))
	(cond ((arrayp keymap)		; full keymap (an array)
	       (log-walk-full keymap prefix))
	      ((and (keymapp keymap)		; sparse
		    (consp keymap))
	       (log-walk-sparse keymap prefix))
	      ((and (keymapp keymap)		; ESC-prefix, etc.
		    (fboundp keymap))
	       (if (arrayp (symbol-function keymap))
		   (log-walk-full (symbol-function keymap) prefix)
		 (log-walk-sparse (symbol-function keymap) prefix)))
	      (t
	       (error (format "log-modify-keymap: can't parse %s"
			      keymap)))))))

;; walks down an array:
(defun log-walk-full (keymap prefix)
  (let ((len (length keymap))
	(i 0))
    (while (< i len)
      (let ((command (aref keymap i)))
	(cond ((keymapp command)        ; nested keymap: recurse
	       (log-modify-keymap command (text-char-description i)))

	      ;; nested keymap, wrapped in a list (?!), so recurse
	      ;; (12-May-93 - FER):
	      ((and (listp command) (keymapp (car command)))
	       (log-modify-keymap (car command) (text-char-description i)))

	      ((null command))          ; nil entry, often many

	      ((log-wrap-this-p command) ; 1-14-94 - call early, for "t" case
	       (log-set-full keymap i (log-wrap command prefix)))
	      ;; dunno, so flag it, and don't wrap it:
	      (t
	       (log-warn (format "log-walk-full: not wrapping: %s" command)))
	      ))
      (setq i (1+ i)))))

;; walks down a list:
(defun log-walk-sparse (keymap prefix)
  (while keymap
    (if (cdr-safe (car-safe keymap))
	(cond ((and (keymapp (cdr (car keymap))) ; nested keymap, so recurse
                    (char-or-string-p (car (car keymap)))) ;; -fer
	       (log-modify-keymap (cdr (car keymap))
				  (text-char-description (car (car keymap)))))

	      ;; nested keymap, wrapped in a list, so recurse (adapted
	      ;; from 12-May-93 above; may never happen):
	      ((and (listp (cdr (car keymap))) (keymapp (car (cdr (car keymap)))))
	       (log-modify-keymap (cdr (car keymap))
				  (text-char-description (car (car keymap)))))

	      ((log-wrap-this-p (cdr (car keymap))) ; 1-14-94 - as above
	       (log-set-sparse (car keymap)
			       (log-wrap (cdr (car keymap)) prefix)))
	      
              ;; dunno, so flag it, and don't wrap it:
	      (t
	       (log-warn (format "log-walk-sparse: not wrapping: %s"
				 (cdr (car keymap)))))
	      ))
    (setq keymap (cdr keymap))))

;; 1-14-94 - sez whether we want to wrap this.  put commands and
;; functions to ignore in here.
(defun log-wrap-this-p (thing)
  (and (or (commandp thing)		; command (maybe lambda) or macro
	   (and (symbolp thing)
		(fboundp thing)))	; non-interactive, eg x-cut-text
       
       (not (and (symbolp thing)
		 (or ;; called on either up or down:
		  (eq thing 'x-mouse-ignore)
		  ;; called on mouse-up and mouse down:
		  (eq thing 'x-flush-mouse-queue))))))

;; record errors in wrapping keystrokes in a buffer; one 
;; occasionally finds keys bound to functions that someone forget to
;; define, but that shouldn't kill us:
(defun log-warn (msg)
  (let ((buf (get-buffer-create "*log-warnings*")))
    (save-excursion	
      (set-buffer buf)
      (goto-char (point-max))
      (insert (format "%s\n" msg)))
    (if log-initialized			; won't see this otherwise
	(message "Warning recorded in buffer *log-warnings*."))))

(defun log-gen-args (command)
  (let* ((sf (symbol-function command))
	 (nargs (if (listp sf)
		    (length (car (cdr sf)))
		  0))
	 arglist)
    (while (> nargs 0)
      (setq arglist (cons (intern (concat "arg" nargs)) arglist))
      (setq nargs (1- nargs)))
    arglist))

(defun log-set-full (keymap index command)
  (aset keymap index command))

(defun log-set-sparse (the-car command)
  (setcdr the-car command))

(defun log-wrap (command prefix)
  "Returns a wrapper for COMMAND, or just COMMAND if it's a keymap or
already wrapped.  PREFIX is an optional string, usually the command prefix."
  (if (or (keymapp command)
	  (eq 'log-keystroke		; already wrapped?
	      (car-safe (car-safe (cdr-safe (cdr-safe (cdr-safe command))))))
	  (not (log-wrap-this-p command)))
      command
    (` (lambda
	 (&rest args)    ; 1-14-94 - mouse functions are called w/args
	 (interactive)
	 (, (list 'log-keystroke	; send prefix and command
		  prefix
		  (if (consp command)	; then command itself is a lambda
		      (prin1-to-string (car command))
		    (prin1-to-string command))))
	 (setq this-command (quote (, command))) ; don't want the wrapper
	 (log-call '(, command) args)))))

;; i think this indirection is only needed because call-interactively
;; doesn't seem to know how to deal with macros, and because the user
;; can bind a symbol's function to either a macro or a function after
;; the key is wrapped.

(defun log-call (command args)
  (if (or (consp command)		; lambda (one hopes)
	  (and (fboundp command)	; function
	       (not (stringp (symbol-function command)))))
      (if (not args)		
	  (call-interactively command)	; keyboard command
	;; 1-14-94 - ELSE mouse command;  "args" are usually
	;; screen-relative xpos, ypos and any cut text.  log-screen
	;; info may jibe.  
	(apply command args)
	(log-command-in-process-buffer
	 (` (lambda () (insert (, (format "%s" args))))))
	(log-command-in-process-buffer
	 (` (lambda () (insert (, (log-screen))))))) ; 
    ;; ELSE
    (execute-kbd-macro command)))	; keyboard macro

(defun log-keystroke (the-keymap the-command)
  (let ((log-buffer (get-buffer-create log-keys-buffer-name))
	(orig-buffer (current-buffer))
	(indent-tabs-mode nil))		; indent with spaces
    (if (eq (setq log-auto-save-counter (1- log-auto-save-counter)) 0)
	(progn (log-do-auto-save)
	       (setq log-auto-save-counter log-auto-save-interval)))
    (unwind-protect			; need this?
	(progn
	  (set-buffer log-buffer)
	  (let ((indent-tabs-mode nil))
	    (goto-char (point-max))
	    (if (not (bolp)) (insert "\n"))
	    (insert (concat the-keymap ;; 18-2-94-FER fix for 19
                            (cond ((eq last-input-char 32) ; keystroke
				   "__")
                                  ((numberp last-input-char)
				       (text-char-description
					last-input-char))
                                  (t last-input-char))))
	    (indent-to-column 11 2)	; whitespace
	    (insert the-command)	; command name
	    (indent-to-column 40 1))	; effects column, prior to timestamp
	  (condition-case e
	      (progn
		(process-send-string log-timer-process "\n")
		(accept-process-output log-timer-process)) ; block on read
	    (error
	     (progn			; try restart timer
		(log-start-timer-process)
		(if (or (not (processp log-timer-process))
			(not (eq (process-status log-timer-process) 'run)))
		    (progn
		      (message (concat "Couldn't spawn timer >" ;; put <> in
				       *log-timer-program*      ;; -fer
				       "<")))))))
	  )
      (set-buffer orig-buffer)))
  nil)

(defun log-stamp-date ()
  (let ((log-buffer (get-buffer-create log-keys-buffer-name))
	(orig-buffer (current-buffer))
	(indent-tabs-mode nil))
    (if (eq (setq log-auto-save-counter (1- log-auto-save-counter)) 0)
	(progn (log-do-auto-save)
	       (setq log-auto-save-counter log-auto-save-interval)))
    (unwind-protect
	(progn
	  (set-buffer log-buffer)
	  (let ((indent-tabs-mode nil))
	    (goto-char (point-max))
	    ;; (indent-to-column 40 1)
            (insert (format "\t%s\n\t" last-command))
	    (insert
	     (cond ((eq last-input-char 32)
		    "__")
		   ((numberp last-input-char)
		    (text-char-description last-input-char))
		   (t (format "%s" last-input-char))))
	    ;;(indent-to-column 11 2)
            )
	  (condition-case e
	      (progn
		(process-send-string log-timer-process "\n")
		(accept-process-output log-timer-process))
	    (error
	     (progn
	       (log-start-timer-process)
	       (if (or (not (processp log-timer-process))
		       (not (eq (process-status log-timer-process) 'run)))
		   (progn
		     (message (concat "Couldn't spawn timer >"
				      *log-timer-program*
				      "<"))))))))
	(set-buffer orig-buffer))))

;; 10-1-93 - use a filter rather than an output buffer:
(defvar log-timestamp nil)

(defun log-timer-filter (process output)
  (let ((inhibit-quit t)
	(orig-buffer (current-buffer))
	(log-buffer (get-buffer-create log-keys-buffer-name)))
    (unwind-protect
	 (progn
	   (setq log-timestamp output)
	   (set-buffer log-buffer)
	   (let ((indent-tabs-mode nil))
	     (goto-char (point-max))
	     (beginning-of-line)
	     (insert output)		; timestamp: c1-c9
	     (indent-to-column 12 2)))	; whitespace
      (set-buffer orig-buffer))))


(defun log-start-timer-process ()  
  (cond ((or (not (processp log-timer-process))
             (not (eq (process-status log-timer-process) 'run)))
         (condition-case e  ; catch errors in starting the process
	     (setq log-timer-process
                   (start-process log-timer-process-name
					 nil ; no buffer
					 *log-timer-program*))
	   (error
	    (message "Couldn't start log-timer-process")
	    (ding)))))
  (if (processp log-timer-process)
      (let ((buf (get-buffer-create log-keys-buffer-name))
	    (cbuf (buffer-name (current-buffer))))
        ;; I have no idea why this goes on, 7/nov/94-FER
	(set-process-filter log-timer-process (function log-timer-filter))
	;; (process-kill-without-query log-timer-process)
	(save-excursion
	  (set-buffer buf)
          ;; insert system load with 'uptime', 7-Jul-97 -FER
	  (insert (format "User: %s\nCreation-date: %s\nSystem: %s\nStart-buffer: %s\n"
			  (user-full-name)
			  (current-time-string)
			  (system-name)
			  cbuf))
          (insert "Uptime: ")
          ;; This should put it in current buf.
          ;; there appears to be some problems at Nottingham or in gnu with
          ;; the other args options.
          (shell-command "uptime" t)
          (insert "\n")
	  (auto-save-mode -1)))))


;; interface for dumping arguments to log.  [tried moving to a preset
;; column here, but was getting the command output in all sorts of
;; funny places.  now the timer sends a string padded on the right by
;; blanks.]
(defun log-command-in-process-buffer (command &optional prefix)
  "Funcalls COMMAND in the log buffer, to capture its output.
Optional PREFIX is inserted first."
  (let ((log-buf (get-buffer log-keys-buffer-name)))
    (if (bufferp log-buf)
        (save-excursion
          (set-buffer log-buf)
	  (goto-char (point-max))	; effects column, last line
	  (if (< 52 (current-column)) (insert "; "))  ; add to what's there
	  (if prefix (insert prefix))	; ever used?
	  (funcall command)))))

;/* Timer process to return seconds.milliseconds for timing keystrokes
;   in emacs.  Gives unique timestamps for 99999 seconds (27 hours or so),
;   as seconds.milliseconds. 
;
;   Fri 10-1-93 - return only timestamp (format in emacs); add a seconds digit
;   Sat 11-14-92 - Erik Altmann */
;
;#include <stdio.h>
;#include <sys/time.h>
;#include <sys/types.h>
;
;main()
;{
;    char c;
;    struct timeval null_timeout, time;
;    fd_set readfds;
;
;    null_timeout.tv_sec = 0;    /* man select for info  */
;    null_timeout.tv_usec = 0;
;   
;    while (1) {
;
;        FD_ZERO(&readfds);      /* zero readfs, which is set by select */
;        FD_SET(fileno(stdin), &readfds);
;        select(1, &readfds, 0, 0, &null_timeout); /* block on stdin */
;        getchar();
;        gettimeofday(&time, 0);
;        /* sssss.mmm, then keystroke stuff padded on the right, then up to
;           28 characters of command, padded on the right to put the 
;           effects column at 52.  HERE: WARNING: if this changes update
;           occurrences of "effects column" in log.el. */
;        printf("%05d.%03d",
;               time.tv_sec % 100000,
;               time.tv_usec / 1000);
;    }
;}

;; command redefinitons.  defvarring them makes the file reloadable,
;; and avoids making the new definitions visible as commands.  to
;; redefine something, do the right defvar below, then log-redefine it
;; in log-initialize.

;; 10-3-93 - log temp buffers generated within emacs.  would the
;; following were enough, but with-output-to-temp-buffer has
;; a byte-code, so the symbol is compiled out.  have to use the hook.
;(defvar log-old-with-output-to-temp-buffer nil)
;(defvar log-new-with-output-to-temp-buffer
;  '(macro lambda (bufname &rest body)   ; arguments are unevalled
;          (eval (list* log-old-with-output-to-temp-buffer
;                       'bufname body)) 
;          (log-temp-buffer (get-buffer bufname) t)))

;; 10-3-93 - reset hooks; see also log-new-load.  if temp*hook is nil,
;; emacs uses something to display the temp buffer, but we set it
;; emacs assumes we're taking care of it.  is display-buffer right?
(defun log-temp-buffer-show-hook (bufname)
  (let ((buf (get-buffer bufname)))
    (log-temp-buffer-show-hook-basic buf)
    ;; if we're it, display buffer too:
    (if (or (not (boundp 'temp-buffer-show-hook))
            (eq temp-buffer-show-hook 'log-temp-buffer-show-hook))
	(progn
	  (print-help-return-message)
	  (display-buffer bufname)))))

(defun log-temp-buffer-show-hook-basic (buf)
  (save-excursion
    (set-buffer buf)
    (set-buffer-modified-p t))	; temp output doesn't modify a buffer
  (log-temp-buffer buf))

(fset 'log-temp-buffer-show-function 'log-temp-buffer-show-hook)

;; automatically save log buffer and files visited by process buffers
;; when exiting.
(defun log-kill-emacs-hook ()
  (let ((inhibit-quit t))		; want these buffers!
    (log-do-auto-save)
    (log-save-accumulation-buffers)))

;; 10-4-93 -     (log-reset-hook ' kill-emacs-hook nil)
(defun log-reset-hook (hook args)
  (let ((new-hook (car (read-from-string ; take this as defunned
			(concat "log-" (symbol-name hook)))))
	(new-hook-fn
	 (car (read-from-string
	       (concat "log-" (symbol-name hook) "-fn")))))
    (if (boundp hook)			; check if defined
	(cond ((null (eval hook))	; set hook to log hook
	       (set hook new-hook))
	      ((consp (eval hook))	; pushnew log hook
	       (if (not (memq new-hook (eval hook)))
		   (set hook (cons new-hook (eval hook))))) ;; eval -fer
	      ((fboundp (eval hook))	; wrap existing hook
	       (if (or (eq (eval hook) new-hook-fn)
		       (eq (eval hook) new-hook))
		   ()
		 (fset new-hook-fn
		       ;; put the other hook 2nd, in case it knows
		       ;; what it's doing:
		       (` (lambda ((, args ))
			    ((, new-hook ) (, args))
			    ((, (eval hook) ) (, args))
			    )))
		 (set hook new-hook-fn)))))))

(defvar log-wrapped-filters nil)
;; 9-30-93 - set process filters to save output of temp buffers:
(defvar log-old-set-process-filter nil)
(defvar log-new-set-process-filter
  '(lambda (process filter)
     ;; if non-nil filter hasn't been wrapped, wrap it:
     (if (and filter
	      (not (memq filter log-wrapped-filters)))
	 (let* ((var (car (read-from-string (format "log-%s" filter))))
		(defn (symbol-function filter)))
	   (set var defn)
	   (setq log-wrapped-filters (cons filter log-wrapped-filters))
	   (fset filter
		 (` (lambda (process output)
		      (funcall (, var) process output) ; call the filter
		      (let ((l (buffer-list)))
			(while l
			  (log-temp-buffer (car l))
			  (setq l (cdr l)))))))))
     ;; make the process filter the wrapped filter:
     (funcall log-old-set-process-filter process filter)))

;; 10-1-93 - added call from set-process-buffer.
;; 9-28-93 - set process buffers to visit a timestamped file.  process
;; names are unique, so accumulation buffers and files will be too.
(defvar log-old-set-process-buffer nil)
(defvar log-new-set-process-buffer
  '(lambda (process buffer)
     (if buffer				; could be nil
	 (log-accumulation-file (process-name process) buffer))
     ;; do this last, because starting processes is funny:
     (funcall log-old-set-process-buffer process buffer)))

;; experimentally commented out, 11-17-94 - FER
;; 
;; ;; what's going on here?  is log-old-start-process ever set to not-nil?
;; ;; -fer
;; (defvar log-old-start-process nil)
;; 
;; (defvar log-new-start-process
;;   (function 
;;     (lambda (name buffer program &rest args)
;;       (if buffer				; could be nil
;; 	  (log-accumulation-file name buffer))
;;       (apply (function funcall)
;;              log-old-start-process name buffer program args))))
;; 
;; (apply log-new-start-process (list 
;; 					 log-timer-process-name
;; 					 nil ; no buffer
;; 					 *log-timer-program*))


;; sets a timestamped file with prefix FILE-PREFIX to be the file
;; visited by BUFNAME, creating the buffer if necessary.  returns the
;; buffer. 
(defun log-accumulation-file (file-prefix bufname)
  (let* ((buf (get-buffer-create bufname))
	 (filename (concat *log-data-directory*
			   "Log." file-prefix "." (get-time-string))))
    (save-excursion
      (set-buffer buf)
      ;; don't write-file, because that changes the buffer name
      ;; too.  auto-save and hope files don't get too big.
      (setq buffer-file-name filename)
      (auto-save-mode 1))
    (log-command-in-process-buffer
     (` (lambda () (insert (concat "visiting<>" (, filename))))))
    buf))

;; save a process buffer visiting a file.
(defvar log-old-kill-buffer nil)
(defvar log-new-kill-buffer 
  '(lambda (bufname)
     (interactive "bKill buffer: ")
     (if (and (get-buffer-process bufname) ; buffer has process?
	      (buffer-file-name (get-buffer bufname))) ; buffer has file?
	 (save-excursion
	   (set-buffer bufname)
	   (save-buffer 0)))
     (funcall log-old-kill-buffer bufname)))

;; also gets save-buffers-kill-emacs. dumbly assumes that all process
;; buffer that visit files need to be automatically saved.
(defvar log-old-save-some-buffers nil)
(defvar log-new-save-some-buffers
  '(lambda (&optional arg exiting)
     (interactive "P")
     (log-save-accumulation-buffers)
     (funcall log-old-save-some-buffers arg exiting)))

;; print line numbers after scrolling and buffer leaps:
(defvar log-old-scroll-down nil)
(defvar log-new-scroll-down
  '(lambda (&optional arg)
     (interactive "P")
     (funcall log-old-scroll-down arg)
     (log-command-in-process-buffer
      (` (lambda () (insert (, (log-screen))))))))
       
(defvar log-old-scroll-up nil)
(defvar log-new-scroll-up
  '(lambda (&optional arg)
     (interactive "P")
     (funcall log-old-scroll-up arg)
     (log-command-in-process-buffer
      (` (lambda () (insert (, (log-screen))))))))

(defvar log-old-end-of-buffer nil)
(defvar log-new-end-of-buffer
  '(lambda (&optional arg)
     (interactive "P")
     (funcall log-old-end-of-buffer arg)
     (sit-for 0)			; redisplay, so screen info is right
     (log-command-in-process-buffer
      (` (lambda () (insert (, (log-screen))))))))
     
(defvar log-old-beginning-of-buffer nil)
(defvar log-new-beginning-of-buffer
  '(lambda (&optional arg)
     (interactive "P")
     (funcall log-old-beginning-of-buffer arg)
     (sit-for 0)
     (log-command-in-process-buffer
      (` (lambda () (insert (, (log-screen))))))))

(defvar log-old-delete-char nil)
(defvar log-new-delete-char
  '(lambda (n &optional killflag)
     (interactive "p\nP")
     (if (< (point) (point-max))
	 (log-command-in-process-buffer
	  (` (lambda () (insert (, (buffer-substring (point) (1+ (point)))))))))
     (funcall log-old-delete-char n killflag)))

(defvar log-old-delete-backward-char nil)
(defvar log-new-delete-backward-char
  '(lambda (n &optional killflag)
     (interactive "p\nP")
     (if (> (point) (point-min))
	 (log-command-in-process-buffer
	  (` (lambda () (insert (, (buffer-substring (point) (1- (point)))))))))
     (funcall log-old-delete-backward-char n killflag)))

;; 9-27-93 - take a snapshot at every save.  use emacs' version
;; control, but compress the backups.  if a file's buffer is
;; unmodified when logging starts, the preimage will be "file.~1~.Z".
;; if the buffer is modified when logging starts, the preimage will be
;; "file.~2~.Z".  if there were numbered backups lying around, then
;; the preimage will be either the lowest- or second-lowest numbered
;; "file.*.Z".  accumulation buffers (process and temp) are saved
;; quietly w/o backups (log-save-accumulation-files), so we won't get
;; snapshots of those files unless the user ^X^Ss explicitly in, say,
;; a shell buffer, and then they'll be compressed.

(defvar log-old-save-buffer nil)	; gets save-some-buffers, write-file
(defvar log-new-save-buffer
  '(lambda (&optional args)
     (interactive "p")
     (setq buffer-backed-up nil)	; buffer-local
     (let ((log-the-backup-name nil))
       (funcall log-old-save-buffer args)
       (if (and log-the-backup-name log-compress!) ;; -fer
	   ;; won't actually compress small files ("man compress").
	   (call-process "compress" nil 0 nil (car log-the-backup-name))))))

(defvar log-old-find-backup-file-name nil)
(defvar log-new-find-backup-file-name
  '(lambda (fn)
     (let ((backup-file-name
	    (funcall log-old-find-backup-file-name fn)))
       (log-command-in-process-buffer
	(` (lambda () (insert (, (car backup-file-name))))))
       (setq log-the-backup-name backup-file-name)))) ; also returns it

;; appears to be dead code, 13-Jan-96 -FER
;; the emacs version (files.el) won't handle, say, the ".Z" in
;; "log.el.~1~.Z": 
;; (defun backup-extract-version (fn)
;;  (if (and (string-match "\\([0-9]+\\)~" fn bv-length)
;;	   (= (match-beginning 0) bv-length))
;;      (string-to-int (substring fn (match-beginning 0) (match-end 0)))
;;    0))

;; 8-17-93 - fix for bug where isearch args aren't appearing in log
;; because isearch calls read-char which bypasses all keymaps (?).

(defvar log-old-read-char nil)
(defvar log-new-read-char
  '(lambda ()
     (log-keystroke "[R]" "read-char")
     (funcall log-old-read-char)))

;; [redefinitions used for s7.]
;; redefine load, to make sure to reclobber any of our redefinitions
;; clobbered by a packaged loaded after this one:

(defvar log-old-load nil)
(defvar log-new-load
  '(lambda (file &optional noerror nomessage nosuffix)
     ;; in case the new package redefines any functions; leave wrapped
     ;; those functions the new package will call to reset keys:
     (mapcar '(lambda (fn)
		(if (not (or (eq fn 'define-key)
			     (eq fn 'global-set-key)
			     (eq fn 'local-set-key)
			     (eq fn 'use-local-map)))
		    (log-re-redefine fn)))
	     log-redefinitions)
     (unwind-protect			; in case errors on load
	 (funcall log-old-load file noerror nomessage nosuffix)
       (mapcar 'log-redefine log-redefinitions)
       (log-reset-hook 'kill-emacs-hook nil) ; 10-3-93
       (log-reset-hook 'temp-buffer-show-hook 'bufname))
     ))

;; redefine key-binding functions to wrap the newly-bound commands.

(defvar log-old-define-key nil)
(defvar log-new-define-key
  '(lambda (keymap keys def)
     (funcall log-old-define-key
	      keymap keys (log-wrap def "[D?]")))) ; defined where?

(defvar log-old-global-set-key nil)
(defvar log-new-global-set-key
  '(lambda (key command)
     (funcall log-old-global-set-key key (log-wrap command "[G]"))))

(defvar log-old-local-set-key nil)
(defvar log-new-local-set-key
  '(lambda (key command)
     (funcall log-old-local-set-key key (log-wrap command "[L]"))))

(defvar log-old-use-local-map nil)	; what about use-global-map?
(defvar log-new-use-local-map
  '(lambda (keymap)
     (funcall log-old-use-local-map keymap)
     (log-modify-keymap keymap "[L]")))

;; redefine yanks/kills and functions that switch buffers to log their
;; arguments.  these functions are lower down the call-graph than the
;; actual commands.

(defvar log-old-copy-region-as-kill nil)
(defvar log-new-copy-region-as-kill
  '(lambda (beg end)
     (interactive "r")
     (funcall log-old-copy-region-as-kill beg end) ; so yank below is correct
     (log-command-in-process-buffer 'log-insert-kill)))

(defvar log-old-yank nil)
(defvar log-new-yank
  '(lambda (&optional arg)
     (interactive "*P")
     (funcall log-old-yank arg)
     (log-command-in-process-buffer 'log-insert-kill)))

(defvar log-old-other-window nil)
(defvar log-new-other-window
  '(lambda (arg)
     (interactive "p")	; should this be "P"?
     (funcall log-old-other-window arg)
     (log-modify-keymap (current-local-map) "[L]")	; just in case
     (log-command-in-process-buffer (log-current-buffer))))

(defvar log-old-delete-window nil)
(defvar log-new-delete-window
  '(lambda (&optional arg)
     (interactive)
     (funcall log-old-delete-window arg)
     (log-modify-keymap (current-local-map) "[L]")
     ;; hack; popper calls delete-window frequently, with confusing
     ;; results: 
     (if (interactive-p)
	 (log-command-in-process-buffer (log-current-buffer)))))

(defvar log-old-delete-other-windows nil)
(defvar log-new-delete-other-windows
  '(lambda ()
     (interactive)
     (funcall log-old-delete-other-windows)
     ;;(log-modify-keymap (current-local-map) "[L]")
     (if (interactive-p)
	 (log-command-in-process-buffer (log-current-buffer)))))

(defvar log-old-pop-to-buffer nil)
(defvar log-new-pop-to-buffer
  '(lambda (buffer &optional window)
     (funcall log-old-pop-to-buffer buffer window)
     (log-modify-keymap (current-local-map) "[L]")
     ;; interactive-p seems to become nil when call is sufficiently nested:
     (log-command-in-process-buffer (log-current-buffer))))

(defvar log-old-switch-to-buffer nil)
(defvar log-new-switch-to-buffer
  '(lambda (buffer &optional norecord)
     (interactive "BSwitch to buffer: ")
     (funcall log-old-switch-to-buffer buffer norecord)
     (log-modify-keymap (current-local-map) "[L]")
     (if (interactive-p)
	 (log-command-in-process-buffer (log-current-buffer)))))

;; data processing utilities:

;; sets:
;;   lex-id (string)
;;   lex-pause
;;   lex-start (integer, as string)
;;   lex-end (integer as string or nil)
;;   lex-duration (integer as string or nil)
;;   lex-keys (string)
(defun log-lex-episode-line ()
  (setq 
   lex-id nil
   lex-pause nil
   lex-start nil
   lex-end nil
   lex-duration nil
   lex-keys nil)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")		; skip leading whitespace, if any
    ;; id:
    (if (not (looking-at "\\([0-9]+\.[0-9]+\\)[ \t]"))
	()
      (setq lex-id (buffer-substring (match-beginning 1) (match-end 1)))
      (goto-char (match-end 0))
      (skip-chars-forward " \t"))
    ;; pause:
    (if (not (looking-at "[0-9]+\.[0-9]+"))
	()
      (setq lex-pause (buffer-substring (match-beginning 0) (match-end 0)))
      (goto-char (match-end 0))
      (skip-chars-forward " \t"))
    ;; start:
    (if (not (looking-at "[0-9]+\.[0-9]+"))
	()
      (setq lex-start (buffer-substring (match-beginning 0) (match-end 0)))
      (goto-char (match-end 0))
      (skip-chars-forward " \t"))
    ;; end and duration, maybe; this can't match a key sequence,
    ;; because of the intermediate whitespace: 
    (if (not (looking-at "\\([0-9]+\.[0-9]+\\)[ \t]+\\([0-9]+\.[0-9]+\\)"))
	()
      (setq lex-end (buffer-substring (match-beginning 1) (match-end 1)))
      (setq lex-duration (buffer-substring (match-beginning 2) (match-end 2)))
      (goto-char (match-end 0))
      (skip-chars-forward " \t"))
    (if (not (looking-at "\\(.*\\)\n"))
	()
      (setq lex-keys (buffer-substring (match-beginning 1) (match-end 1))))))

(defun log-floatify (int)
  (format "%s.%s" (/ int 10) (% int 10)))

;; 12-23-93 - with hundredths, big numbers were overflowing:
;(defun log-intify (string-float)
;  "STRING-FLOAT is decimal seconds, with 1 or more places after the
;decimal.   Returns integer tenths of a second.  For use with
;log-float-and-round-num."
;  (let* ((decimal (string-match "\\." string-float))
;         (decimal-places (1- (- (length string-float) decimal)))
;         (decimal-part (string-to-int (substring string-float (1+ decimal)))))
;    (+ (* 100
;          (string-to-int (substring string-float 0 decimal)))
;       (cond ((= decimal-places 1)
;              (* 10 decimal-part))
;             ((= decimal-places 2)
;              decimal-part)
;             ((= decimal-places 3)
;              (round decimal-part 10))
;             (t                         ; vaguely appropriate
;              0)))))

(defun log-intify (string-float &optional significance)
  "STRING-FLOAT is decimal seconds, with 1 or more places after the
decimal.   Returns integer tenths of a second.  For use with
log-float-and-round-num.  See also log-integer-second." 
  (let* ((decimal (string-match "\\." string-float))
	 (decimal-places (1- (- (length string-float) decimal)))
	 (decimal-part (string-to-int (substring string-float (1+ decimal)))))
    (+ (* 10				; 12-23-93 - 100 to 10
	  (string-to-int (substring string-float 0 decimal)))
       (round decimal-part (log-raise 10 (1- decimal-places))))))

(defun log-raise (base exp)
  "Raise BASE to non-negative EXP."
  (let ((r 1))
    (while (plusp exp)
      (setq r (* r base))
      (decf exp))
    r))

;; 12-23-93 - with hundredths, big numbers were overflowing:
;(defun log-float-and-round-num (int)
;  "INT is integer tenths of a second.   Returns a string float, in
;seconds with one decimal place.  For use on log-initify-ed numbers." 
;  (let* ((whole (/ int 100))
;         (part (round (% int 100) 10)))
;    (if (= part 10)
;        (format "%s.0" (1+ whole))
;      (format "%s.%s" whole part))))

(defun log-float-and-round-num (int)
  "INT is integer tenths of a second.   Returns a string float, in
seconds with one decimal place.  For use on log-initify-ed numbers." 
  (format "%s.%s" (/ int 10) (% int 10)))

(defun log-spread-out ()
  "On data generated by log-time-episodes, spreads records out
on a time-line, one second per line.  Output to *log-spread-out*."
  (interactive)
  (let (line
	lex-id				; fields in line
	lex-pause
	lex-start
	lex-end
	lex-duration
	lex-keys
	start
	(end nil)
	(output-buf (get-buffer-create "*log-spread-out*"))
	)
    (save-excursion
      (set-buffer output-buf)
      (erase-buffer))
    (save-excursion
      (beginning-of-buffer)		; find 1st non-blank line
      (skip-chars-forward " \t\n")
      (log-lex-episode-line)
      (setq line (log-integer-second lex-start))
      (while (not (eobp))
	(log-lex-episode-line)
	(save-excursion
	  (set-buffer output-buf)
	  (setq start (log-integer-second lex-start))
	  (while (< line start)
	    (insert (format "%8s\n" line))
	    (incf line))
	  (insert (format "%8s\t%8s\t%8s\t%8s"
			  line lex-id lex-pause lex-start))
	  (if (not lex-end)
	      (insert (format "\t%8s\t%8s\t%s\n" "" "" lex-keys))
	    (setq end (log-integer-second lex-end))
	    (if (>= line end)
		(insert (format "\t%8s\t%8s\t%s\n" lex-end lex-duration lex-keys))
	      (insert (format "\t%8s\t%8s\t%s\n" "" "" lex-keys))
	      (incf line)
	      (while (< line end)
		(insert (format "%8s\n" line))
		(incf line))
	      (insert (format "%8s\t%8s\t%8s\t%8s\t%8s\t%8s\t%s\n"
			      line
			      "" "" ""
			      lex-end
			      lex-duration
			      ""))))
	  (incf line))
	(forward-line 1)))
    (pop-to-buffer output-buf)
    (goto-char (point-min))))

(defun log-integer-second (string-float)
  "Returns rounded whole part of STRING-FLOAT.  Based on log-intify."
  (let* ((decimal (string-match "\\." string-float))
	 (decimal-places (1- (- (length string-float) decimal)))
	 (decimal-part (string-to-int (substring string-float (1+ decimal)))))
    (+ (string-to-int (substring string-float 0 decimal))
       (round decimal-part (log-raise 10 decimal-places)))))

(defvar *log-time-episodes-interval* 100
  "*Used by log-time-episodes to create new episodes.")

;; 8-1-93 -
(defun log-time-episodes ()
  "Like log-meta-command-episodes, but also breaks a string when
the pause between keystrokes is longer than 1 second.
Prints episode start/end times and inter-episode intervals and episode
durations, but leaves out command names and arguments.  First output
field is episode start time in original format, for tracing to other
representations.  Output to *log-time-episodes*."   
  (interactive)
  (let ((previous-was-episode nil)
	previous-was-kill
	(current-is-kill nil)
	(current-timestamp 0)
	(previous-timestamp 0)
	(episode-beginning 0)
	episode-len	
	lex-line-number			; globals used by log-lex-line
	lex-timestamp			; as integer tenths-of-seconds
	lex-keymap
	lex-keys
	(output-buf (get-buffer-create "*log-time-episodes*")))
    (save-excursion
      (set-buffer output-buf)
      (erase-buffer))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
	(log-lex-line)			; moves point!
	(if lex-timestamp		; not an add-one kill/yank
	    (save-excursion
	      (set-buffer output-buf)
	      (setq previous-timestamp current-timestamp)
	      ;; 11-8-93 - need the whole thing, because intervals
	      ;; between episodes get long (cf "minusp")
	      (setq current-timestamp (log-intify lex-timestamp))
	      (setq pause (- current-timestamp previous-timestamp))
	      (if (minusp pause)
		  ;; then one of the eight-digit begin-end
		  ;; brackets have wrapped (cf "minusp"):
		  (setq pause (+ pause 1000000))) 
	      (setq previous-was-kill current-is-kill)
	      (setq current-is-kill (log-line-is-kill))
	      ;; if not a meta-command, and no big pause, and not
	      ;; other things, append to episode:
	      (if (and lex-keymap
		       ;; HERE:
		       ;;(> *log-time-episodes-interval* pause)
		       (> 10 pause)	; ie, > 1 sec
		       ;; not 1st of a sequence of kills:
		       (or previous-was-kill (not current-is-kill))
		       ;; not first command after a kill:
		       (not (and previous-was-kill (not current-is-kill)))
		       )
		  (progn
		    (insert lex-keys)
		    (setq previous-was-episode t))
		
		;; ELSE previous episode ends and next one begins.
		
		(skip-chars-backward "^ \t") ; backward over inserted keys
		;; if previous episode had only one keystroke, insert NA for
		;; end timestamp, else end timestamp is that of last key in
		;; episode: 
		(if (not previous-was-episode)
		    (insert (format "%6s\t%6s\t" "" ""))
		  (setq previous-was-episode nil)
		  (setq episode-len (- previous-timestamp episode-beginning))
		  (if (minusp episode-len)
		      ;; then one of the seven-digit begin-end
		      ;; brackets have wrapped (cf "minusp"):
		      (setq episode-len (+ episode-len 1000000)))
		  (insert (format "%6s\t%6s\t"
				  (log-float-and-round-num previous-timestamp)
				  (log-float-and-round-num episode-len))))
		(setq episode-beginning current-timestamp)
		(end-of-line)
		(insert "\n")
		
		;; insert episode start time in original format, for
		;; mapping; preceding pause, episode start time, and first
		;; keys in episode:
		(insert (format "%s\t%6s\t%6s\t%s"
				lex-timestamp
				(if (not (zerop previous-timestamp))
				    (log-float-and-round-num pause)
				  "0.0")
				(log-float-and-round-num current-timestamp)
				lex-keys)))))
	(forward-line 1)))
    (pop-to-buffer output-buf)
    (skip-chars-backward "^ \t")
    (insert (format "%6s\t%6s\t" "" ""))
    (end-of-buffer)
    (insert "\n")
    (goto-char (point-min))))

;; HERE: fix this to save excursion and move to beginning of line.
;; sets:
;;   lex-line-number (string or nil)
;;   lex-timestamp (string representing integer milliseconds)
;;   lex-keymap (string or nil)
;;   lex-keys (string)
;; assumes point is at bol, and leaves point wherever.
(defun log-lex-line ()
  (setq lex-line-number nil
	lex-timestamp nil
	lex-keymap nil
	lex-keys nil)
  (skip-chars-forward " \t")		; skip whitespace befor field 1
  (if (not (looking-at "\\([0-9]+\\)[ \t]"))	; line number?
      ()
    (setq lex-line-number (buffer-substring (match-beginning 1) (match-end 1)))
    (goto-char (match-end 0))
    (skip-chars-forward " \t"))
  (if (not (looking-at "[0-9]+\.[0-9]+")) ; timestamp?
      ()
    (setq lex-timestamp (buffer-substring (match-beginning 0)
					  (match-end 0)))
    (goto-char (match-end 0))
    (skip-chars-forward " \t")
    ;; remaining fields non-nil are contingent on there being a timestamp:
    (if (not (looking-at "\[[A-Z]+\\??\]"))	; keymap?
	()
      (setq lex-keymap (buffer-substring (match-beginning 0) (match-end 0)))
      (goto-char (match-end 0)))
    (re-search-forward "\\([^ \t]*\\)")	; match keys (assumes trailing ws)
    (setq lex-keys (buffer-substring (match-beginning 1) (match-end 1)))
    ;; don't bother with command name or args for now, but it's 
    ;; probably simple enough.
    ))

;; t if the current log line is part of a kill or yank.
;; needs the line to be lexed, doesn't care about point.
(defun log-line-is-kill ()
  (or (not lex-timestamp)
      (string-match "\\^\\(K\\|W\\|Y\\)" lex-keys)))

;; 6-20-93 - 
(defun log-meta-command-episodes ()
  "Destructively filters a keystroke log buffer to organize keystrokes
into epidodes.   Lines for commands invoked via C-x or M-x
(meta-commands, loosely), as well as kills and yanks, are left intact.
For other lines, only the keys get through:  
----
     1	6204.696  ^X^F       find-file                      
     2	6207.462  [G]f       self-insert-command            
     3	6207.696  [G]r       self-insert-command            
-- becomes:
     1	6204.696  ^X^F       find-file                      
fr
----
Provides high compression but a coarse view of the data.  Good for
finding strings typed by the user.  See also log-collapse-repeats and
log-time-episodes."
  (interactive)
  (save-excursion
    (let ((catenating nil)
	  previous-was-kill
	  (current-is-kill nil))
      (beginning-of-buffer)
      ;; delete yank/kill lines after the first (which probably won't
      ;; have NNNN.NNN timestamps): 
      (delete-non-matching-lines "^.*[0-9][0-9][0-9][0-9]\.[0-9][0-9][0-9].*")
      ;; search for log.el's keymap format, eg, "[G]" for global map, because
      ;; ^X and ESC command sequences don't have this.  exempt the
      ;; first of sequence of kills, wipes, and yanks. 
      (while (not (eobp))
	(setq previous-was-kill current-is-kill)
	;; HERE: replace with a call to log-line-is-kill:
	(setq current-is-kill  (string-match "\\^\\(K\\|W\\|Y\\)"
					  (buffer-substring
					   ;; WARNING: there's a magic
					   ;; number 30 here; search
					   ;; for "effects column". 
					   (point) (+ (point) 30))))
	;; HERE: does this get "[D?]"?  use log-lex-line?
	(if (and (looking-at ".*\[[A-Z]*\]\\([^ ]*\\).*\n") ; keymap?
		 (or previous-was-kill
		     (not current-is-kill)))
	    ;; delete all of the log line except the keystrokes
	    (progn
	      (replace-match "\\1")
	      (setq catenating t))
	  ;; ELSE leave the log line intact
	  (if catenating		; end current string
	      (progn
		(setq catenating nil)
		(insert "\n")))
	  (forward-line 1))))))


(defun log-collapse-repeats ()
  "Collapses sequences of 3 or more repeats of selected character
pairs.  Eg,^N^N^N^N______^A^A becomes ^N4... __3... ^A^A.  (Note the
spaces, which can't otherwise get into the data.)  Collapses ^A-^Z,
^? and __."
  (interactive)
  (save-excursion
    (let ((count 0))
      (beginning-of-buffer)
      (while (not (eobp))
	(cond ((and (zerop count)
		    (looking-at "\\(\\^[A-Z?]\\|__\\)\\1\\1")) ; three pair
	       (setq count 3)
	       (forward-char 2)
	       (delete-char 2))		; delete middle, look at last pair
	      ((and (>= count 3)
		    (looking-at "\\(\\^[A-Z?]\\|__\\)\\1")) ; another beyond
	       (incf count)
	       (delete-char 2))
	      ((and (>= count 3)
		    (not (looking-at "\\(\\^[A-Z?]\\|__\\)\\1"))) ; end
	       (delete-char 2)		; empty the pair queue
	       (insert (format "%s... " count 1))
	       (setq count 0))
	      (t
	       (forward-char 1)))))))

;; other utilities:

;; 10-1-93 - 
;; if the buffer hasn't been saved in some other way, and we might
;; care about it, save its contents.  temp buffers may not be marked
;; as modified.
(defun log-temp-buffer (buf &optional is-so-modified)
  (if (bufferp buf)
      (save-excursion
	(set-buffer buf)
	(if (or (not (or (buffer-modified-p) is-so-modified))
		(buffer-file-name buf)
		(eq buf (get-buffer log-keys-buffer-name))
		(eq buf (get-buffer log-temp-buffer-name))
		;; can we get the minibuffer directly?
		(eq buf (window-buffer (minibuffer-window))) )
	    ()
	  ;; grab new output in this buffer:
	  (let* ((separator
		  (format "logged<>%s<>%s" (buffer-name buf) log-timestamp))
		 (log-buf (get-buffer log-temp-buffer-name)))
	    (save-excursion
	      ;; if the accumulation buffer's died, set up another:
	      (if (not log-buf)
		  (setq log-buf (log-accumulation-file
				 "temp-buffers" log-temp-buffer-name)))
	      (set-buffer log-buf)
	      (goto-char (point-max))
	      (if (not (bolp)) (insert "\n"))
	      (insert (concat separator "\n"))
	      (insert-buffer buf))
	    (set-buffer-modified-p nil) ; blech; 12-22-93 - HERE: why do this?
	    (log-command-in-process-buffer
	     (list 'lambda nil (list 'insert separator))))))))

(defun log-load-emergency-file ()
  "Load emergency.el in the current directory (sample code below).
Bound (one hopes) to C-x4l.  Use for debugging if keys get wedged, or,
say, to load a file of commands to write all buffers.
Sample code for emergency .el:  ;; extra space before .el??  -fer
  ;; --- save contents of *cmushell* buffer:
  ;;(save-excursion
  ;;(set-buffer \"*cmushell*\")
  ;;(auto-save-mode 1)
  ;;(do-auto-save))
  ;; ...then exit emacs:
  ;;(kill-emacs t)
  ;; --- or, figure out where the problem lies:
  ;;(setq debug-on-error t)"
  (interactive)
  (funcall log-old-load (expand-file-name "emergency.el") nil nil t))

;; 9-28-93 - quietly save accumulation buffers, w/o backups:
(defun log-save-accumulation-buffers ()
  ;; save accumulated temp buffer output:
  (if (bufferp (get-buffer log-temp-buffer-name))
      (save-excursion
	(set-buffer (get-buffer log-temp-buffer-name))
	(save-buffer 0)))
  ;; save process buffers:
  (log-save-process-buffers))
  
(defun log-save-process-buffers ()
  (let ((l (process-list))
        process)
    (while l
      (setq process (car l))
      (if (and (process-buffer process)	; seems nil can have a file-name
	       (buffer-file-name (process-buffer process)))
	  (save-excursion
	    (set-buffer (process-buffer process))
	    (save-buffer 0)))
      (setq l (cdr l)))))

(defun log-screen ()
  (concat "screen<>" (log-screen-info)))

;; 9-28-93 - return line-number/top-line:bottom-line.  from what-line.
(defun log-screen-info ()
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)		; don't be at eol
      (let* ((top-line (1+ (count-lines 1 (window-start
					   (selected-window))))))
	(format "%d:%d-%d"
		(+ top-line
		   (count-lines (window-start (selected-window)) (point)))
		top-line
		(+ top-line (window-height) -2))))))

(defun log-current-buffer ()
  (` (lambda () (insert (, (concat "now-in<>"
				   (buffer-name (current-buffer))
				   "<>"
				   (log-screen-info)))))))

(defun log-insert-kill ()
  (let ((start (point))
	(indent-tabs-mode nil))		; indent with spaces
    (insert (car kill-ring-yank-pointer))
    ;; indent all of a multi-line kill to start at effects column:
    (indent-rigidly (progn (save-excursion
			     (goto-char start) (forward-line) (point)))
		    (point)
		    52)))

;; functions to redefine functions and toggle back and forth:

(defvar log-redefinitions nil)
(defun log-redefine (command)
  (if (not (memq command log-redefinitions))
      (setq log-redefinitions (cons command log-redefinitions)))
  (let* ((name (symbol-name command))
	 (old-command (intern (concat "log-old-" name)))
	 (new-command (intern (concat "log-new-" name)))
	 (sf-cur (symbol-function command))
	 (sf-new (eval new-command)))	; lamda-valued defvar
    (if (eq sf-cur sf-new)
	nil
      ;; ELSE clobbered, or not yet redefined
      (set old-command sf-cur)
      (fset command sf-new)
      command)))

;; 10-4-93 - fset a function to its original value.
(defun log-re-redefine (command)
  (fset command (eval (car (read-from-string
			    (concat "log-old-" (symbol-name command)))))))

(defun log-do-auto-save ()		; would a macro be faster?  
  "Save buffer in time-stamped file name, then clear buffer."
  (if (bufferp (get-buffer log-keys-buffer-name))
      (let ((save-buf (find-file-noselect
		       (concat *log-data-directory*
			       "Log.keys."
			       (get-time-string))))
	    (log-buf (get-buffer log-keys-buffer-name))
	    (require-final-newline t))	; for catenating log files
	(save-excursion
	  (set-buffer save-buf)
	  (insert-buffer log-buf)
	  (save-buffer)
          (if log-compress! ;; -fer
              (call-process "compress" nil 0 nil (buffer-name save-buf))))
	(kill-buffer save-buf)
	(save-excursion
	  (set-buffer log-buf)
	  (erase-buffer)))))

;; date/timestamp for the log file:

(defun get-time-string ()
  (let* ((time-string (current-time-string))
	 (gronk (substring time-string 8 9))) ; maybe blank
    (concat
     (substring time-string 0 3) ; day
     "-" (substring time-string 4 7) ; month
     "-" (if (string-equal gronk " ")
	     (substring time-string 9 10) ; date, 1<10
	   (substring time-string 8 10)) ; date, 9<...
     "-" (substring time-string 11 19) ; hh:mm:ss
     "-" (substring time-string 20 24) ; year
     )))

;; 6-25-93 - need to load just to get access to parsing files:
;; (log-initialize)
(if log-initialized
    (message (concat "Already logging, to "
		     *log-data-directory*))
  (message "To start logging, M-x log-initialize."))
(sleep-for 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; still todo:

;;;;
;;;;     why does comint-bol (^A) crap out?

;;;;   the read-char definition means that ^S (and ^R) get logged
;;;;     twice [G]^R...isearch-backward [R]^R...read-char                    
;;;;   make documentation current
;;;;   make sure there's no version control for 'cuum buffers
;;;;   can we compress trace files?
;;;;   log modified temp buffers when leaving them?
;;;;   have a test to see if current buffer changes, output name of new buffer 
;;;;   do something to capture output of temp and (mainly) process buffers
;;;;     [8-16-93] in the intermim, just write buffer to a file; then
;;;;       killing the buffer will prompt emacs to prompt for a save.
;;;;     [9-26-93] for process buffers (ie shell), need to make sure that
;;;;       killing the buffer doesn't hurt.
;;;;   decide what to do about, say ^X <pause> b, which registers
;;;;     only the time of the "b"
;;;; 
;;;; old todo:
;;;;   Xbe smarter about the mouse [12-30-93, after s12]
;;;;     Xoutput cursor position, command, etc  [see below]
;;;;     ?parse it better; add to log-collapse? [?]
;;;;     Xget meaningful output - line & column numbers would do
;;;;       [1-14-94 - outputs args to mouse command, incl.
;;;;        (xpos ypos) in character OFFSET from top left of SCREEN]
;;;;     Xget x-cut-text wrapped in the keymap 
;;;;     Xget the lisp code wrapped - x-cut-text, x-get-cut-buffer [an arg]
;;;;     Xmaybe don't wrap x-flush-mouse-queue [right]
;;;;   Xdiscovered from s12's run that mouse events show up as
;;;;     Xtwin x-flush-mouse-queue's--why? [12-22-93]
;;;;   Xfigure out why characters typed to i-search don't show (8-2-93)
;;;;   Xmake up/down movement keys put out line and column numbers? [don't need]
;;;;   Xstore diffs in backup files (see "overwrite" in K&P) [compress backups]
;;;;   Xwrap with-output-to-temp-buffer
;;;;   Xcompress keystroke logs on the fly ("call-process...")
;;;;   Xwrite temp buffer log when exiting
;;;;   Xwrap set-process-filter, for procs that don't use a process buffer
;;;;   Xwrite filter-output buffer like i do keys buffer
;;;;   Xchange keystroke logging to use a process filter, to get timestamps
;;;;   Xmake buffer-logging modular
;;;;   Xwrap set-process-buffer, in case buf changes after start-proces
;;;;   Xhave character deletes put out what they're deleting (time*.s7.ann*.txt:7858; 9-14-93)
;;;;   Xget ^W to print its arguments (if there's really a place it doesn't)
;;;;      [9-27-93 - s7's keystrokes are ok; might have been fooled by \n.
;;;;   Xredefine define-key and its relations
;;;;   Xwalk control-x, esc, and local (?) maps
;;;;   Xwrite the log file
;;;;   Xinstall the coarse log [wrap yanks, kills, other-window]
;;;;   Xdump a dribble like open-dribble, so we can compare to make sure
;;;;     we got all keystrokes [got most of them,i think]
;;;;   Xdump the original keymaps, for getting the commands back [now
;;;;     print commands]
;;;;   Xfset-ed macros?
;;;;   Xminibuffer completion map?
;;;;   Xwrap use-local-map? [put a hook on find-file-hooks] [wrapped
;;;;     other-window with log-modify-local-maps; sooner or later, will
;;;;     get all of them...]
;;;;   Xaccessible-keymaps, to get all maps
;;;;   Xmake timer portable to other systems
;;;;   Xtest with completion stuff loaded (first!)
;;;;   Xget rid of nfound in timer.c (save some time)
;;;;   
;;;; History:

;;;; 12-2-94 - modified to work in 19, sorta.  Not as clean or commented, 
;;;;   but not bad.  Useful for undergraduate practicals.
;;;;
;;;; 1-14-94 - smarter mouse behavior.  x-flush-mouse-queue was it,
;;;;   getting called on both up and down clicks.  now this and
;;;;   x-mouse-ignore aren't logged, and other mouse functions are,
;;;;   along with their args.  seems mouse functions aren't
;;;;   interactive, and get args directly when they're dispatched, so
;;;;   changed log-call to deal with args when they're there.
;;;;
;;;; 12-23-93 - timestamps (in log-time-episodes) were overflowing;
;;;;   changed from 100s of a second to 10s.
;;;;
;;;; 12-22-93 - display-time is a process whose buffer is whatever
;;;;   buffer you're in.  this confused the code in log-initialize
;;;;   that set buffer-file-names for existing process buffers,
;;;;   causing it to change the name of a text buffer.  fixed that and
;;;;   some other contingencies.  
;;;;   
;;;; 12-20-93 - "(accept-process-output log-timer-process 1)"; "1" was
;;;;    wrong; how'd it get there?
;;;;
;;;; 11-9-93 - log-time-episodes and log-spread-out now take bigger
;;;;   timestamps (5+3 digits).  episode brackets are 5+1, the 5
;;;;   because a very long interval (two emacs, in different windows)
;;;;   would cause aliasing, the 1 because that's all that's useful in
;;;;   spread-out format.  floats now represented internally as
;;;;   hundredth-second ints.
;;;; 
;;;; 10-13-93 - key logs written with require-final-newline t.
;;;; 
;;;; 10-4-93 - popper was redefining pop-to-buffer, causing a binding
;;;;   loop; log-new-load now resets most functions before calling
;;;;   load, then restores the redefinitions.  removed "list*" (see
;;;;   below).  log.el loads into a bare editor, and my .emacs (which
;;;;   loads popper) loads after.  compress snapshots.
;;;;
;;;;     note: log.el breaks substitute-command-keys (because keys are all
;;;;     lambdas), which has a byte-code.
;;;;
;;;; 10-3-93 - set hooks better; tried to set temp-buffer-show-hook.
;;;;   replaced dolist; can't figure out where to get it, or where i
;;;;   get backquote.
;;;; 
;;;; 10-2-93 - keystroke files get compressed when written.  filenames
;;;;   are better.  wrote some code and fixed some bugs from within an
;;;;   editor that was logging.
;;;; 
;;;; 10-1-93 - now filters output of timer, and blocks on read to
;;;;   synchronize; maybe this will prevent any future instances of
;;;;   things being written in the log out ofkilter, and we can now
;;;;   get the timestamp by itself.  process buffers and filters now
;;;;   log process output.  
;;;; 
;;;; 9-28-93 - logs chars deleted, forwards or backwards, and screen
;;;;   info for scrolls and buffer leaps (#6 and #7, in updated list);
;;;;   quietly visits a timestamped file for each new or existing
;;;;   process buffer, quietly writing it when buffers are killed or
;;;;   saved (#2); prints characters entered to isearch (but won't
;;;;   get other read-char args unless the .el files for the functions
;;;;   are reloaded).
;;;;
;;;; 9-27-93 - priorities:
;;;;      X1. preimages - copy the original; does "~" do it?
;;;;         no: user may start in the middle of a session
;;;;      X2. save process buffers
;;;;      3. accumulate output of temp buffers
;;;;      4. isearch - record arguments
;;;;      X5. checkpoints a diff at every save
;;;;         . wrap auto-save to record a timestamp with the file
;;;;         attribute representing time that i see with an ls; set
;;;;         delete-auto-save-files (215) to nil
;;;;         . change save-buffer to call backup-buffer every save
;;;;         . change backup-buffer to timestamp filenames
;;;;         . set make-backup-files t
;;;;         . version-control (variable)
;;;;         . save-some-buffers in log-initialize, if that helps
;;;;      X6. scrolls put out line and column numbers
;;;;         . scroll-up, scroll-down, scroll-other-window all separate, C
;;;;      X7. deletes put out characters
;;;;      ?8. ^N, ^P put out line numbers [too slow?  'sides, have scrolls]
;;;;
;;;;      todo:
;;;;        copy all visited buffers on exit (?)
;;;;      emacs things to look at:
;;;;         checkpointing - "~" (#1)
;;;;         associating process buffers with file and auto-saving (#2)
;;;;         with-output-to-temp-buffer (#3)
;;;;         redefining read-char (#4)
;;;;         redefining isearch (#5)
;;;;
;;;;         append-to-file
;;;;         copy-file
;;;;         kept-old/new-versions (213)
;;;;     [initial version:]
;;;;      1. preimages - copy the original; does "~" do it?
;;;;         no: user may start in the middle of a session
;;;;      2. save process buffers
;;;;      3. accumulate output of temp buffers
;;;;      4. isearch - record arguments
;;;;      5. checkpoints a diff at every save
;;;;         . wrap auto-save to record a timestamp with the file
;;;;         attribute representing time that i see with an ls; set
;;;;         delete-auto-save-files (215) to nil
;;;;         . change save-buffer to call backup-buffer every save
;;;;         . change backup-buffer to timestamp filenames
;;;;         . set make-backup-files t
;;;;         . version-control (variable)
;;;;         . save-some-buffers in log-initialize, if that helps
;;;;
;;;;   did 1 and 5, by redefining save-buffer and
;;;;   find-backup-file-name.  log now points to backups, up to a
;;;;   hundred of which are made, every save.  saves all buffers on
;;;;   initialization after redefinition, to get preimages of buffers
;;;;   modified but not written at log-initialization.
;;;;   
;;;;   the preimage is the set of first backups for each file after
;;;;   log-initialization.
;;;; 
;;;; 9-26-93 - wrap find-file (and write-file?) to squirrel away the
;;;;   pre-image; also for reading files; and get snapshots when files
;;;;   are written!  what if source files are huge, changes small, and
;;;;   writes frequent?  store diffs, like RCS?
;;;; 
;;;; 6-26-93 - Fixed up the keymap-walking functions to send warnings
;;;;   to a buffer instead of generating an error.  Also added fix
;;;;   that FER's sent me on 14 May.
;;;;
;;;;   Here's something odd (see ~/data/s10*/keystroke*/originals/).
;;;;   I suppose some buffer somewhere got clobbered, but it's beyond me.
;;;;
;;;;	9839.355  [G]^P      previous-line                  .52  ;; "." marks col 52, which was eol
;;;;	9842.996  [G]^P      previou[G]^P                   .52
;;;;	9843.496  previous-line [G]^P                          .55
;;;;	9843.527  previous-line [G]^P                          .55
;;;;	9843.574  previous-line [G]^P                          .55
;;;;	9843.605  previous-line [G]^P                          .55
;;;;	9843.636  previous-line [G]^P                          .55
;;;;	...
;;;;	9848.386  next-line  [G]^N                          .52  ;; "[" (3') begins where (3) did
;;;;	...
;;;;	9878.308  previous-line ^[>                            .55
;;;;	...
;;;;	9882.183  self-insert-command [D?]^M                         .61 ;; diff (9) is length of commands
;;;;	9883.511  sde-return [G]g                           .52
;;;; 
;;;; 6-20-93 - Began adding functions to parse the logs; may never get
;;;;   beyond log-parse-1.
;;;;
;;;; 3-30-93 - added log-load-emergency-file and C-x4l binding, after
;;;;   losing gary's shell binding when the keybindings wedged.
;;;;   tim freeman pointed out that a back door would have been
;;;;   useful.  added test in log-keystroke ("3-30-93") that fixed the
;;;;   problem when exit-emacs queried about killing remaining
;;;;   processes; didn't seem to slow things down. 
;;;; 
;;;; 3-5-93 - ways to segment the log:
;;;;   . version-control, on the file visited by the log buffer.  but
;;;;   version-control isn't buffer-local, so it will be on for all
;;;;   files being edited.
;;;;   . do my own version-control; write the file, rename it to
;;;;   something that includes the version-control counter.  but that
;;;;   entails visiting a file, which means that one can't use the
;;;;   nice buffer name that begins with a leading blank.
;;;;
;;;; . Command arguments seem to reach the process buffer before the
;;;; output process, even though the commands that generate them are
;;;; called after the process is sent its string.  And
;;;; accept-process-output after the process-send-string didn't help.
;;;; And when using the debugger to trace calls to log-call (first)
;;;; and log-keystroke (second), the arguments and process output
;;;; reach the process buffer in the right order.  Some race
;;;; condition?
;;;; 
;;;;   [ 11-18-92 - fixed by moving the process marker just before
;;;;   sending the new process string (in log-keystroke) rather than
;;;;   just after inserting command output in the process buffer
;;;;   (log-comman*).  execution order is now (1) log keystroke; (2)
;;;;   command output; (3) process output from #1 arrives at process
;;;;   mark, before command output (4) (next keystroke) process mark
;;;;   moved past command output.  #4 was happening after #2, meaning
;;;;   that #3 was appearing after the command output ]
;;;;
;;;; . Loses the logging of command arguments if packages loaded on
;;;; top of this re-defun the commands.  Popper.el is irresponsible in
;;;; this regard (it should define new functions that call the old
;;;; symbol-function, rather than clobber it with a defun).
;;;;
;;;;   [ 11-22-92 - keep track of all redefined functions, then
;;;;   redefine load to map log-redefine across the list.  made
;;;;   log-redefine "reentrant". ]
;;;;
;;;; 11-28-92 - indenting copy-as-kills is meant to improve
;;;;   filterability.  in particular, it means that keystrokes can be
;;;;   counted by filtering out lines that lead with spaces.
;;;;   important if one happens to yank mulitple lines from the log
;;;;   buffer... 

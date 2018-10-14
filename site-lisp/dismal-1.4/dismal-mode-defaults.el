;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : dismal-mode-defaults.el
;;;; Author          : Frank Ritter
;;;; Created On      : Thu Feb 27 15:36:34 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Aug 30 1992
;;;; Update Count    : 7
;;;; 
;;;; PURPOSE
;;;;	i. 	Requires not otherwise covered
;;;;	ii.	Set up the directory for dismal
;;;;	iii.	Set up the directory for dismal- OS2 section
;;;;	iv.	Actually set the load-path
;;;;	v.	Set up autoloads for dismal
;;;; TABLE OF CONTENTS
;;;;	N. 	Dead code
;;;; 
;;;; Copyright 1991, Carnegie Mellon University.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;;	i. 	Requires not otherwise covered
;;;

(if (string= emacs-version "^18")
    (error "This Dismal does not work with Emacs18.  Get later Emacs\
 or earlier dismal!"))

;; The Emacs Common-lisp look-alike package
;; Every site should have this.  Email us if you don't.
;; Put here because it doesn't fit in the make otherwise.
(require 'cl)


;;;
;;;	ii.	Set up the directory for dismal- function call
;;;

;; Fabulous hack to figure out where this file is located and add that
;; directory to the load-path.  This allows users simply to issue a
;;      (load "/foo/bar/soar")
;; and automatically get the /foo/bar directory added to their load-path if
;; it's not in load-path already.  Algorithm: look at where this file is
;; being loaded from, look at load-path, and if the current directory isn't
;; on load-path, add it.
;;
;; dismal-directory is based on original code from Andy Norman:
;;
;;   From: Andy Norman <ange@hplb.hpl.hp.com>
;;   To: hucka@engin.umich.edu
;;   Subject: Re: How to get path info during load? 
;;   Date: Tue, 28 Jan 92 10:40:28 GMT

;; if we assume a make using a makefile in the directory, this is not
;; necessary.  But can we assume it?  30-Aug-95 -FER

(defun dismal-directory ()
  "Guess the directory of the file currently being loaded, or return nil."
  (let* ((buf (get-buffer-create " *backtrace*"))
	 (standard-output buf)
	 file)
    (condition-case error
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (backtrace)
	  (goto-char (point-min))
	  (if (re-search-forward "load(\"\\([^\"]+\\)\"" nil t)
	      (setq file (buffer-substring (match-beginning 1) (match-end 1))))
	  (kill-buffer buf))
      (error nil))
    (if (and file (file-name-directory file))
	(directory-file-name (file-name-directory (expand-file-name file))))))
   

;;;
;;;	iii.	Set up the directory for dismal- OS2 section
;;;
;;; 'beginning of delete me for os2 patch'


;; OS2 cant use the hack above, so put this code into to get the user to do 
;; it the old fashioned way by hand.
(if (eq system-type 'emx)
    (progn
      (switch-to-buffer (get-buffer-create "*OS2-ERROR*"))
      (erase-buffer)
      (insert
"Normally dismal can set up its installed directory by itself.
You are running under OS/2, and will have to set the directory by hand.
\nHere's how.
  * Open up the file dismal-mode-defaults.el
  * Go to where you see the line (setq dismal-directory (dismal-directory))
    (not this error message which appears first!)
  * Replace the (dismal-directory) with the absolute path name, 
    not ending with a slash or other end of directory 
    marker (e.g., /psych/dismal/new)
  * Delete the section marked 'delete me for os2 patch'
  * Restart.\n")
    (error "Read *OS2-ERROR*")) )

;; 'end of delete me for os2 patch'

;;;
;;;	iv.	Actually set the load-path
;;;

(setq dismal-directory (dismal-directory))

(if (not (member dismal-directory load-path))
    (setq load-path
          (cons dismal-directory 
                load-path)))


;;;
;;;	v.	Set up autoloads for dismal
;;;

;; This lets any existing dismal file (with any extension) open in
;; dismal-mode
(autoload 'dismal-mode (concat dismal-directory "/dismal")
   "The dismal code." t)

;; This make any new file ending in .dis get opened up into dismal-mode
(if (not (assoc "\\.dis$" auto-mode-alist))
    (set-default 'auto-mode-alist
             (append (mapcar '(lambda (x) (cons x 'dismal-mode))
                             '("\\.dis$"))
                 auto-mode-alist)))


;;;
;;;	N. 	Dead code
;;;

;; This is all done by the automatic directory setting code above.
;;

;;; This code should be uncommented and put into your .emacs file.  It
;;; will start up dismal mode when you open a dismal file.
;; This tells dismal where it's to load its associated files from.
;; (setq dismal-directory
;;      "/psyc/lang/soar/emacs/utils/dismal/new")
;;
;; (defvar dismal-directory
;;  "/afs/cs/project/soar/member/dismal/new/dismal/0.6/"
;;  "Directory where dismal lives.  Automatically loaded to load-path.")
;;
;; (autoload 'dismal-mode
;;    ;;; the path to dismal goes here:
;;    "/afs/cs/project/soar/member/ritter/dismal/0.6"
;;    "The dismal code." t)
;; 
;; ;; This make any new file ending in .dis get opened up into dismal-mode
;; (if (not (assoc "\\.dis$" auto-mode-alist))
;;     (set-default 'auto-mode-alist
;;              (append (mapcar '(lambda (x) (cons x 'dismal-mode))
;;                              '("\\.dis$"))
;;                  auto-mode-alist)))

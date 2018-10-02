;;; x-face-e21.el --- X-Face utilities for Emacs 21 (and possibly 22)

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2007 Katsumi Yamaoka

;; Author: Katsumi Yamaoka  <yamaoka@jpl.org>
;; Created: 2000/06/16
;; Revised: 2007/03/06
;; Keywords: x-face, cmail, emh, gnus, mew, mh-e, rmail, vm, wanderlust, bbdb

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Using x-face-e21 to display X-Faces in the Gnus article buffer is
;; not recommended.  There is still the way to do that, though.

;; The Elisp based uncompface program is now supported.  You need to
;; install No Gnus v0.2 (and later) in which the compface.el module
;; provides that program.  The `x-face-use-uncompface-internal'
;; variable switches whether to use it.

;; Displaying the `Face' header is now supported.  It is assumed that
;; the `Face' header contains a base64 encoded PNG image.  You need to
;; have installed Emacs 21 with the PNG support.  You also need the
;; external pngtopnm command if you'd like to scale Face images.  When
;; you use Emacs 22, it should be the version of 2004-02-05 and later.

;; At first, you have to install this file manually to the location
;; which is referred by `load-path'.  For example:
;;
;; # cp -p x-face-e21.el /usr/local/share/emacs/site-lisp/
;; # cd /usr/local/share/emacs/site-lisp/
;; # emacs -batch -f batch-byte-compile x-face-e21.el

;; If you would like to use BBDB, it should be the version 2.2 or
;; later.  The old versions are out of support even if it works.

;; If you wish to scale down or scale up X-Face images, the external
;; commands pnmscale and pgmtoppm are required.  This feature has been
;; tested with netpbm-9.24 and its bundled commands.

;; Gray X-Faces are now supported.  You can save those X-Face headers
;; in an XPM file using the command `x-face-save'.

;; The command `x-face-insert' allows not only XBM files but also pre-
;; encoded data files.  Therefore, if you already have such a file,
;; for example, ~/.xface which is the default value of
;; `wl-x-face-file' or `mew-x-face-file', you can use it just as it
;; is.

;; FYI: Gnus, VM (7.03 and later) and possibly Mew can display X-Faces
;; inline under Emacs 21+ by themselves.  So, you don't have to use
;; this program if you are using one of them and you only want to show
;; X-Faces simply.

;; Here are some examples of how to use this package with various MUAs
;; and BBDB.

;;[Common settings for all MUAs] For your .emacs file:
;;----------------------------------------------------
;;(autoload 'x-face-decode-message-header "x-face-e21")
;;
;;(autoload 'x-face-insert "x-face-e21" nil t)
;;(autoload 'x-face-save "x-face-e21" nil t)
;;(autoload 'x-face-show "x-face-e21" nil t)
;;(autoload 'x-face-ascii-view "x-face-e21" nil t)
;;(autoload 'x-face-turn-off "x-face-e21")
;;
;;;; Show X-Face images when `x-face-insert' is done.
;;(setq x-face-auto-image t)
;;
;;;; If you show X-Face images in the message sending buffer, it is
;;;; strongly recommended that you remove images from the buffer before
;;;; sending a message (it isn't required with Gnus, though).  The
;;;; following lines are for SEMI and Mew (the latter can be put into
;;;; the .mew file instead).
;;(add-hook 'mime-edit-translate-hook 'x-face-turn-off)
;;(add-hook 'mew-make-message-hook 'x-face-turn-off)


;;[Cmail with SEMI] For your .emacs file:
;;---------------------------------------
;;(add-hook 'mime-display-header-hook 'x-face-decode-message-header)
;;(eval-after-load "cmail-modes"
;;  '(progn
;;     (define-key cmail-summary-mode-map "\C-x4s" 'x-face-save)
;;     (define-key cmail-summary-mode-map "\C-x4a" 'x-face-ascii-view)
;;     (define-key cmail-mail-mode-map "\C-x4i" 'x-face-insert)
;;     (define-key cmail-mail-mode-map "\M-t" 'x-face-show)))
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'cmail-mail-hook 'x-face-insert)


;;[Gnus] For your .gnus.el file:
;;------------------------------
;;(eval-after-load "gnus-sum"
;;  '(progn
;;     (define-key gnus-summary-mode-map "\C-x4s" 'x-face-save)
;;     (define-key gnus-summary-mode-map "\C-x4a" 'x-face-ascii-view)))
;;(define-key message-mode-map "\C-x4i" 'x-face-insert)
;;(define-key message-mode-map "\M-t" 'x-face-show)
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'gnus-message-setup-hook 'x-face-insert)
;;
;;;; Although using x-face-e21 to display X-Faces in the article buffer
;;;; is not recommended, uncomment the following lines if you want to
;;;; do that anyway.
;;;;(setq gnus-article-x-face-command
;;;;      (lambda (&rest args)
;;;;	(x-face-decode-message-header nil nil nil 'face)))
;;;;(eval-after-load "gnus-art"
;;;;  '(setcdr (assq 'gnus-treat-display-face
;;;;		 gnus-treatment-function-alist)
;;;;	   '((lambda (&rest args)
;;;;	       (x-face-decode-message-header nil nil nil 'x-face)))))


;;[Mew] For your .mew file:
;;-------------------------
;;;; Note that the specifications of variables and functions in Mew will
;;;; be changed frequently and exclusively, so the following example may
;;;; become obsolete sooner or later.  The best way is to not use this
;;;; program.
;;
;;(setq mew-use-highlight-x-face t)
;;(if (fboundp 'mew-use-highlight-x-face-function)
;;    (setq mew-use-highlight-x-face-function
;;	  'x-face-decode-message-header)
;;  (defalias 'mew-highlight-x-face 'x-face-decode-message-header))
;;(define-key mew-summary-mode-map "\C-x4s" 'x-face-save)
;;(define-key mew-summary-mode-map "\C-x4a" 'x-face-ascii-view)
;;(define-key mew-draft-mode-map "\C-x4i" 'x-face-insert)
;;(define-key mew-draft-header-map "\C-x4i" 'x-face-insert)
;;(define-key mew-draft-mode-map "\M-t" 'x-face-show)
;;(define-key mew-draft-header-map "\M-t" 'x-face-show)
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'mew-draft-mode-hook 'x-face-insert)


;;[MH-E without EMH] For your .emacs file:
;;----------------------------------------
;;(add-hook 'mh-show-mode-hook 'x-face-decode-message-header)
;;(eval-after-load "mh-e"
;;  '(progn
;;     (define-key mh-folder-mode-map "\C-x4s" 'x-face-save)
;;     (define-key mh-folder-mode-map "\C-x4a" 'x-face-ascii-view)))
;;(eval-after-load "mh-comp"
;;  '(progn
;;     (define-key mh-letter-mode-map "\C-x4i" 'x-face-insert)
;;     (define-key mh-letter-mode-map "\M-t" 'x-face-show)))
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'mh-letter-mode-hook 'x-face-insert)


;;[MH-E with EMH] For your .emacs file:
;;-------------------------------------
;;(add-hook 'mime-display-header-hook 'x-face-decode-message-header)
;;(eval-after-load "mh-e"
;;  '(progn
;;     (define-key mh-folder-mode-map "\C-x4s" 'x-face-save)
;;     (define-key mh-folder-mode-map "\C-x4a" 'x-face-ascii-view)))
;;(eval-after-load "mh-comp"
;;  '(progn
;;     (define-key mh-letter-mode-map "\C-x4i" 'x-face-insert)
;;     (define-key mh-letter-mode-map "\M-t" 'x-face-show)))
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'mh-letter-mode-hook 'x-face-insert)


;;[RMAIL without RMAIL-MIME] For your .emacs file:
;;------------------------------------------------
;;(add-hook 'rmail-show-message-hook 'x-face-decode-message-header)
;;(eval-after-load "rmail"
;;  '(progn
;;     (define-key rmail-mode-map "\C-x4s" 'x-face-save)
;;     (define-key rmail-mode-map "\C-x4a" 'x-face-ascii-view)))
;;(eval-after-load "rmailsum"
;;  '(progn
;;     (define-key rmail-summary-mode-map "\C-x4s" 'x-face-save)
;;     (define-key rmail-summary-mode-map "\C-x4a" 'x-face-ascii-view)))
;;(eval-after-load "sendmail"
;;  '(progn
;;     (define-key mail-mode-map "\C-x4i" 'x-face-insert)
;;     (define-key mail-mode-map "\M-t" 'x-face-show)))
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'mail-setup-hook 'x-face-insert)


;;[RMAIL with RMAIL-MIME] For your .emacs file:
;;---------------------------------------------
;;(add-hook 'mime-display-header-hook 'x-face-decode-message-header)
;;(eval-after-load "rmail"
;;  '(progn
;;     (define-key rmail-mode-map "\C-x4s" 'x-face-save)
;;     (define-key rmail-mode-map "\C-x4a" 'x-face-ascii-view)))
;;(eval-after-load "rmailsum"
;;  '(progn
;;     (define-key rmail-summary-mode-map "\C-x4s" 'x-face-save)
;;     (define-key rmail-summary-mode-map "\C-x4a" 'x-face-ascii-view)))
;;(eval-after-load "sendmail"
;;  '(progn
;;     (define-key mail-mode-map "\C-x4i" 'x-face-insert)
;;     (define-key mail-mode-map "\M-t" 'x-face-show)))
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'mail-setup-hook 'x-face-insert)


;;[VM] For your .vm file:
;;-----------------------
;;(defadvice vm-energize-headers-and-xfaces
;;  (after x-face-decode-message-header activate compile)
;;  "Show X-Face using `x-face-decode-message-header'."
;;  (save-restriction
;;    (widen)
;;    (narrow-to-region (vm-headers-of (car vm-message-pointer))
;;		      (vm-text-of (car vm-message-pointer)))
;;    (x-face-decode-message-header)))
;;(define-key vm-summary-mode-map "\C-x4s" 'x-face-save)
;;(define-key vm-summary-mode-map "\C-x4a" 'x-face-ascii-view)
;;(define-key vm-mail-mode-map "\C-x4i" 'x-face-insert)
;;(define-key vm-mail-mode-map "\M-t" 'x-face-show)
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'vm-mail-mode-hook 'x-face-insert)


;;[Wanderlust] For your .wl file:
;;-------------------------------
;;(setq wl-highlight-x-face-function 'x-face-decode-message-header)
;;(define-key wl-summary-mode-map "\C-x4s" 'x-face-save)
;;(define-key wl-summary-mode-map "\C-x4a" 'x-face-ascii-view)
;;(define-key wl-draft-mode-map "\C-x4i" 'x-face-insert)
;;;; "\M-t" key is reserved for wl command.
;;(define-key wl-draft-mode-map "\M-\C-t" 'x-face-show)
;;
;;;; If a file name has no directory component, it should be found in
;;;; the directory specified by the `x-face-image-file-directory'
;;;; variable.
;;(setq x-face-default-xbm-file "YourFace.xbm")
;;
;;(add-hook 'wl-mail-setup-hook 'x-face-insert)
;;;; If you use `wl-draft-insert-x-face-field' instead of
;;;; `x-face-insert' for inserting an X-Face, you can highlight it as an
;;;; image with the setting of the following hook:
;;(add-hook 'wl-draft-insert-x-face-field-hook
;;	  (lambda nil
;;	    (x-face-insert wl-x-face-file)))


;;[BBDB]
;;------
;; To make BBDB automatically add X-Face and Face headers in the record,
;; put the following lines in your startup (commonly .emacs) file after
;; setting up the value of `bbdb-auto-notes-alist' or after the library
;; "bbdb-hooks" is loaded (you may use `eval-after-load' for that).
;;
;; By courtesy of YAMASHITA Junji, the "Face" section was added and also
;; the `x-face-energize-bbdb-buffer' function was modified.
;;
;;(put 'face 'field-separator "\n")
;;(setq bbdb-auto-notes-alist
;;      (append bbdb-auto-notes-alist
;;	      `(("x-face"
;;		 (,(concat "[[:blank:]\n]*\\([^\n]+\\)"
;;			   "\\(?:[[:blank:]\n]+\\([^\n]+\\)\\)?"
;;			   "\\(?:[[:blank:]\n]+\\([^\n]+\\)\\)?"
;;			   "\\(?:[[:blank:]\n]+\\([^\n]+\\)\\)?"
;;			   "\\(?:[[:blank:]\n]+\\([^\n]+\\)\\)?")
;;		  face
;;		  "\\1\\2\\3\\4\\5"))
;;		("Face"
;;		 (,(concat "[[:blank:]\n]*\\([^[:blank:]\n]*\\)"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?"
;;			   "\\([[:blank:]\n]+\\([^[:blank:]\n]+\\)\\)?")
;;		  face
;;		  (lambda (string)
;;		    (let ((n 1)
;;			  (result ""))
;;		      (while (match-beginning n)
;;			(setq result (concat result
;;					     (substring string
;;							(match-beginning n)
;;							(match-end n))))
;;			(setq n (+ n 2)))
;;		      result)))))))

;;; Code:

(eval-and-compile
  (if (or (featurep 'xemacs)
	  (not (boundp 'emacs-major-version))
	  (< emacs-major-version 21))
      (error "This program is for Emacs 21+, good-bye")))

(defconst x-face-e21-version "0.129")

(defgroup x-face nil
  "X-Face utilities."
  :group 'news
  :group 'mail)

(defcustom x-face-image-file-directory "~/x-faces/"
  "Name of the directory where image files can be found."
  :version "21.1"
  :type '(directory :format "%t:\nDirectory: %v" :size 0)
  :group 'x-face)

(defcustom x-face-default-xbm-file nil
  "Default XBM file name for the user's face.
The file will be inserted in the buffer as an X-Face header using the
`x-face-insert' command without argument.  If the file name does not
include directory components, it should be found in the directory
which is specified by the `x-face-image-file-directory' variable."
  :version "21.1"
  :type '(radio (const :tag "Not specified" nil)
		(string :format "XBM File: %v\n" :size 0))
  :group 'x-face)

(defcustom x-face-insert-query-file-name-when-no-argument nil
  "Non-nil means the `x-face-insert' function prompts you for a file name.
It will occur only when the `x-face-insert' function is called
non-interactively with no argument."
  :version "21.1"
  :type '(boolean :format "%{%t%}: %[%v%]")
  :group 'x-face)

(defcustom x-face-image-file-directory-for-save x-face-image-file-directory
  "Name of the directory where image files should go.
The `x-face-save' command refers to it."
  :version "21.1"
  :type '(directory :format "%t:\nDirectory: %v" :size 0)
  :group 'x-face)

(defcustom x-face-compressor nil
  "Compressing method used for saving image files.
The valid values include nil, `gzip' and `bzip2'."
  :version "21.1"
  :type '(radio (const :format "%v " bzip2)
		(const :format "%v " gzip)
		(const :tag "off" nil))
  :group 'x-face)

(defcustom x-face-use-overlay nil
  "Non-nil means use the `before-string' overlay to show an X-Face image.
It is useful if you don't want to insert any extra text in the showing
buffer.  However, you should take notice that some MUAs (e.g. Gnus)
might delete such overlays unconditionally.  It will be overridden
with t buffer-locally when the `x-face-insert' function is performed."
  :version "21.1"
  :type '(boolean :format "%{%t%}: %[%v%]")
  :group 'x-face)

(defcustom x-face-hide-related-headers nil
  "Non-nil means hide X-Face related headers in the showing buffer.
Set it as t if you don't want to show X-Face-Version headers, etc.  It
will be overridden with nil buffer-locally when the `x-face-insert'
function is performed."
  :version "21.1"
  :type '(boolean :format "%{%t%}: %[%v%]")
  :group 'x-face)

(defcustom x-face-hidden-properties '(invisible t intangible t)
  "Property list used to hide raw X-Face headers or their related headers.
It will be modified buffer-locally in order to transfer an icon image
when the `x-face-insert' function is performed."
  :version "21.1"
  :type '(plist :key-type (symbol :format "Key: %v\n" :size 0)
		:value-type (sexp :format "Value: %v\n" :size 0))
  :group 'x-face)

(defcustom x-face-auto-image nil
  "If it is non-nil, show X-Face images when `x-face-insert'is performed.
It can also be a function, for example, `interactive-p',
`(lambda nil (memq major-mode '(message-mode wl-draft-mode)))', etc."
  :version "21.1"
  :type '(radio (const :format "%t " :tag "OFF" nil)
		(const :format "%t\n" :tag "ON" t)
		(function :format "Function: %v" :size 0))
  :group 'x-face)

(defcustom x-face-mouse-face 'highlight
  "Face used to highlight buttons when the mouse moves over it."
  :version "21.1"
  :type 'face
  :group 'x-face)

(defcustom x-face-bbdb-display (and (locate-library "bbdb") t)
  "If it is non-nil, show X-Face images in the BBDB buffer."
  :version "21.1"
  :type '(boolean :format "%{%t%}: %[%v%]")
  :group 'x-face)

(defvar x-face-ring nil "Cache for face data.")
(defvar x-face-ring-size 32 "Length of the ring used for `x-face-ring'.")

(defcustom x-face-frame-background-mode nil
  "The brightness of the background.
It is similar to the `frame-background-mode' variable, except that it
only affects when showing X-Face images and ascii pictures.  Set this
to the symbol `dark' if your background color is dark, `light' if your
background is light, or nil (default) if you want Emacs to examine the
brightness for you."
  :version "21.1"
  :type '(radio (const :format "%v " nil)
		(const :format "%v " light)
		(const dark))
  :group 'x-face)

(eval-when-compile
  (defmacro x-face-dark-background-p ()
    "Return t if the selected frame's background is dark."
    '(eq 'dark (or x-face-frame-background-mode
		   (frame-parameter (selected-frame) 'background-mode)))))

(eval-when-compile
  (defmacro x-face-image-attributes-for-bbdb ()
    "Merge the value of the `x-face-image-attributes-for-bbdb' variable
into the copy of the `x-face-image-attributes' variable and return the
result."
    '(let* ((bgmode (if (x-face-dark-background-p)
			'dark
		      'light))
	    (basis (copy-sequence (symbol-value 'x-face-image-attributes)))
	    (element (assq bgmode basis))
	    (modifiers (cdr (assq bgmode
				  (symbol-value
				   'x-face-image-attributes-for-bbdb))))
	    attributes)
       (if element
	   (progn
	     (setq attributes (copy-sequence (cdr element)))
	     (while modifiers
	       (setq attributes (plist-put attributes
					   (car modifiers) (cadr modifiers))
		     modifiers (cddr modifiers)))
	     (cons (cons bgmode attributes) (delq element basis)))
	 (cons (cons bgmode modifiers) basis)))))

(put 'x-face-internal-function 'show-sample
     (lambda (&optional bbdb remove-cache)
       "Show an X-Face sample image in a customization buffer.  If BBDB is
non-nil, assign the value of `x-face-image-attributes-for-bbdb' to an
image instead of `x-face-image-attributes'.  If REMOVE-CACHE is non-
nil, reset the value of `x-face-ring' to nil."
       (when (and (display-images-p)
		  (image-type-available-p 'pbm)
		  (eq 'custom-mode major-mode))
	 (save-excursion
	   (let ((inhibit-point-motion-hooks t))
	     (goto-char (point-min))
	     (when (re-search-forward "^<?X Face Image Attributes" nil t)
	       (forward-line 1)
	       (let ((inhibit-read-only t))
		 (delete-region (point)
				(progn
				  (when (looking-at "From:")
				    (forward-line 1)
				    (when (search-forward "X-Face:" nil t)
				      (while (progn
					       (forward-line 1)
					       (memq (char-after)
						     '(?\t ?\ ))))))
				  (skip-chars-forward "\n")
				  (point)))
		 (save-restriction
		   (narrow-to-region (point) (point))
		   (unless
		       (and x-face-default-xbm-file
			    (prog2
				(insert "From: "
					(if user-mail-address
					    (if user-full-name
						(concat
						 user-full-name " <"
						 user-mail-address ">")
					      user-mail-address)
					  "X-Face")
					"\n")
				(condition-case nil
				    (let (x-face-auto-image
					  x-face-hidden-properties)
				      (x-face-insert x-face-default-xbm-file)
				      t)
				  (error
				   (delete-region (point-min) (point-max))
				   nil))
			      (dolist (var '(x-face-use-overlay
					     x-face-hide-related-headers
					     x-face-hidden-properties))
				(kill-local-variable var))))
		     (insert "\
From: X-Face
X-Face: 2i'm.M0UyETCme?'R/?fE}i)R-aY$t;].MSLwmUfB\"^3H+so!vO79{mzviSR4#DM+}\"\"
 ZwMOJ~e&Rr*qL'CrQZo-@jdTL=w{o3Pxu2PY]_qB=w%GLU1S_Pk8HX>4C}W2YTc=V=(~QH[vcm2!]O
 pq&CB^,K:NO/lVI-m&kP;pa&K.Xo)<V4=l<eRW]>X\n"))
		   (when remove-cache
		     (setq x-face-ring nil))
		   (if bbdb
		       (let ((x-face-image-attributes
			      (x-face-image-attributes-for-bbdb))
			     x-face-ring)
			 (x-face-decode-message-header))
		     (let (x-face-ring)
		       (x-face-decode-message-header)))
		   (goto-char (point-min))
		   (forward-line 1)
		   (delete-region (point) (point-max))))))))))

(put 'x-face-internal-function 'image-attributes
     (lambda nil
       "Return a widget type for entering X-Face image attributes."
       (let* ((props (list :ascent :margin :relief :conversion
			   :foreground :background
			   ;; Extra keyword.
			   :scale-factor))
	      (lmax (let ((result 0))
		      (dolist (prop props result)
			(setq result (max result
					  (length (symbol-name prop)))))))
	      (tag "When frame background mode is %v\n")
	      (left (- (length tag) lmax 4))
	      (selection `(checklist :format
				     ,(format (format "%%%ds%%%%v" (- left 2))
					      "")))
	      prop)
	 (while (setq prop (pop props))
	   (setq selection
		 (nconc
		  selection
		  `((list :format "%v" :inline t
			  (const :format "%v " :value ,prop)
			  (sexp :format
				,(format
				  (format
				   "%%%ds%%-%ds"
				   (- lmax (length (symbol-name prop)))
				   (if props left 0))
				  "" "%v\n")
				:size 0))))))
	 `(list (cons :format "%v" (sexp :format ,tag :size 0) ,selection)
		(cons :format "%v" (sexp :format ,tag :size 0) ,selection)))))

(put 'x-face-internal-function 'uncompface-internal
     (lambda nil
       "Check whether `uncompface-internal' is available."
       (if noninteractive
	   nil
	 (and (or (and (featurep 'compface)
		       (fboundp 'uncompface-internal))
		  (let ((compface (locate-library "compface")))
		    (and compface
			 (string-match "\\.elc\\'" compface)
			 (file-readable-p compface)
			 (> (nth 7 (file-attributes compface)) 20000)
			 (condition-case nil
			     (progn
			       (setq compface
				     (if (fboundp 'uncompface)
					 (symbol-function 'uncompface)))
			       (require 'compface))
			   (error nil))
			 (or (fboundp 'uncompface-internal)
			     (prog1
				 nil
			       (if compface
				   (fset 'uncompface compface)
				 (fmakunbound 'uncompface))
			       (setq features
				     (delq 'compface features)))))))))))

(eval-when-compile
  (defmacro x-face-cleanup-plist (plist)
    "Remove properties with the value nil in PLIST."
    `(let ((plist ,plist)
	   property value rest)
       (while (prog1
		  (setq property (pop plist))
		(when (setq value (pop plist))
		  (setq rest (nconc rest (list property value))))))
       rest)))

(put 'x-face-internal-function 'cleanup-attributes
     (lambda (attributes)
       "Remove properties with the value nil in each element of ATTRIBUTES."
       (dolist (element attributes attributes)
	 (setcdr element (x-face-cleanup-plist (cdr element))))))

(defcustom x-face-image-attributes
  '((light :ascent 80 :foreground "#000000")
    (dark :ascent 80 :foreground "#000000" :background "#ffffff"))
  "List of image attributes assigning to the X-Face images.
Each element consists of the value of the frame background mode
\(`light' or `dark') and keyword-value pairs.  This allows the extra
keyword `:scale-factor' which is used to scale down or scale up X-Face
images.  The value for the `:scale-factor' keyword should be a
positive number or nil (treated as 1)."
  :version "21.1"
  :type (funcall (get 'x-face-internal-function 'image-attributes))
  :get (lambda (symbol)
	 (prog1
	     (let* ((value (default-value symbol))
		    (first (assq (or (frame-parameter (selected-frame)
						      'background-mode)
				     'light)
				 value)))
	       (funcall (get 'x-face-internal-function 'cleanup-attributes)
			(cons first (delq first (copy-sequence value)))))
	   (funcall (get 'x-face-internal-function 'show-sample))))
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default
	      symbol
	      (funcall (get 'x-face-internal-function 'cleanup-attributes)
		       value))
	   (funcall (get 'x-face-internal-function 'show-sample) nil t)))
  :group 'x-face)

(defcustom x-face-image-attributes-for-bbdb
  '((light :ascent center :foreground "#002000" :background "#fffacd"
	   :relief 2 :scale-factor 0.5)
    (dark :ascent center :foreground "#002000" :background "#fffacd"
	  :relief 2 :scale-factor 0.5))
  "Additional image attributes to show X-Face images in the BBDB buffer.
It is used by merging into the `x-face-image-attributes' variable.
The value form is the same as `x-face-image-attributes'."
  :version "21.1"
  :type (funcall (get 'x-face-internal-function 'image-attributes))
  :get (lambda (symbol)
	 (prog1
	     (let* ((value (default-value symbol))
		    (first (assq (or (frame-parameter (selected-frame)
						      'background-mode)
				     'light)
				 value)))
	       (cons first (delq first (copy-sequence value))))
	   (funcall (get 'x-face-internal-function 'show-sample) t)))
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (funcall (get 'x-face-internal-function 'show-sample) t t)))
  :group 'x-face)

(defcustom x-face-use-uncompface-internal
  (funcall (get 'x-face-internal-function 'uncompface-internal))
  "Say whether to use the ELisp based uncompface program."
  :version "21.1"
  :type '(boolean :format "%{%t%}: %[%v%]")
  :set (lambda (symbol value)
	 (custom-set-default symbol
			     (and value
				  (funcall (get 'x-face-internal-function
						'uncompface-internal)))))
  :group 'x-face)

(defcustom uncompface-program "uncompface"
  "Program used to decode X-Face."
  :version "21.1"
  :type '(string :size 0)
  :group 'x-face)

(defcustom compface-program "compface"
  "Program used to encode X-Face."
  :version "21.1"
  :type '(string :size 0)
  :group 'x-face)

(defcustom x-face-pnmscale-program "pnmscale"
  "The pnmscale executable which comes from the netpbm package.
It will be used to scale down or scale up X-Face images if the value
for the `:scale-factor' keyword in the `x-face-image-attributes'
variable or the `x-face-image-attributes-for-bbdb' variable is neither
nil nor the number 1.  For quickly scaling, you can alter it to the
\"pnmscalefixed\" command."
  :version "21.1"
  :type '(radio (const :format "Not specified " nil)
		(string :format "%v" :size 0))
  :set (lambda (symbol value)
	 (custom-set-default
	  symbol
	  (and (not noninteractive)
	       (stringp value)
	       (with-temp-buffer
		 (set-buffer-multibyte nil)
		 (insert "P1\n1 1\n0\n")
		 (condition-case nil
		     (let ((coding-system-for-read 'binary)
			   (coding-system-for-write 'binary))
		       (call-process-region (point-min) (point-max)
					    value t '(t nil) nil "2")
		       (goto-char (point-min))
		       (let (case-fold-search)
			 (re-search-forward "\
^P5[[:blank:]\n\r]+2[[:blank:]\n\r]+2[[:blank:]\n\r]+" nil t)))
		   (error nil)))
	       value)))
  :group 'x-face)

(defcustom x-face-pgmtoppm-program "pgmtoppm"
  "The pgmtoppm executable which comes from the netpbm package.
It will be used to colorize scaled X-Face images."
  :version "21.1"
  :type '(radio (const :format "Not specified " nil)
		(string :format "%v" :size 0))
  :set (lambda (symbol value)
	 (custom-set-default
	  symbol
	  (and (not noninteractive)
	       (stringp value)
	       (with-temp-buffer
		 (set-buffer-multibyte nil)
		 (insert "P1\n2 1\n0 1\n")
		 (condition-case nil
		     (let ((coding-system-for-read 'binary)
			   (coding-system-for-write 'binary))
		       (call-process-region (point-min) (point-max)
					    value t '(t nil) nil
					    "#000000-#ffffff")
		       (goto-char (point-min))
		       (let (case-fold-search)
			 (re-search-forward "\
^P6[[:blank:]\n\r]+2[[:blank:]\n\r]+1[[:blank:]\n\r]+\
\[[:digit:]]+[[:blank:]\n\r]+"
					    nil t)))
		   (error nil)))
	       value)))
  :group 'x-face)

(defcustom x-face-pngtopnm-program "pngtopnm"
  "The pngtopnm executable which comes from the netpbm package.
It will be used to scale down or scale up Face images together with
the pnmscale program if the value for the `:scale-factor' keyword in
the `x-face-image-attributes' variable or the
`x-face-image-attributes-for-bbdb' variable is neither nil nor the
number 1."
  :version "21.1"
  :type '(radio (const :format "Not specified " nil)
		(string :format "%v" :size 0))
  :set (lambda (symbol value)
	 (custom-set-default
	  symbol
	  (and (not noninteractive)
	       (stringp value)
	       (with-temp-buffer
		 (set-buffer-multibyte nil)
		 (insert (base64-decode-string "iVBORw0KGgoAAAANSUhEUg\
AAAAIAAAABAQAAAADcWUInAAAACklEQVR4nGM4AAAAwgDBUl5XUQAAAABJRU5ErkJggg=="))
		 (condition-case nil
		     (let ((coding-system-for-read 'binary)
			   (coding-system-for-write 'binary))
		       (call-process-region (point-min) (point-max)
					    value t '(t nil))
		       (goto-char (point-min))
		       (let (case-fold-search)
			 (re-search-forward "\
^P4[[:blank:]\n\r]+2[[:blank:]\n\r]+1[[:blank:]\n\r]+\000" nil t)))
		   (error nil)))
	       value)))
  :group 'x-face)

(defvar x-face-field-icon
  (if (image-type-available-p 'xpm)
      (create-image "/* XPM */
static char * x_face_field_icon_xpm[] = {
/* width height ncolors chars_per_pixel */
\"16 14 6 1\",
/* colors */
\"     c None\",
\".    c #000000 s outline\",
\"X    c #ff0000\",
\"o    c #00ff00\",
\"O    c #ffff00\",
\"+    c #00ffff\",
/* pixels */
\"                \",
\"......       ...\",
\".XXXXX.     .o. \",
\" .XXXXX.   .o.  \",
\"  .XXXXX. .o.   \",
\"   .XXXXX.o.    \",
\"    .XXX.O.     \",
\"     .X.OOO.    \",
\"    .+.OOOOO.   \",
\"   .+. .OOOOO.  \",
\"  .+.   .OOOOO. \",
\" .+.     .OOOOO.\",
\"...       ......\",
\"                \"
};
"
		    'xpm t :ascent 'center)
    (create-image "P1
16 14
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 1 1 1 1 1 0 0 0 0 0 0 0 1 0 0
0 0 1 1 1 1 1 0 0 0 0 0 1 0 0 0
0 0 0 1 1 1 1 1 0 0 0 1 0 0 0 0
0 0 0 0 1 1 1 1 1 0 1 0 0 0 0 0
0 0 0 0 0 1 1 1 0 1 0 0 0 0 0 0
0 0 0 0 0 0 1 0 1 1 1 0 0 0 0 0
0 0 0 0 0 1 0 1 1 1 1 1 0 0 0 0
0 0 0 0 1 0 0 0 1 1 1 1 1 0 0 0
0 0 0 1 0 0 0 0 0 1 1 1 1 1 0 0
0 0 1 0 0 0 0 0 0 0 1 1 1 1 1 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
"
		  'pbm t :ascent 'center :mask 'heuristic))
  "Icon image used to be iconified raw X-Face header fields.
It can be `pbm', `xbm' or `xpm' image format, or the value nil meaning
not to iconify the fields.")

(defvar x-face-enable-cache t
  "*Non-nil means images are cached in the `x-face-ring' variable.")

(autoload 'ring-elements "ring")
(autoload 'ring-insert "ring")

(defvar x-face-show-buffer nil
  "Internal variable used to say where X-Face images should be displayed.
A value is a buffer in which images will be displayed.")

(defvar x-face-working-buffer " *x-face-working*"
  "Temporary buffer for the internal use.")

(defconst x-face-most-positive-fixnum (eval '(lsh -1 -1))
  "Maximum integer for this Emacs.")

(eval-when-compile
  ;; Avoid byte-compile warnings.
  (autoload 'cmail-folder-buffer "cmail-misc")
  (autoload 'cmail-get-page-number-from-summary "cmail-misc")
  (autoload 'cmail-n-page "cmail-misc")
  (autoload 'cmail-show-contents "cmail")
  (autoload 'gnus-summary-select-article "gnus-sum")
  (autoload 'mew-buffer-message "mew")
  (autoload 'mew-summary-display "mew-summary")
  (autoload 'mh-get-msg-num "mh-utils")
  (autoload 'mh-show-msg "mh-utils")
  (autoload 'uncompface-internal "compface")
  (autoload 'vm-follow-summary-cursor "vm-motion")
  (autoload 'wl-message-get-original-buffer "wl-message")
  (autoload 'wl-summary-set-message-buffer-or-redisplay "wl-summary")

  (defsubst x-face-buffer-live-p (buffer)
    "Say whether BUFFER is alive or not."
    (and buffer
	 (get-buffer buffer)
	 (buffer-name (get-buffer buffer))))

  (defsubst x-face-narrow-to-head ()
    "Narrow the buffer to the head of the message.  Point is left at the
beginning of the narrowed-to region."
    (narrow-to-region
     (goto-char (point-min))
     (if (let (case-fold-search)
	   (re-search-forward (concat "^$\\|^"
				      (regexp-quote mail-header-separator)
				      "$")
			      nil t))
	 (match-beginning 0)
       (point-max)))
    (goto-char (point-min)))

  (defmacro x-face-set-properties (start end properties &optional mark)
    "If `x-face-use-overlay' is nil, set PROPERTIES and MARK to the text
from START to END, otherwise use overlay instead of text property."
    `(let ((properties ,properties))
       (when properties
	 (let ((start ,start)
	       (end ,end))
	   (if x-face-use-overlay
	       (let ((overlay (make-overlay start end)))
		 (overlay-put overlay 'evaporate t)
		 (overlay-put overlay 'priority x-face-most-positive-fixnum)
		 ,(when mark
		    `(overlay-put overlay ,mark t))
		 (while properties
		   (overlay-put overlay (pop properties) (pop properties))))
	     ,(when mark
		`(put-text-property start end ,mark t))
	     (let ((s1 (1+ start))
		   (e1 (1- end)))
	       (cond ((< s1 e1)
		      (put-text-property start s1 'front-sticky nil)
		      (put-text-property start e1 'rear-nonsticky nil)
		      (put-text-property s1 end 'front-sticky t)
		      (put-text-property e1 end 'rear-nonsticky t))
		     ((= s1 e1)
		      (add-text-properties start s1
					   '(front-sticky nil
							  rear-nonsticky nil))
		      (add-text-properties e1 end
					   '(front-sticky t
							  rear-nonsticky t)))
		     ((= start e1)
		      (add-text-properties start end
					   '(front-sticky nil
							  rear-nonsticky t)))))
	     (add-text-properties start end properties))))))

  (defsubst x-face-remove-x-face-images ()
    "Remove all X-Face images.  The buffer is expected to be narrowed to
just the header of the message."
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'x-face-image)
	(delete-overlay overlay)))
    (let ((inhibit-point-motion-hooks t)
	  (end (point-min))
	  start regions)
      (while (and end
		  (setq start (text-property-any end (point-max)
						 'x-face-image t)))
	(push (cons start
		    (setq end (text-property-not-all start (point-max)
						     'x-face-image t)))
	      regions))
      (let ((mod (buffer-modified-p))
	    (inhibit-read-only t))
	(dolist (region regions)
	  (delete-region (car region) (or (cdr region) (point-max))))
	(set-buffer-modified-p mod))))

  (defsubst x-face-expose-hidden-text ()
    "Expose raw X-Face and its related headers.  The buffer is expected to
be narrowed to just the header of the message."
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'x-face-invisible)
	(delete-overlay overlay)))
    (let ((props (append x-face-hidden-properties '(x-face-invisible t)))
	  (mod (buffer-modified-p))
	  (inhibit-read-only t)
	  (inhibit-point-motion-hooks t)
	  (end (point-min))
	  start)
      (while (and end
		  (setq start (text-property-any end (point-max)
						 'x-face-invisible t)))
	(setq end (text-property-not-all start (point-max)
					 'x-face-invisible t))
	(remove-text-properties start (or end (point-max)) props))
      (set-buffer-modified-p mod)))

  (defmacro x-face-insert-error (error-symbol &rest args)
    "Signal ERROR-SYMBOL with concatenated ARGS."
    (list 'let* (list (list 'args (cons 'list args))
		      '(rest (if (interactive-p)
				 (pop args)
			       "[x-face-insert]")))
	  (list 'signal error-symbol
		'(list (dolist (arg args rest)
			 (setq rest (concat rest " " arg)))))))

  (defmacro x-face-bool-vector-to-string (bool-vector)
    "Convert a 2304-bit bool-vector to a 288-byte binary string."
    `(read (substring (let (print-length)
			(prin1-to-string ,bool-vector))
		      6)))

  (defmacro x-face-string-to-bool-vector (string)
    "Convert a 288-byte binary string to a 2304-bit bool-vector."
    `(read (concat "#&2304" (let (print-length)
			      (prin1-to-string ,string)))))

  (defmacro x-face-cleanup-x-face (x-face &optional with-header)
    "Strip an X-Face header, whitespace and newlines from X-FACE.  If
WITH-HEADER is non-nil, it is assumed that X-FACE includes the X-Face:
header, otherwise it will examine whether there is the X-Face: header."
    (if (or (eq t with-header)
	    (and with-header
		 (eq 'quote (car-safe with-header))))
	`(apply 'concat (cdr (split-string ,x-face)))
      `(let ((x-face (split-string ,x-face)))
	 (if (or ,with-header
		 (string-match "^\\(X-\\)?Face\\(-[[:digit:]]+\\)?:$"
			       (car x-face)))
	     (apply 'concat (cdr x-face))
	   (apply 'concat x-face)))))

  (defmacro x-face-cleanup-x-faces (x-faces &optional with-header)
    "Strip the X-Face: header, whitespace and newlines from each X-FACE.
If WITH-HEADER is non-nil, it is assumed that each X-FACE includes the
X-Face: header, otherwise it will examine whether there is the X-Face:
header in each X-FACE."
    (if (or (eq t with-header)
	    (and with-header
		 (eq 'quote (car-safe with-header))))
	`(let ((x-faces ,x-faces)
	       faces)
	   (while x-faces
	     (push (apply 'concat (cdr (split-string (pop x-faces))))
		   faces))
	   (nreverse faces))
      `(let ((x-faces ,x-faces)
	     (header ,with-header)
	     face faces)
	 (while x-faces
	   (if header
	       (push (apply 'concat (cdr (split-string (car x-faces)))) faces)
	     (if (string-match "^\\(X-\\)?Face\\(-[[:digit:]]+\\)?:$"
			       (car (setq face (split-string (car x-faces)))))
		 (progn
		   (setq header t)
		   (push (apply 'concat (cdr face)) faces))
	       (push (apply 'concat face) faces)))
	   (setq x-faces (cdr x-faces)))
	 (nreverse faces)))))

(defconst x-face-mirror (eval-when-compile
			  (let ((table (make-vector 256 nil))
				(i 0)
				j k l)
			    (while (< i 256)
			      (setq j i
				    k 0
				    l 8)
			      (while (> l 0)
				(setq k (lsh k -1)
				      l (1- l))
				(if (>= (setq j (lsh j 1)) 256)
				    (setq j (logand 255 j)
					  k (logior 128 k))))
			      (aset table i k)
			      (setq i (1+ i)))
			    table))
  "Bitwise symmetrical table for 8-bit data.")

(defconst x-face-xpm-colors
  (eval-when-compile
    (vconcat
     " .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZASDF"))
  "Color map used to 6-bit XPM images.")

(defun x-face-perhaps-change-buffer ()
  "Perhaps change the buffer to where the raw X-Face headers can be found.
You can redefine it for customization.  Note that this function is
used by the `x-face-decode-message-header' function exclusively and
there is no need to change the buffer for almost MUAs except for RMAIL
without RMAIL-MIME.  The value of the `x-face-show-buffer' variable
can be modified if it is needed."
  (cond ((and (eq 'rmail-summary-mode major-mode)
	      (not (featurep 'rmail-mime)))
	 (set-buffer (symbol-value 'rmail-buffer))
	 (x-face-perhaps-change-buffer))
	((and (eq 'rmail-mode major-mode)
	      (not (featurep 'rmail-mime)))
	 (let ((buffer (get-buffer-create x-face-working-buffer)))
	   (save-excursion
	     (goto-char (point-min))
	     (save-restriction
	       (widen)
	       (when (search-backward "\n\n" nil t)
		 (skip-chars-backward "\n")
		 (forward-char))
	       (copy-to-buffer buffer (point)
			       (if (search-backward "\n\n" nil t)
				   (match-end 0)
				 (point-min)))))
	   (set-buffer buffer)))))

(defun x-face-possibly-change-buffer ()
  "Possibly change the buffer to where raw X-Face headers can be found.
This function will be used for saving or viewing X-Face headers
contained in the top of a message, which means that it cannot be used
for forwarded MIME parts in a message.  You can redefine it for
customization."
  (cond ((eq 'cmail-summary-mode major-mode)
	 (let ((page (cmail-get-page-number-from-summary))
	       (buffer (get-buffer-create x-face-working-buffer)))
	   (unless (eq page (symbol-value '*cmail-current-page))
	     (cmail-show-contents page))
	   (set-buffer (cmail-folder-buffer
			(symbol-value 'cmail-current-folder)))
	   (cmail-n-page page)
	   (copy-to-buffer buffer (point) (if (search-forward "\n\n" nil t)
					      (1+ (match-beginning 0))
					    (point-max)))
	   (set-buffer buffer)))
	((eq 'gnus-summary-mode major-mode)
	 (gnus-summary-select-article)
	 (set-buffer (symbol-value 'gnus-original-article-buffer)))
	((and (boundp 'gnus-article-buffer)
	      (eq (current-buffer)
		  (get-buffer (symbol-value 'gnus-article-buffer)))
	      (x-face-buffer-live-p (symbol-value
				     'gnus-original-article-buffer)))
	 (set-buffer (symbol-value 'gnus-original-article-buffer)))
	((memq major-mode '(mew-summary-mode mew-virtual-mode))
	 (mew-summary-display nil)
	 (set-buffer (mew-buffer-message)))
	((eq 'mh-folder-mode major-mode)
	 (let* ((num (mh-get-msg-num t))
		(buffer (symbol-value 'mh-show-buffer))
		(folder (symbol-value 'mh-current-folder))
		(raw-buffer (concat "article-" (if (bufferp folder)
						   (buffer-name folder)
						 folder))))
	   (if (and (x-face-buffer-live-p buffer)
		    (string-match (format "/%d$" num)
				  (buffer-file-name (get-buffer buffer))))
	       (set-buffer (if (x-face-buffer-live-p raw-buffer)
			       raw-buffer
			     buffer))
	     (mh-show-msg num)
	     (if (x-face-buffer-live-p raw-buffer)
		 (set-buffer raw-buffer)
	       (current-buffer)))))
	((eq 'rmail-summary-mode major-mode)
	 (set-buffer (symbol-value 'rmail-buffer))
	 (x-face-possibly-change-buffer))
	((eq 'rmail-mode major-mode)
	 (let ((buffer (get-buffer-create x-face-working-buffer)))
	   (save-excursion
	     (goto-char (point-min))
	     (save-restriction
	       (widen)
	       (when (search-backward "\n\n" nil t)
		 (skip-chars-backward "\n")
		 (forward-char))
	       (copy-to-buffer buffer (point)
			       (if (search-backward "\n\n" nil t)
				   (match-end 0)
				 (point-min)))))
	   (set-buffer buffer)))
	((eq 'vm-summary-mode major-mode)
	 (vm-follow-summary-cursor)
	 (if (x-face-buffer-live-p (symbol-value 'vm-mail-buffer))
	     (set-buffer (symbol-value 'vm-mail-buffer))
	   (error "Folder buffer has been killed")))
	((eq 'wl-summary-mode major-mode)
	 (wl-summary-set-message-buffer-or-redisplay)
	 (set-buffer (wl-message-get-original-buffer)))))

(defun x-face-sort-gray-x-faces (x-faces)
  "Sort gray X-FACES in order from msb to lsb.
Each X-FACE string should contain the X-Face: header or the
X-Face-NUMBER: header."
  (let (face faces)
    (while x-faces
      (setq face (split-string (pop x-faces)))
      (push (cons (string-to-number (substring (car face) 6 -1))
		  (apply 'concat (cdr face)))
	    faces))
    (setq faces (sort faces 'car-less-than-car))
    (while faces
      (push (cdr (pop faces)) x-faces)))
  x-faces)

;;;###autoload
(defun x-face-to-bitmap (x-face &optional bool-vector bit-reverse cleaned)
  "Convert an X-FACE to raw bitmap data.
If BOOL-VECTOR is non-nil, it will return a 2304-bit bool-vector,
otherwise 288-byte binary data as a string.  If BIT-REVERSE is non-nil,
each byte will be bit-reversed.  If CLEANED is non-nil, it is assumed
that X-FACE includes neither the X-Face: header, whitespace nor
newlines."
  (unless cleaned
    (if (stringp x-face)
	(setq x-face (x-face-cleanup-x-face x-face))
      ;; Return an empty X-Face by default.
      (setq x-face ",\\m{?h\\)X")))
  (if x-face-use-uncompface-internal
      (let ((bits (uncompface-internal x-face t))
	    (index 0)
	    bytes)
	(if bit-reverse
	    (while (< index 2304)
	      (push (+ (if (aref bits index) 1 0)
		       (if (aref bits (1+ index)) 2 0)
		       (if (aref bits (+ 2 index)) 4 0)
		       (if (aref bits (+ 3 index)) 8 0)
		       (if (aref bits (+ 4 index)) 16 0)
		       (if (aref bits (+ 5 index)) 32 0)
		       (if (aref bits (+ 6 index)) 64 0)
		       (if (aref bits (+ 7 index)) 128 0))
		    bytes)
	      (setq index (+ 8 index)))
	  (while (< index 2304)
	    (push (+ (if (aref bits index) 128 0)
		     (if (aref bits (1+ index)) 64 0)
		     (if (aref bits (+ 2 index)) 32 0)
		     (if (aref bits (+ 3 index)) 16 0)
		     (if (aref bits (+ 4 index)) 8 0)
		     (if (aref bits (+ 5 index)) 4 0)
		     (if (aref bits (+ 6 index)) 2 0)
		     (if (aref bits (+ 7 index)) 1 0))
		  bytes)
	    (setq index (+ 8 index))))
	(if bool-vector
	    (x-face-string-to-bool-vector (concat (nreverse bytes)))
	  (concat (nreverse bytes))))
    (with-temp-buffer
      (insert x-face)
      (call-process-region (point-min) (point-max)
			   uncompface-program t '(t nil))
      (goto-char (point-min))
      (setq case-fold-search t)
      (while (search-forward "0x" nil t)
	(delete-char -2))
      (goto-char (point-min))
      (while (re-search-forward "[^[:xdigit:]]+" nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (insert "\"")
      (while (not (eobp))
	(insert "\\x")
	(forward-char 2))
      (insert "\"")
      (goto-char (point-min))
      (if bit-reverse
	  (let ((data (read (current-buffer))))
	    (dolist (byte (prog1
			      (append data nil)
			    (setq data nil)))
	      (push (aref x-face-mirror byte) data))
	    (if bool-vector
		(x-face-string-to-bool-vector (concat (nreverse data)))
	      (concat (nreverse data))))
	(if bool-vector
	    (x-face-string-to-bool-vector (read (buffer-string)))
	  (string-as-unibyte (read (current-buffer))))))))

;;;###autoload
(defun x-face-bitmap-to-pbm (bitmap &optional plain)
  "Convert a raw BITMAP to a PBM format.
BITMAP may be a 2304-bit bool-vector or a 288-byte binary string.  If
PLAIN is non-nil, it will return a plain PBM format, otherwise a raw
PBM format."
  (if plain
      (let ((ix 0)
	    iy
	    (iz 6)
	    byte)
	(when (bool-vector-p bitmap)
	  (setq bitmap (x-face-bool-vector-to-string bitmap)))
	(with-temp-buffer
	  (insert "P1\n48 48\n")
	  (while (< ix 288)
	    (setq byte (aref bitmap ix)
		  ix (1+ ix)
		  iy 128)
	    (while (> iy 0)
	      (insert (if (zerop (logand byte iy))
			  "0"
			"1"))
	      (setq iy (lsh iy -1)))
	    (when (zerop (setq iz (1- iz)))
	      (insert "\n")
	      (setq iz 6)))
	  (buffer-string)))
    (concat "P4\n48 48\n" (if (bool-vector-p bitmap)
			      (x-face-bool-vector-to-string bitmap)
			    bitmap))))

;;;###autoload
(defun x-face-to-pbm (x-face &optional plain cleaned)
  "Convert an X-FACE to a PBM format.
If PLAIN is non-nil, it will return a plain PBM format, otherwise a
raw PBM format.  If CLEANED is non-nil, it is assumed that X-FACE
includes neither the X-Face: header, whitespace nor newlines."
  (x-face-bitmap-to-pbm (x-face-to-bitmap x-face nil nil cleaned) plain))

;;;###autoload
(defun x-face-bitmap-to-xbm (bitmap &optional filename)
  "Convert a raw BITMAP to an XBM format.
BITMAP may be a 2304-bit bool-vector or a 288-byte binary string.
FILENAME is used to be identifiers in file contents which defaults to
X_Face."
  (when (bool-vector-p bitmap)
    (setq bitmap (x-face-bool-vector-to-string bitmap)))
  (setq filename (if filename
		     (replace-regexp-in-string "[\000-/:-\?[-^`{-\377]" "_"
					       filename)
		   "X_Face"))
  (with-temp-buffer
    (insert "\
#define " filename "_width 48
#define " filename "_height 48
static char " filename "_bits[] = {
 ")
    (let ((ix 0)
	  (iy 15))
      (while (< ix 288)
	(insert (format "0x%02x," (aref x-face-mirror (aref bitmap ix))))
	(setq ix (1+ ix))
	(when (zerop (setq iy (1- iy)))
	  (insert "\n ")
	  (setq iy 15)))
      (delete-backward-char 1)
      (insert "};\n"))
    (buffer-string)))

;;;###autoload
(defun x-face-to-xbm (x-face &optional filename cleaned)
  "Convert an X-FACE to an XBM format.
FILENAME is used to be identifiers in file contents which defaults to
X_Face.  If CLEANED is non-nil, it is assumed that X-FACE includes
neither the X-Face: header, whitespace nor newlines."
  (x-face-bitmap-to-xbm (x-face-to-bitmap x-face nil nil cleaned) filename))

;;;###autoload
(defun x-face-gray-x-faces-to-pixmap (x-faces &optional sorted)
  "Convert gray X-FACES to a pixmap.
Pixmap is a vector which contains 288-pixel raw gray map.  Each X-FACE
is an encoded X-Face string which may or may not include the X-Face:
header, whitespace or newlines.  If SORTED is non-nil, it is assumed
that X-FACES are sorted in order from msb to lsb."
  (setq x-faces (if sorted
		    (x-face-cleanup-x-faces x-faces)
		  (x-face-sort-gray-x-faces x-faces)))
  (dolist (x-face (prog1
		      x-faces
		    (setq x-faces nil)))
    (push (x-face-to-bitmap x-face t t t) x-faces))
  (setq x-faces (nreverse x-faces))
  (let* ((ix 0)
	 iy
	 (depth (length x-faces))
	 (pixmap (make-vector 2304 nil))
	 pixel)
    (while (< ix 2304)
      (setq iy 0
	    pixel 0)
      (while (< iy depth)
	(setq pixel (if (aref (nth iy x-faces) ix)
			(+ pixel pixel 1)
		      (* pixel 2))
	      iy (1+ iy)))
      (aset pixmap ix pixel)
      (setq ix (1+ ix)))
    pixmap))

;;;###autoload
(defun x-face-gray-pixmap-to-pgm (pixmap &optional maxval plain)
  "Convert a gray PIXMAP to a PGM format.
PIXMAP is a vector which should contain 288-byte raw gray map.  MAXVAL
is a number which specifies the white level.  If MAXVAL is nil, it
will be guessed by the values of PIXMAP.  If PLAIN is non-nil, it will
return a plain PGM format, otherwise a raw PGM format."
  (unless maxval
    (setq maxval (1- (expt 2 (1+ (logb (apply 'max (append pixmap nil))))))))
  (if plain
      (concat "P2\n48 48\n" (number-to-string maxval) "\n"
	      (mapconcat 'number-to-string pixmap " ") "\n")
    (concat "P5\n48 48\n" (number-to-string maxval) "\n" pixmap)))

;;;###autoload
(defun x-face-gray-pixmap-to-xpm (pixmap &optional filename ncolors)
  "Convert a gray PIXMAP to an XPM format.
PIXMAP is a vector which should contain 288-byte raw gray map.
FILENAME is used to be identifiers in file contents which defaults to
X_Face.  NCOLORS is a number which specifies how many colors PIXMAP
has.  If NCOLORS is nil, it will be guessed by PIXMAP itself.  Note
that it limits the image depth maximum to 6-bit, i.e., the lower bits
will be ignored."
  (unless ncolors
    (setq ncolors (expt 2 (1+ (logb (apply 'max (append pixmap nil)))))))
  (let ((ix 0)
	iy scale coeff cval)
    (if (> ncolors 64)
	(setq scale (/ 64.0 ncolors)
	      ncolors 64
	      coeff (/ 256.0 63))
      (setq coeff (/ 256.0 (1- ncolors))))
    (with-temp-buffer
      (insert "\
/* XPM */
static char * "
	      (if filename
		  (replace-regexp-in-string "[\000-/:-\?[-^`{-\377]" "_"
					    filename)
		"X_Face")
	      "[] = {
/* width height ncolors chars_per_pixel */
\"48 48 " (number-to-string ncolors) " 1\",
/* colors */
")
      (while (< ix ncolors)
	(setq cval (min (truncate (* ix coeff)) 255))
	(insert (format "\"%c c #%02x%02x%02x\",\n"
			(aref x-face-xpm-colors (- ncolors ix 1))
			cval cval cval))
	(setq ix (1+ ix)))
      (insert "\
/* pixels */
\"")
      (setq ix 0
	    iy 48)
      (if scale
	  (while (< ix 2304)
	    (insert (aref x-face-xpm-colors
			  (- ncolors (truncate (* (aref pixmap ix) scale)) 1)))
	    (setq ix (1+ ix))
	    (when (zerop (setq iy (1- iy)))
	      (insert "\",\n\"")
	      (setq iy 48)))
	(while (< ix 2304)
	  (insert (aref x-face-xpm-colors (- ncolors (aref pixmap ix) 1)))
	  (setq ix (1+ ix))
	  (when (zerop (setq iy (1- iy)))
	    (insert "\",\n\"")
	    (setq iy 48))))
      (delete-backward-char 3)
      (insert "\n};\n")
      (buffer-string))))

;;;###autoload
(defun x-face-gray-x-faces-to-xpm (x-faces &optional sorted filename)
  "Convert gray X-FACES to an XPM format.
Each X-FACE is an encoded X-Face string which may or may not include
the X-Face: header, whitespace or newlines.  If SORTED is non-nil, it
is assumed that X-FACES are sorted in order from msb to lsb.  FILENAME
is used to be identifiers in file contents which defaults to X_Face."
  (x-face-gray-pixmap-to-xpm
   (x-face-gray-x-faces-to-pixmap x-faces sorted)
   filename (expt 2 (length x-faces))))

;;;###autoload
(defun x-face-create-image (x-face &rest props)
  "Create a PBM image from X-FACE.
X-FACE is an encoded X-Face string which may or may not include the
X-Face: header, whitespace or newlines.  The rest PROPS are additional
image attributes assigning to the image.  The value of the
`x-face-image-attributes' variable will be used to the default image
attributes.

Here are some examples of how to use this function:

;; Insert an image at point.
\(insert-image (x-face-create-image X-Face :KEYWORD VALUE ...))

;; Create an image without any properties.
\(let (x-face-image-attributes)
  (x-face-create-image X-Face))

;; Create a scaled image.
\(x-face-create-image X-Face :scale-factor 0.707)

;; Insert an image as an overlay.
\(let ((overlay (make-overlay (point) (1+ (point))))
      (image (x-face-create-image X-Face)))
  (overlay-put overlay 'evaporate t)
  (overlay-put overlay 'before-string (propertize \" \" 'display image)))
"
  (if (stringp x-face)
      (setq x-face (x-face-cleanup-x-face x-face))
    ;; Return an empty X-Face by default.
    (setq x-face ",\\m{?h\\)X"))
  (let* ((params (copy-sequence (cdr (assq (if (x-face-dark-background-p)
					       'dark
					     'light)
					   x-face-image-attributes))))
	 (scale (or (plist-get props :scale-factor)
		    (plist-get params :scale-factor)
		    1))
	 bg fg should-colorize siblings image)
    (while props
      (setq params (plist-put params (pop props) (pop props))))
    (setq params (x-face-cleanup-plist (plist-put params :scale-factor nil))
	  bg (or (plist-get params :background)
		 (face-background 'default)
		 "#ffffff")
	  fg (or (plist-get params :foreground)
		 (face-foreground 'default)
		 "#000000")
	  should-colorize
	  (and x-face-pgmtoppm-program
	       (not (and (string-match
			  "^white$\\|^gr[ae]y100$\\|^#\\(?:fff\\)+$" bg)
			 (string-match
			  "^black$\\|^gr[ae]y0$\\|^#\\(?:000\\)+$" fg)))))
    (plist-put params :background bg)
    (plist-put params :foreground fg)
    (when x-face-enable-cache
      (if x-face-ring
	  (setq siblings (cdr (assoc x-face (ring-elements x-face-ring)))
		image (cdr (assoc scale siblings)))
	(setq x-face-ring (make-ring x-face-ring-size))))
    (unless image
      ;; Create an image.
      (setq image
	    (if (and x-face-pnmscale-program
		     (/= 1 scale))
		(progn
		  (setq image (x-face-to-bitmap x-face nil nil t))
		  (let ((coding-system-for-read 'binary)
			(coding-system-for-write 'binary)
			default-enable-multibyte-characters)
		    (with-temp-buffer
		      (insert "P4\n48 48\n" image)
		      (call-process-region (point-min) (point-max)
					   x-face-pnmscale-program
					   t '(t nil) nil
					   (number-to-string scale))
		      (when should-colorize
			(call-process-region (point-min) (point-max)
					     x-face-pgmtoppm-program
					     t '(t nil) nil
					     (concat fg "-" bg)))
		      (create-image (buffer-string) 'pbm t))))
	      (create-image (x-face-to-pbm x-face) 'pbm t)))
      (when x-face-enable-cache
	(if siblings
	    (push (cons scale image) siblings)
	  (ring-insert x-face-ring (list x-face (cons scale image))))))
    (append image params)))

;;;###autoload
(defun x-face-create-gray-image (x-faces &optional sorted)
  "Convert gray X-FACES to an image in the PGM format.
Each X-FACE should be cleaned up that the X-Face: header has been
stripped.  For caching images, stripping also whitespace and newlines
from each X-FACE string is recommended.  If SORTED is non-nil, it is
assumed that X-FACES are sorted in order from msb to lsb."
  (setq x-faces (if sorted
		    (x-face-cleanup-x-faces x-faces)
		  (x-face-sort-gray-x-faces x-faces)))
  (let* ((params (copy-sequence (cdr (assq (if (x-face-dark-background-p)
					       'dark
					     'light)
					   x-face-image-attributes))))
	 (size (or (plist-get params :scale-factor)
		   1))
	 siblings image)
    (dolist (face (prog1
		      (nreverse x-faces)
		    (setq x-faces nil)))
      (push (apply 'concat (split-string face)) x-faces))
    (when x-face-enable-cache
      (if x-face-ring
	  (setq siblings (cdr (assoc x-faces (ring-elements x-face-ring)))
		image (cdr (assoc size siblings)))
	(setq x-face-ring (make-ring x-face-ring-size))))
    (unless image
      (if (and x-face-pnmscale-program
	       (/= 1 size))
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary)
		default-enable-multibyte-characters)
	    (with-temp-buffer
	      (insert (x-face-gray-pixmap-to-pgm
		       (x-face-gray-x-faces-to-pixmap x-faces t)
		       (1- (expt 2 (length x-faces)))))
	      (call-process-region (point-min) (point-max)
				   x-face-pnmscale-program t '(t nil) nil
				   (number-to-string size))
	      (setq image (create-image (buffer-string) 'pbm t))))
	(setq image (create-image (x-face-gray-pixmap-to-pgm
				   (x-face-gray-x-faces-to-pixmap x-faces t)
				   (1- (expt 2 (length x-faces))))
				  'pbm t)))
      (when x-face-enable-cache
	(if siblings
	    (push (cons size image) siblings)
	  (ring-insert x-face-ring (list x-faces (cons size image))))))
    (plist-put params :background nil)
    (plist-put params :foreground nil)
    (plist-put params :scale-factor nil)
    (append image (x-face-cleanup-plist params))))

;;;###autoload
(defun x-face-create-face-image (face &rest props)
  "Create a PNG (or PPM) image from FACE.
FACE is a base64 encoded PNG Face string which may or may not include
the Face: header, whitespace or newlines.  The rest PROPS are
additional image attributes assigning to the image.  The value of the
`x-face-image-attributes' variable will be used to the default image
attributes.  It returns a PPM image rather than a PNG image if an
image is scaled."
  (let* ((params (copy-sequence (cdr (assq (if (x-face-dark-background-p)
					       'dark
					     'light)
					   x-face-image-attributes))))
	 (scale (or (plist-get props :scale-factor)
		    (plist-get params :scale-factor)
		    1))
	 (empty "iVBORw0KGgoAAAANSUhEUgAAADAAAAAwAQAAAAB/e\
cQqAAAAEElEQVR4nGP4DwYMoxR1KABPVB7waCGvfwAAAABJRU5ErkJggg==")
	 siblings image)
    (if (stringp face)
	(setq face (x-face-cleanup-x-face face))
      ;; Return an empty image by default.
      (setq face empty))
    (while props
      (setq params (plist-put params (pop props) (pop props))))
    (when x-face-enable-cache
      (if x-face-ring
	  (setq siblings (cdr (assoc face (ring-elements x-face-ring)))
		image (cdr (assoc scale siblings)))
	(setq x-face-ring (make-ring x-face-ring-size))))
    (unless image
      ;; Create an image.
      (setq image (condition-case nil
		      (base64-decode-string face)
		    (error
		     (base64-decode-string empty))))
      (setq image
	    (if (and x-face-pngtopnm-program
		     x-face-pnmscale-program
		     (/= 1 scale))
		(progn
		  (let ((coding-system-for-read 'binary)
			(coding-system-for-write 'binary)
			default-enable-multibyte-characters)
		    (with-temp-buffer
		      (insert image)
		      (call-process-region (point-min) (point-max)
					   x-face-pngtopnm-program
					   t '(t nil))
		      (call-process-region (point-min) (point-max)
					   x-face-pnmscale-program
					   t '(t nil) nil
					   (number-to-string scale))
		      (create-image (buffer-string) 'pbm t))))
	      (create-image image 'png t)))
      (when x-face-enable-cache
	(if siblings
	    (push (cons scale image) siblings)
	  (ring-insert x-face-ring (list face (cons scale image))))))
    (plist-put params :background nil)
    (plist-put params :foreground nil)
    (plist-put params :scale-factor nil)
    (append image (x-face-cleanup-plist params))))

(eval-when-compile
  (defvar last))

;;;###autoload
(defun x-face-decode-message-header (&optional beg end buffer ignore)
  "Display X-Face images in the current message.
Optional BEG and END are no more than placeholders to keep the backward
compatibility.  If optional BUFFER is specified, it is assumed that the
raw X-Face headers can be found in the BUFFER.  BUFFER can also be a
function which is similar to the `x-face-possibly-change-buffer'
function.  The optional IGNORE specifies the symbol of the type which
should be ignored.  The valid values include nil, `face' and `x-face'.
This requires a support for images in your Emacs and the external
`uncompface' program or the ELisp based `uncompface' program."
  (when (and (display-images-p)
	     (image-type-available-p 'pbm))
    (let ((inhibit-point-motion-hooks t)
	  start images)
      (save-excursion
	;; FIXME: non-nil value of `ignore' means it is called in the
	;; Gnus article buffer.  That is kludge, should be fixed.
	(unless ignore
	  ;; Remove all existing X-Face images.
	  (save-restriction
	    (x-face-narrow-to-head)
	    (x-face-remove-x-face-images)))
	(setq x-face-show-buffer (current-buffer))
	;; Change buffer to where the raw X-Face headers can be found.
	(cond ((functionp buffer)
	       (funcall buffer))
	      ((x-face-buffer-live-p buffer)
	       (set-buffer buffer))
	      (t
	       (x-face-perhaps-change-buffer)))
	;; Extract X-Faces in header.
	(save-restriction
	  (x-face-narrow-to-head)
	  (let ((regexp (cond ((eq 'face ignore)
			       "X-Face\\(-[[:digit:]]+\\)?:")
			      ((eq 'x-face ignore)
			       "Face:")
			      (t
			       "\\(X-\\)?Face\\(-[[:digit:]]+\\)?:")))
		(case-fold-search t)
		type face faces x-faces)
	    (while (progn
		     (while (not (or (eobp)
				     (looking-at regexp)))
		       (forward-line 1))
		     (not (eobp)))
	      (setq start (point)
		    type (cond ((or (eq 'x-face ignore)
				    (not (or ignore
					     (match-beginning 1))))
				'png)
			       ((or (and ignore
					 (match-beginning 1))
				    (match-beginning 2))
				'gray)))
	      (while (progn
		       (forward-line 1)
		       (memq (char-after) '(?\t ?\ ))))
	      (setq face (buffer-substring-no-properties start (point)))
	      (store-match-data nil)
	      (string-match "\\([\000- ]\\)+\\([!-~]\\)?" face)
	      (if (match-beginning 2)
		  (if (eq 'png type)
		      (push face faces)
		    (push face x-faces))
		;; If a header has an empty body, use the encoded empty data.
		(push (concat (substring face 0 (match-beginning 0))
			      " ,\\m{?h\\)X")
		      x-faces)))
	    (if (eq type 'gray)
		(setq images (list (x-face-create-gray-image x-faces)))
	      (while x-faces
		(push (x-face-create-image
		       (apply 'concat (cdr (split-string (pop x-faces)))))
		      images)))
	    (while faces
	      (push (x-face-create-face-image
		     (apply 'concat (cdr (split-string (pop faces)))))
		    images)))))
      (when (x-face-buffer-live-p x-face-working-buffer)
	(kill-buffer x-face-working-buffer))
      (when images
	;; Insert X-Face images.
	(save-excursion
	  (set-buffer x-face-show-buffer)
	  (let ((case-fold-search t)
		(inhibit-read-only t)
		(mod (buffer-modified-p)))
	    (save-restriction
	      (x-face-narrow-to-head)
	      (let ((from (if (re-search-forward "^From:" nil t)
			      (prog1
				  (match-end 0)
				(goto-char (point-min)))
			    (point))))
		;; Hide raw X-Face headers.
		(let ((icon (plist-get x-face-hidden-properties 'display)))
		  (when icon
		    (setq icon (copy-sequence icon))
		    (let ((type (plist-get (cdr icon) :type))
			  (outline (if (x-face-dark-background-p)
				       (or (plist-get (cdar images)
						      :background)
					   "#ffffff")
				     (or (plist-get (cdar images)
						    :foreground)
					 "#000000"))))
		      (cond ((eq 'xpm type)
			     (plist-put (cdr icon) :color-symbols
					(list (cons "outline" outline))))
			    ((memq type '(pbm xbm))
			     (plist-put (cdr icon) :foreground outline)))))
		  (let ((regexp (if x-face-hide-related-headers
				    "\\(X-\\)?Face.*: ?"
				  "\\(X-\\)?Face\\(-[[:digit:]]+\\)?: ?")))
		    (while (progn
			     (while (not (or (eobp) (looking-at regexp)))
			       (forward-line 1))
			     (not (eobp)))
		      (setq start (if icon
				      (match-end 0)
				    (point)))
		      (while (progn
			       (forward-line 1)
			       (memq (char-after) '(?\t ?\ ))))
		      (if icon
			  (x-face-set-properties start (1- (point))
						 (plist-put
						  (copy-sequence
						   x-face-hidden-properties)
						  'display icon)
						 'x-face-invisible)
			(if (and (boundp 'gnus-article-buffer)
				 (eq (current-buffer)
				     (get-buffer (symbol-value
						  'gnus-article-buffer))))
			    ;; We should not put the hidden properties at
			    ;; the last newline since it will be left when
			    ;; sorting header is done.
			    (x-face-set-properties start (1- (point))
						   x-face-hidden-properties
						   'x-face-invisible)
			  (x-face-set-properties start (point)
						 x-face-hidden-properties
						 'x-face-invisible))))))
		(goto-char from)))
	    (if x-face-use-overlay
		(let ((overlay (make-overlay (point) (1+ (point))))
		      (rest ""))
		  (setq images (nreverse images))
		  (while images
		    ;; There is a face `default' used to hide an
		    ;; underline text behind an image.
		    (setq rest (concat (propertize " " 'display
						   (pop images) 'face 'default)
				       rest)))
		  (overlay-put overlay 'before-string
			       (if (bolp)
				   (concat rest "\n")
				 rest))
		  (overlay-put overlay 'x-face-image t)
		  (overlay-put overlay 'priority x-face-most-positive-fixnum)
		  (overlay-put overlay 'evaporate t))
	      (while images
		(setq start (point))
		(when (prog1
			  (when (bolp)
			    (insert "From:")
			    t)
			(insert-image (pop images)))
		  (insert " X-Face\n"))
		(put-text-property start (point) 'x-face-image t)))
	    (set-buffer-modified-p mod)))))
    (when (and (boundp 'gnus-article-buffer)
	       (eq (current-buffer) (symbol-value 'gnus-article-buffer)))
      (set 'last t))))

;;;###autoload
(defun x-face-show (&optional arg)
  "Toggle showing X-Face images.  With ARG, turn showing on if and only
if ARG is positive."
  (interactive "P")
  (if (if (numberp arg)
	  (> arg 0)
	(not (or (and (numberp arg) (< arg 0))
		 ;; There is no From header in the buffer.
		 (get-text-property (point-min) 'x-face-image)
		 (save-excursion
		   (save-restriction
		     (x-face-narrow-to-head)
		     (or (let ((overlays (overlays-in (point-min)
						      (point-max))))
			   (while (and overlays
				       (not (overlay-get (car overlays)
							 'x-face-image)))
			     (setq overlays (cdr overlays)))
			   overlays)
			 (text-property-any (point-min) (point-max)
					    'x-face-image t)))))))
      ;; The non-nil value for `message-strip-special-text-properties'
      ;; (by default) prevents hiding raw X-Face headers (some users
      ;; likely show her/his own face in the message buffer :-).
      (let (message-strip-special-text-properties)
	(x-face-decode-message-header))
    (x-face-remove-x-face-images)
    (x-face-expose-hidden-text)))

;;;###autoload
(defun x-face-turn-off ()
  "Remove X-face images from the buffer."
  (x-face-show -1))

(defvar x-face-read-file-name-type 'xbm
  "Internal variable used to say what a type of image files are preferred.
The `x-face-read-file-name' function refers it.")

(defun x-face-read-file-name-completion-handler (operation &rest args)
  "Internal function used to handle completing names of image files.
The `x-face-read-file-name-type' variable controls what a type of
image files should be handled."
  (let ((inhibit-file-name-handlers
	 (cons 'x-face-read-file-name-completion-handler
	       inhibit-file-name-handlers))
	(inhibit-file-name-operation operation)
	(partial (car args))
	(dir (cadr args))
	(regexp (format "\\(?:/\\|\\.%s\\(?:\\.bz2\\|\\.gz\\)?\\)\\'"
			x-face-read-file-name-type))
	(case-fold-search t)
	candidates)
    (if (eq 'file-name-completion operation)
	(try-completion
	 partial
	 (dolist (name (file-name-all-completions partial dir) candidates)
	   (setq candidates (nconc candidates (list (list name))))))
      (if (eq 'file-name-all-completions operation)
	  (dolist (name (file-name-all-completions partial dir) candidates)
	    (when (string-match regexp name)
	      (setq candidates (nconc candidates (list name)))))
	(apply operation args)))))

;;;###autoload
(defun x-face-read-file-name (prompt &optional dir default-filename
				     mustmatch initial)
  "Read an image file name.
This function is equivalent to `read-file-name', except that it limits
to image files.  The `x-face-read-file-name-type' variable controls
what a type of image files should be read.  Note that since the
`file-name-history' variable will not be updated in this function, use
the `x-face-put-file-name-in-history' variable to register the actual
file name after it has been decided."
  (let ((file-name-handler-alist
	 (cons (cons "" 'x-face-read-file-name-completion-handler)
	       file-name-handler-alist))
	(regexp (format "\\(?:/\\|\\.%s\\(?:\\.bz2\\|\\.gz\\)?\\)\\'"
			x-face-read-file-name-type))
	(case-fold-search t)
	(file-name-history file-name-history)
	file history)
    (while (setq file (pop file-name-history))
      (when (string-match regexp file)
	(push file history)))
    (setq file-name-history (nreverse history))
    (read-file-name prompt
		    ;; The 2nd arg DIR should have the trailing "/"
		    ;; because the 5th arg INITIAL will be appended it.
		    (file-name-as-directory dir)
		    default-filename mustmatch initial)))

(defun x-face-put-file-name-in-history (filename)
  "Put FILENAME in `file-name-history'."
  (let ((abbrev (when (and abbreviated-home-dir
			   (string-match abbreviated-home-dir filename))
		  (replace-match "~/" nil nil filename))))
    (unless (or (member filename file-name-history)
		(and abbrev
		     (member abbrev file-name-history)))
      (push (or abbrev filename) file-name-history))))

(defun x-face-fill-headers ()
  "Fill X-Face headers in the buffer.
The buffer is expected to be narrowed to just the X-Face headers."
  (goto-char (point-min))
  (let (begin width)
    (if (looking-at "\\(X-\\)?Face\\(-[[:digit:]]+\\)?:")
	(setq begin (match-end 0))
      (insert "X-Face:")
      (setq begin (point)))
    (while (not (eobp))
      (while (progn
	       (forward-line 1)
	       (and (not (eobp))
		    (not (looking-at "\\(X-\\)?Face\\(-[[:digit:]]+\\)?:")))))
      (save-restriction
	(narrow-to-region begin (point))
	(setq begin (match-end 0))
	(goto-char (point-min))
	(while (re-search-forward "[[:blank:]\n]" nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
	(goto-char (point-max))
	(insert "\n")
	(goto-char (point-min)))
      (setq width (if (= 5 (current-column))
		      77
		    79))
      (insert " ")
      (while (and (= width (move-to-column width))
		  (not (eolp)))
	(insert "\n "))
      (forward-line 1))))

;;;###autoload
(defun x-face-insert (&optional xbm-file keep-existing-headers)
  "Insert XBM-FILE as an X-Face header.
If XBM-FILE is omitted, the value of `x-face-default-xbm-file' will be
used for it.  If the contents of a file do not look like the XBM
format, they will be regarded as pre-encoded data.  If optional
KEEP-EXISTING-HEADERS is non-nil, existing X-Face headers will not be
removed.  This requires the external `compface' program.  It will
override the value of the `x-face-use-overlay' variable to t
buffer-locally."
  (interactive "*i\nP")
  (unless (and (stringp xbm-file) (> (length xbm-file) 0))
    (let (dir default)
      (when (and (stringp x-face-default-xbm-file)
		 (> (length x-face-default-xbm-file) 0))
	(setq dir (file-name-directory x-face-default-xbm-file)
	      default
	      (if (and dir
		       (let ((inode1 (nth 10 (file-attributes dir)))
			     (inode2 (nth 10 (file-attributes
					      x-face-image-file-directory))))
			 (and inode1 inode2 (equal inode1 inode2))))
		  (file-name-nondirectory x-face-default-xbm-file)
		x-face-default-xbm-file)))
      (if (or (interactive-p)
	      x-face-insert-query-file-name-when-no-argument)
	  (setq xbm-file
		(let ((x-face-read-file-name-type "\\(?:png\\|xbm\\)"))
		  (x-face-read-file-name
		   (concat "XBM (or PNG) file"
			   (if default
			       (concat " (default " default "): ")
			     ": "))
		   (or dir x-face-image-file-directory) default t)))
	(when default
	  (setq xbm-file x-face-default-xbm-file))))
    (unless (and (stringp xbm-file) (> (length xbm-file) 0))
      (x-face-insert-error 'file-error "No xbm file is specified")))
  (setq xbm-file (expand-file-name xbm-file x-face-image-file-directory))
  (when (interactive-p)
    (x-face-put-file-name-in-history xbm-file))
  (when (file-directory-p xbm-file)
    (x-face-insert-error 'file-error xbm-file "is a directory"))
  (or (file-exists-p xbm-file)
      (and (file-exists-p (concat xbm-file ".bz2"))
	   (setq xbm-file (concat xbm-file ".bz2")))
      (and (file-exists-p (concat xbm-file ".gz"))
	   (setq xbm-file (concat xbm-file ".gz")))
      (x-face-insert-error 'file-error "Cannot open XBM file:" xbm-file))
  (let (x-face)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (if (and (string-match "\\.bz2$\\|\\.gz$" xbm-file)
	       (not auto-compression-mode))
	  (progn
	    (auto-compression-mode 1)
	    (unwind-protect
		(insert-file-contents xbm-file)
	      (auto-compression-mode -1)))
	(if auto-image-file-mode
	    (progn
	      (auto-image-file-mode -1)
	      (insert-file-contents xbm-file)
	      (auto-image-file-mode 1))
	  (insert-file-contents xbm-file)))
      (setq case-fold-search t)
      (goto-char (point-min))
      (cond (;; xbm
	     (and
	      (search-forward "width" nil t)
	      (condition-case nil
		  (= 48 (read (current-buffer)))
		(error nil))
	      (goto-char (point-min))
	      (search-forward "height" nil t)
	      (condition-case nil
		  (= 48 (read (current-buffer)))
		(error nil))
	      (search-forward "{" nil t)
	      (progn
		(delete-region (point-min) (point))
		(while (search-forward "0x" nil t)
		  (delete-char -2))
		(goto-char (point-min))
		(while (re-search-forward "[^[:xdigit:]]+" nil t)
		  (delete-region (match-beginning 0) (match-end 0)))
		(= 576 (buffer-size))))
	     (goto-char (point-min))
	     (insert "\"")
	     (while (not (eobp))
	       (insert "\\x")
	       (forward-char 2))
	     (insert "\"")
	     (goto-char (point-min))
	     (dolist (byte (prog1
			       (append (read (current-buffer)) nil)
			     (erase-buffer)))
	       (insert (format "%02x" (aref x-face-mirror byte))))
	     (call-process-region (point-min) (point-max)
				  compface-program t '(t nil))
	     (goto-char (point-min))
	     (while (search-forward "\r" nil t)
	       (delete-char -1))
	     (goto-char (point-min))
	     (skip-chars-forward "\t\n ")
	     (delete-region (point-min) (point))
	     (insert "X-Face: ")
	     (goto-char (point-max))
	     (skip-chars-backward "\t\n ")
	     (delete-region (point) (point-max))
	     (insert "\n"))
	    (;; png
	     (looking-at "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a")
	     (base64-encode-region (point-min) (point-max) t)
	     (goto-char (point-min))
	     (insert "Face: ")
	     (x-face-fill-headers))
	    (;; encoded header
	     (and (> (buffer-size) 0)
		  (progn
		    (goto-char (point-min))
		    (skip-chars-forward "\t\n\v\f\r -~")
		    (eobp)))
	     ;; Cleanup file contents.
	     (goto-char (point-max))
	     (unless (bolp)
	       (insert "\n"))
	     (goto-char (point-min))
	     (while (re-search-forward "^[[:blank:]\r]*\n" nil t)
	       (delete-region (match-beginning 0) (match-end 0)))
	     (goto-char (point-min))
	     (while (re-search-forward "[[:blank:]\r]+$" nil t)
	       (delete-region (match-beginning 0) (match-end 0)))
	     (if (zerop (buffer-size))
		 (insert "X-Face: ,\\m{?h\\)X\n")
	       (x-face-fill-headers)))
	    (t
	     (x-face-insert-error 'file-error
				  "Unrecognized file format:" xbm-file)))
      (setq x-face (buffer-string)))
    (set (make-local-variable 'x-face-use-overlay) t)
    (set (make-local-variable 'x-face-hide-related-headers) nil)
    (let ((props (default-value 'x-face-hidden-properties)))
      (set (make-local-variable 'x-face-hidden-properties)
	   (when (and x-face-field-icon
		      (plist-get props 'invisible))
	     (list 'display (copy-sequence x-face-field-icon)
		   ;; It may be needless to make the raw field intangible.
		   'intangible nil ;; (plist-get props 'intangible)
		   ))))
    (save-excursion
      (save-restriction
	(let ((inhibit-point-motion-hooks t))
	  (x-face-narrow-to-head)
	  (let (x-faces)
	    (let ((case-fold-search t))
	      (while (progn
		       (while (not (or (eobp)
				       (looking-at "\\(X-\\)?Face:")))
			 (forward-line 1))
		       (not (eobp)))
		(push (cons (point)
			    (progn
			      (while (progn
				       (forward-line 1)
				       (memq (char-after) '(?\t ?\ ))))
			      (point)))
		      x-faces))
	      (goto-char (or (cdar x-faces) (point-max)))
	      (let ((inhibit-read-only t)
		    (keymap (current-local-map))
		    props start)
		(setq keymap (if keymap
				 (copy-keymap keymap)
			       (make-sparse-keymap)))
		(define-key keymap [mouse-2] 'x-face-show)
		(setq props (list 'local-map keymap
				  'mouse-face x-face-mouse-face
				  'help-echo
				  "mouse-2 toggles X-Face visibility"))
		(unless (bolp) (insert "\n"))
		(save-restriction
		  (narrow-to-region (point) (point))
		  (insert x-face)
		  (goto-char (point-min))
		  (search-forward " ")
		  (setq start (point))
		  (forward-line 1)
		  (while (not (eobp))
		    (while (memq (char-after) '(?\t ?\ ))
		      (forward-line 1))
		    (x-face-set-properties start (1- (point)) props)
		    (while (prog1
			       (not (or (eobp)
					(and (looking-at
					      "\\(X-\\)?Face:[[:blank:]\n]+")
					     (setq start (match-end 0)))))
			     (forward-line 1)))))
		(unless keep-existing-headers
		  (while (setq x-face (pop x-faces))
		    (delete-region (car x-face) (cdr x-face)))))))))))
  (when (if (functionp x-face-auto-image)
	    (funcall x-face-auto-image)
	  x-face-auto-image)
    (x-face-show 1)))

;;;###autoload
(defun x-face-save ()
  "Save X-Face headers to XBM, PNG or XPM files.
This requires the external `uncompface' program or the ELisp based
`uncompface' program.  It doesn't work with forwarded MIME parts,
except for Mew.  Files will be named uniquely and saved into the
directory specified by the `x-face-image-file-directory-for-save'
variable."
  (interactive)
  (let ((case-fold-search t)
	type x-faces basename)
    (save-excursion
      (x-face-possibly-change-buffer)
      (save-restriction
	(x-face-narrow-to-head)
	(let ((inhibit-point-motion-hooks t)
	      start)
	  (while (progn
		   (while (not (or (eobp)
				   (looking-at
				    "\\(X-\\)?Face\\(-[[:digit:]]+\\)?:")))
		     (forward-line 1))
		   (not (eobp)))
	    (setq start (point)
		  type (cond ((not (match-beginning 1))
			      'png)
			     ((match-beginning 2)
			      'xpm)
			     (t
			      'xbm)))
	    (while (progn
		     (forward-line 1)
		     (memq (char-after) '(?\t ?\ ))))
	    (push (cons type (buffer-substring-no-properties start (point)))
		  x-faces)))
	(unless (and (setq basename (mail-fetch-field "from"))
		     (setq basename (cadr (mail-extract-address-components
					   basename)))
		     (setq basename (replace-regexp-in-string
				     "[^\000-\176]" "~"
				     (replace-regexp-in-string
				      "[][!\"#$%&'()*/:;<>?\\`{|}]" "-"
				      (replace-regexp-in-string
				       "[\000-\040]+" "_"
				       (replace-regexp-in-string
					"[\t ]+" "" basename)))))
		     (not (string-equal basename "")))
	  (setq basename "x-face"))))
    (when (x-face-buffer-live-p x-face-working-buffer)
      (kill-buffer x-face-working-buffer))
    (if x-faces
	(let ((basename-re (concat "^" (regexp-quote basename)))
	      (num 0)
	      (log-buffer (get-buffer-create "*X-Face-Save-Log*"))
	      nform filename compr)
	  (if (file-directory-p x-face-image-file-directory-for-save)
	      (let ((files (directory-files
			    x-face-image-file-directory-for-save
			    nil basename-re t))
		    re file)
		(when files
		  (setq re (concat basename-re "-\\([[:digit:]]+\\)"))
		  (while (setq file (pop files))
		    (setq num (max num (if (string-match re file)
					   (1+ (string-to-number
						(match-string 1 file)))
					 1))))))
	    (make-directory x-face-image-file-directory-for-save t))
	  (with-current-buffer log-buffer
	    (setq case-fold-search nil)
	    (goto-char (point-max))
	    (unless (bolp) (insert "\n"))
	    (buffer-disable-undo))
	  (setq nform (concat "%s-%0" (number-to-string
				       (max 2 (length (number-to-string num))))
			      "d"))
	  (while x-faces
	    (setq type (caar x-faces)
		  filename (if (zerop num)
			       basename
			     (format nform basename num))
		  num (1+ num))
	    (with-temp-buffer
	      (set-buffer-multibyte nil)
	      (cond ((eq type 'xpm)
		     (insert (x-face-gray-x-faces-to-xpm (mapcar 'cdr x-faces)
							 nil filename))
		     (setq x-faces nil))
		    ((eq type 'png)
		     (insert (base64-decode-string
			      (substring (cdr (pop x-faces)) 5))))
		    (t
		     (insert (x-face-to-xbm (cdr (pop x-faces)) filename))))
	      (setq compr (unless (eq type 'png)
			    (cdr (assq x-face-compressor
				       '((bzip2 . \.bz2)
					 (gzip . \.gz)))))
		    filename (expand-file-name
			      (format "%s.%s%s" filename type (or compr ""))
			      x-face-image-file-directory-for-save))
	      (if (and compr
		       (require 'jka-compr)
		       (not auto-compression-mode))
		  (progn
		    (auto-compression-mode 1)
		    (unwind-protect
			(write-region (point-min) (point-max) filename)
		      (auto-compression-mode -1)))
		(write-region (point-min) (point-max) filename))
	      (when (interactive-p)
		(x-face-put-file-name-in-history filename)
		(when x-faces
		  (sit-for 0.5)))
	      (with-current-buffer log-buffer
		(insert (format-time-string "%Y-%m-%d %T ")
			(abbreviate-file-name filename)
			"\n")
		(bury-buffer log-buffer)))))
      (message "No X-Face headers found"))))

;;;###autoload
(defun x-face-ascii-view (&optional buffer)
  "Show X-Face images as ASCII pictures.
If optional BUFFER is specified, it is assumed that the raw X-Face
headers can be found in the BUFFER.  BUFFER can also be a function
similarly to `x-face-possibly-change-buffer'.  It does not work with
forwarded MIME parts, except for Mew.  If you are a Gnus user, you can
use this function as the main X-Face viewer as follows. :-p

\(setq gnus-article-x-face-command
      (lambda (&rest args) (x-face-ascii-view 'ignore)))

Note that this example can only be used with Gnus v 5.10.3 and later
or T-gnus 6.16.3 and later."
  (interactive)
  (let ((table (if (x-face-dark-background-p)
		   [?$ ?o ?\" ?\ ]
		 [?\  ?\" ?o ?$]))
	start faces bitmap config idx0 idx1 row column event)
    (save-excursion
      ;; Change buffer to where the raw X-Face headers can be found.
      (cond ((functionp buffer)
	     (funcall buffer))
	    ((x-face-buffer-live-p buffer)
	     (set-buffer buffer))
	    (t
	     (x-face-possibly-change-buffer)))
      ;; Extract X-Faces in header.
      (save-restriction
	(x-face-narrow-to-head)
	(while (progn
		 (while (not (or (eobp)
				 (looking-at "X-Face:")))
		   (forward-line 1))
		 (not (eobp)))
	  (setq start (point))
	  (while (progn
		   (forward-line 1)
		   (memq (char-after) '(?\t ?\ ))))
	  (push	(buffer-substring-no-properties start (point)) faces))))
    (when faces
      (setq faces (nreverse faces)
	    config (current-window-configuration))
      (delete-other-windows)
      (while faces
	(switch-to-buffer (setq buffer (generate-new-buffer "*X-Face*")))
	(setq line-spacing 0
	      bitmap (x-face-to-bitmap (pop faces) t t)
	      idx0 0
	      idx1 48
	      row 24)
	(while (> row 0)
	  (setq column 48)
	  (while (> column 0)
	    (insert-char (aref table (+ (if (aref bitmap idx0) 1 0)
					(if (aref bitmap idx1) 2 0)))
			 1)
	    (setq idx0 (1+ idx0)
		  idx1 (1+ idx1)
		  column (1- column)))
	  (insert "\n")
	  (setq idx0 idx1
		idx1 (+ idx1 48)
		row (1- row)))
	(set-buffer-modified-p nil)
	(goto-char 49)
	(set-window-start (selected-window) 1)
	(message "Press any key to continue (C to not kill the buffer)")
	(while (progn
		 (setq event (read-event))
		 (not (or (integerp event) (symbolp event)))))
	(discard-input)
	(unless (eq event ?C)
	  (kill-buffer buffer)))
      (set-window-configuration config))))

(eval-and-compile
  (autoload 'bbdb-current-record "bbdb-com")
  (autoload 'bbdb-record-getprop "bbdb")
  (autoload 'bbdb-record-name "bbdb"))

(defun x-face-energize-bbdb-buffer (&optional all silent)
  "Show X-Face and Face images in the BBDB buffer.
If optional ALL is not specified, it deals with only the current
record."
  (when x-face-bbdb-display
    (save-excursion
      (set-buffer (symbol-value 'bbdb-buffer-name))
      (save-restriction
	(unless all
	  (beginning-of-line)
	  (narrow-to-region (progn
			      (while (and (memq (char-after)
						'(nil ?\t ?\n ?\ ))
					  (zerop (forward-line -1))))
			      (point))
			    (progn
			      (while (progn
				       (forward-line 1)
				       (memq (char-after) '(?\t ?\ ))))
			      (point))))
	(x-face-remove-x-face-images)
	(when (and x-face-bbdb-display
		   (display-images-p)
		   (image-type-available-p 'pbm))
	  (let ((buffer (get-buffer-create " *x-face-energize-bbdb*"))
		(x-face-use-overlay t)
		(picon (when x-face-field-icon
			 (let* ((icon (copy-sequence x-face-field-icon))
				(type (plist-get (cdr icon) :type))
				(bgmode (if (x-face-dark-background-p)
					    'dark
					  'light))
				(params
				 (x-face-cleanup-plist
				  (cdr (assq bgmode x-face-image-attributes))))
				(outline (if (eq bgmode 'dark)
					     (or (plist-get params
							    :background)
						 "#ffffff")
					   (or (plist-get params :foreground)
					       "#000000"))))
			   (cond ((eq 'xpm type)
				  (plist-put (cdr icon) :color-symbols
					     (list (cons "outline" outline))))
				 ((memq type '(pbm xbm))
				  (plist-put (cdr icon) :foreground outline)))
			   icon)))
		(x-face-image-attributes (x-face-image-attributes-for-bbdb))
		;; Space with the face `default' used to stop leaking
		;; of neighboring faces.
		(space (propertize " " 'face 'default))
		record faces msg nfaces start end from to icon)
	    (goto-char (point-min))
	    (unwind-protect
		(while (not (eobp))
		  (when all
		    ;; Go to the beginning or the next recored.
		    (while (and (looking-at "[[:blank:]]\\|[[:blank:]]*$")
				(zerop (forward-line 1)))))
		  (when (and (not (eobp))
			     (setq record (bbdb-current-record))
			     (setq faces (bbdb-record-getprop record 'face)))
		    (when (and all
			       (not silent))
		      (setq msg (format "Extracting X-Face(s) for %s..."
					(bbdb-record-name record)))
		      (message "%s" msg))
		    (setq faces (split-string faces)
			  nfaces (length faces))
		    (with-current-buffer buffer
		      (erase-buffer)
		      (insert "From: X\n")
		      (dolist (face faces)
			(insert (if (string= "iVBORw0K" ;; PNG header
					     (substring face 0 8))
				    "Face: "
				  "X-Face: ")
				face "\n"))
		      (x-face-decode-message-header)
		      (setq faces (overlay-properties (car (overlays-at 6)))))
		    (plist-put faces 'before-string
			       (concat (plist-get faces 'before-string)
				       space))
		    (x-face-set-properties (point) (1+ (point)) faces)
		    (forward-line 1)
		    (when picon
		      ;; Iconify the raw X-Face records.
		      (while (not (or (when (looking-at "[[:blank:]]+face:")
					(goto-char (match-end 0)))
				      (looking-at "[^[:blank:]\n]*$")
				      (eobp)))
			(forward-line 1))
		      (unless (or (bolp) (eobp))
			(setq start (point))
			(while (progn
				 (forward-line 1)
				 ;; There might not be a blank line between
				 ;; records when BBDB v2.33 and later is
				 ;; running.
				 (not (or (looking-at "\
\[^[:blank:]\n]\\|[[:blank:]]+[^[:blank:]\n:]+:\
\\(?:[[:blank:]]\\|[[:blank:]]*$\\)")
					  (eobp)))))
			(setq end (point))
			(goto-char start)
			(while (re-search-forward "[^[:blank:]\n]+" end t)
			  (setq from (match-beginning 0)
				to (match-end 0)
				nfaces (1- nfaces))
			  (if (zerop nfaces)
			      ;; Use the parent icon directly.
			      (setq icon picon)
			    (when (and (looking-at "[[:blank:]\n]+")
				       (< (match-end 0) end))
			      (setq to (match-end 0)
				    icon (copy-sequence picon))))
			  (x-face-set-properties from to
						 (list 'display icon
						       'intangible t
						       'x-face-image t))))))
		  (forward-line 1))
	      (when msg
		(message "%sdone" msg))
	      (kill-buffer buffer))))))))

(let (current-load-list)
  (defadvice bbdb-display-records-1
    (around show-x-face-images-in-the-bbdb-buffer activate compile)
    "Advised by X-Face-E21.  Show X-Faces images in the BBDB buffer."
    (let ((silent (or (and (boundp 'bbdb-gag-messages)
			   (symbol-value 'bbdb-gag-messages))
		      (and (boundp 'bbdb-silent-running)
			   (symbol-value 'bbdb-silent-running)))))
      (unless silent
	(message "Formatting..."))
      (let ((bbdb-silent-running t)
	    (bbdb-list-hook bbdb-list-hook))
	(remove-hook 'bbdb-list-hook 'x-face-energize-bbdb-buffer)
	ad-do-it)
      (x-face-energize-bbdb-buffer 'all silent)
      (unless silent
	(message "Formatting...done")))))

(add-hook 'bbdb-list-hook 'x-face-energize-bbdb-buffer)

(provide 'x-face-e21)

;;; x-face-e21.el ends here

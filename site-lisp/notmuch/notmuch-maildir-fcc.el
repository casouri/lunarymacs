;;; notmuch-maildir-fcc.el ---

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use this as the fcc handler for message-mode,
;; customize the notmuch-fcc-dirs variable

;;; Code:

(eval-when-compile (require 'cl))
(require 'message)

(require 'notmuch-lib)

(defvar notmuch-maildir-fcc-count 0)

(defcustom notmuch-fcc-dirs "sent"
 "Determines the Fcc Header which says where to save outgoing mail.

Three types of values are permitted:

- nil: no Fcc header is added,

- a string: the value of `notmuch-fcc-dirs' is the Fcc header to
  be used.

- a list: the folder is chosen based on the From address of the
  current message using a list of regular expressions and
  corresponding folders:

     ((\"Sebastian@SSpaeth.de\" . \"privat\")
      (\"spaetz@sspaeth.de\" . \"OUTBOX.OSS\")
      (\".*\" . \"defaultinbox\"))

  If none of the regular expressions match the From address, no
  Fcc header will be added.

If `notmuch-maildir-use-notmuch-insert' is set (the default) then
the header should be of the form \"folder +tag1 -tag2\" where
folder is the folder (relative to the notmuch mailstore) to store
the message in, and tag1 and tag2 are tag changes to apply to the
stored message. This string is split using `split-string-and-unquote',
so a folder name containing spaces can be specified by
quoting each space with an immediately preceding backslash
or surrounding the entire folder name in double quotes.

If `notmuch-maildir-use-notmuch-insert' is nil then the Fcc
header should be the directory where the message should be
saved. A relative directory will be understood to specify a
directory within the notmuch mail store, (as set by the
database.path option in the notmuch configuration file).

In all cases you will be prompted to create the folder or
directory if it does not exist yet when sending a mail."

 :type '(choice
	 (const :tag "No FCC header" nil)
	 (string :tag "A single folder")
	 (repeat :tag "A folder based on the From header"
		 (cons regexp (string :tag "Folder"))))
 :require 'notmuch-fcc-initialization
 :group 'notmuch-send)

(defcustom notmuch-maildir-use-notmuch-insert 't
  "Should fcc use notmuch insert instead of simple fcc"
  :type '(choice :tag "Fcc Method"
		 (const :tag "Use notmuch insert" t)
		 (const :tag "Use simple fcc" nil))
  :group 'notmuch-send)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions which set up the fcc header in the message buffer.

(defun notmuch-fcc-header-setup ()
  "Add an Fcc header to the current message buffer.

Sets the Fcc header based on the values of `notmuch-fcc-dirs'.

Originally intended to be use a hook function, but now called directly
by notmuch-mua-mail"

  (let ((subdir
	 (cond
	  ((or (not notmuch-fcc-dirs)
	       (message-field-value "Fcc"))
	   ;; Nothing set or an existing header.
	   nil)

	  ((stringp notmuch-fcc-dirs)
	   notmuch-fcc-dirs)

	  ((and (listp notmuch-fcc-dirs)
		(stringp (car notmuch-fcc-dirs)))
	   ;; Old style - no longer works.
	   (error "Invalid `notmuch-fcc-dirs' setting (old style)"))

	  ((listp notmuch-fcc-dirs)
	   (let* ((from (message-field-value "From"))
		  (match
		   (catch 'first-match
		     (dolist (re-folder notmuch-fcc-dirs)
		       (when (string-match-p (car re-folder) from)
			 (throw 'first-match re-folder))))))
	     (if match
		 (cdr match)
	       (message "No Fcc header added.")
	       nil)))

	  (t
	   (error "Invalid `notmuch-fcc-dirs' setting (neither string nor list)")))))

    (when subdir
      (if notmuch-maildir-use-notmuch-insert
	  (notmuch-maildir-add-notmuch-insert-style-fcc-header subdir)
	(notmuch-maildir-add-file-style-fcc-header subdir)))))

(defun notmuch-maildir-add-notmuch-insert-style-fcc-header (subdir)
  ;; Notmuch insert does not accept absolute paths, so check the user
  ;; really want this header inserted.

  (when (or (not (= (elt subdir 0) ?/))
	    (y-or-n-p (format "Fcc header %s is an absolute path and notmuch insert is requested.\nInsert header anyway? "
			      subdir)))
    (message-add-header (concat "Fcc: " subdir))))

(defun notmuch-maildir-add-file-style-fcc-header (subdir)
  (message-add-header
   (concat "Fcc: "
	   (file-truename
	    ;; If the resulting directory is not an absolute path,
	    ;; prepend the standard notmuch database path.
	    (if (= (elt subdir 0) ?/)
		subdir
	      (concat (notmuch-database-path) "/" subdir))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for saving a message either using notmuch insert or file
;; fcc. First functions common to the two cases.

(defmacro with-temporary-notmuch-message-buffer (&rest body)
  "Set-up a temporary copy of the current message-mode buffer."
  `(let ((case-fold-search t)
	 (buf (current-buffer))
	 (mml-externalize-attachments message-fcc-externalize-attachments))
     (with-current-buffer (get-buffer-create " *message temp*")
       (erase-buffer)
       (insert-buffer-substring buf)
       ,@body)))

(defun notmuch-maildir-setup-message-for-saving ()
  "Setup message for saving. Should be called on a temporary copy.

This is taken from the function message-do-fcc."
  (message-encode-message-body)
  (save-restriction
    (message-narrow-to-headers)
    (let ((mail-parse-charset message-default-charset))
      (mail-encode-encoded-word-buffer)))
  (goto-char (point-min))
  (when (re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "$")
	 nil t)
    (replace-match "" t t )))

(defun notmuch-maildir-message-do-fcc ()
  "Process Fcc headers in the current buffer.

This is a rearranged version of message mode's message-do-fcc."
  (let (list file)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(setq file (message-fetch-field "fcc" t)))
      (when file
	(with-temporary-notmuch-message-buffer
	 (save-restriction
	   (message-narrow-to-headers)
	   (while (setq file (message-fetch-field "fcc" t))
	     (push file list)
	     (message-remove-header "fcc" nil t)))
	 (notmuch-maildir-setup-message-for-saving)
	 ;; Process FCC operations.
	 (while list
	   (setq file (pop list))
	   (notmuch-fcc-handler file))
	 (kill-buffer (current-buffer)))))))

(defun notmuch-fcc-handler (fcc-header)
  "Store message with notmuch insert or normal (file) fcc.

If `notmuch-maildir-use-notmuch-insert` is set then store the
message using notmuch insert. Otherwise store the message using
normal fcc."
  (message "Doing Fcc...")
  (if notmuch-maildir-use-notmuch-insert
      (notmuch-maildir-fcc-with-notmuch-insert fcc-header)
    (notmuch-maildir-fcc-file-fcc fcc-header)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for saving a message using notmuch insert.

(defun notmuch-maildir-notmuch-insert-current-buffer (folder &optional create tags)
  "Use notmuch insert to put the current buffer in the database.

This inserts the current buffer as a message into the notmuch
database in folder FOLDER. If CREATE is non-nil it will supply
the --create-folder flag to create the folder if necessary. TAGS
should be a list of tag changes to apply to the inserted message."
  (let* ((args (append (when create (list "--create-folder"))
		       (list (concat "--folder=" folder))
		       tags)))
    (apply 'notmuch-call-notmuch-process
	   :stdin-string (buffer-string) "insert" args)))

(defun notmuch-maildir-fcc-with-notmuch-insert (fcc-header &optional create)
  "Store message with notmuch insert.

The fcc-header should be of the form \"folder +tag1 -tag2\" where
folder is the folder (relative to the notmuch mailstore) to store
the message in, and tag1 and tag2 are tag changes to apply to the
stored message. This string is split using `split-string-and-unquote',
so a folder name containing spaces can be specified by
quoting each space with an immediately preceding backslash
or surrounding the entire folder name in double quotes.

If CREATE is non-nil then create the folder if necessary."
  (let* ((args (split-string-and-unquote fcc-header))
	 (folder (car args))
	 (tags (cdr args)))
    (condition-case nil
	(notmuch-maildir-notmuch-insert-current-buffer folder create tags)
      ;; Since there are many reasons notmuch insert could fail, e.g.,
      ;; locked database, non-existent folder (which could be due to a
      ;; typo, or just the user want a new folder, let the user decide
      ;; how to deal with it.
      (error
       (let ((response (notmuch-read-char-choice
			"Insert failed: (r)etry, (c)reate folder, (i)gnore, or (e)dit the header? "
			'(?r ?c ?i ?e))))
	 (case response
	       (?r (notmuch-maildir-fcc-with-notmuch-insert fcc-header))
	       (?c (notmuch-maildir-fcc-with-notmuch-insert fcc-header 't))
	       (?i 't)
	       (?e (notmuch-maildir-fcc-with-notmuch-insert
		    (read-from-minibuffer "Fcc header: " fcc-header)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for saving a message using file fcc.

(defun notmuch-maildir-fcc-host-fixer (hostname)
  (replace-regexp-in-string "/\\|:"
			    (lambda (s)
			      (cond ((string-equal s "/") "\\057")
				    ((string-equal s ":") "\\072")
				    (t s)))
			    hostname
			    t
			    t))

(defun notmuch-maildir-fcc-make-uniq-maildir-id ()
   (let* ((ftime (float-time))
	  (microseconds (mod (* 1000000 ftime) 1000000))
	  (hostname (notmuch-maildir-fcc-host-fixer (system-name))))
     (setq notmuch-maildir-fcc-count (+ notmuch-maildir-fcc-count 1))
     (format "%d.%d_%d_%d.%s"
	     ftime
	     (emacs-pid)
	     microseconds
	     notmuch-maildir-fcc-count
	     hostname)))

(defun notmuch-maildir-fcc-dir-is-maildir-p (dir)
  (and (file-exists-p (concat dir "/cur/"))
       (file-exists-p (concat dir "/new/"))
       (file-exists-p (concat dir "/tmp/"))))

(defun notmuch-maildir-fcc-create-maildir (path)
  (cond ((or (not (file-exists-p path)) (file-directory-p path))
	 (make-directory (concat path "/cur/") t)
	 (make-directory (concat path "/new/") t)
	 (make-directory (concat path "/tmp/") t))
	((file-regular-p path)
	 (error "%s is a file. Can't create maildir." path))
	(t
	 (error "I don't know how to create a maildir here"))))

(defun notmuch-maildir-fcc-save-buffer-to-tmp (destdir)
  "Returns the msg id of the message written to the temp directory
if successful, nil if not."
  (let ((msg-id (notmuch-maildir-fcc-make-uniq-maildir-id)))
    (while (file-exists-p (concat destdir "/tmp/" msg-id))
      (setq msg-id (notmuch-maildir-fcc-make-uniq-maildir-id)))
    (cond ((notmuch-maildir-fcc-dir-is-maildir-p destdir)
	   (write-file (concat destdir "/tmp/" msg-id))
	   msg-id)
	  (t
	   (error (format "Can't write to %s. Not a maildir."
			  destdir))
	   nil))))

(defun notmuch-maildir-fcc-move-tmp-to-new (destdir msg-id)
  (add-name-to-file
   (concat destdir "/tmp/" msg-id)
   (concat destdir "/new/" msg-id ":2,")))

(defun notmuch-maildir-fcc-move-tmp-to-cur (destdir msg-id &optional mark-seen)
  (add-name-to-file
   (concat destdir "/tmp/" msg-id)
   (concat destdir "/cur/" msg-id ":2," (when mark-seen "S"))))

(defun notmuch-maildir-fcc-file-fcc (fcc-header)
  "Write the message to the file specified by FCC-HEADER.

It offers the user a chance to correct the header, or filesystem,
if needed."
  (if (notmuch-maildir-fcc-dir-is-maildir-p fcc-header)
      (notmuch-maildir-fcc-write-buffer-to-maildir fcc-header 't)
    ;; The fcc-header is not a valid maildir see if the user wants to
    ;; fix it in some way.
    (let* ((prompt (format "Fcc %s is not a maildir: (r)etry, (c)reate folder, (i)gnore, or  (e)dit the header? "
			   fcc-header))
	    (response (notmuch-read-char-choice prompt '(?r ?c ?i ?e))))
	 (case response
	       (?r (notmuch-maildir-fcc-file-fcc fcc-header))
	       (?c (if (file-writable-p fcc-header)
		       (notmuch-maildir-fcc-create-maildir fcc-header)
		     (message "No permission to create %s." fcc-header)
		     (sit-for 2))
		   (notmuch-maildir-fcc-file-fcc fcc-header))
	       (?i 't)
	       (?e (notmuch-maildir-fcc-file-fcc
		    (read-from-minibuffer "Fcc header: " fcc-header)))))))

(defun notmuch-maildir-fcc-write-buffer-to-maildir (destdir &optional mark-seen)
  "Writes the current buffer to maildir destdir. If mark-seen is
non-nil, it will write it to cur/, and mark it as read. It should
return t if successful, and nil otherwise."
  (let ((orig-buffer (buffer-name)))
    (with-temp-buffer
      (insert-buffer-substring orig-buffer)
      (catch 'link-error
	(let ((msg-id (notmuch-maildir-fcc-save-buffer-to-tmp destdir)))
	  (when msg-id
	    (cond (mark-seen
		   (condition-case err
		       (notmuch-maildir-fcc-move-tmp-to-cur destdir msg-id t)
		     (file-already-exists
		      (throw 'link-error nil))))
		  (t
		   (condition-case err
		       (notmuch-maildir-fcc-move-tmp-to-new destdir msg-id)
		     (file-already-exists
		      (throw 'link-error nil))))))
	  (delete-file (concat destdir "/tmp/" msg-id))))
      t)))

(provide 'notmuch-maildir-fcc)

;;; notmuch-maildir-fcc.el ends here

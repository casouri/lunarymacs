;;; notmuch-lib.el --- common variables, functions and function declarations
;;
;; Copyright © Carl Worth
;;
;; This file is part of Notmuch.
;;
;; Notmuch is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Notmuch is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Notmuch.  If not, see <https://www.gnu.org/licenses/>.
;;
;; Authors: Carl Worth <cworth@cworth.org>

;; This is an part of an emacs-based interface to the notmuch mail system.

;;; Code:

(require 'mm-util)
(require 'mm-view)
(require 'mm-decode)
(require 'cl)
(require 'notmuch-compat)

(unless (require 'notmuch-version nil t)
  (defconst notmuch-emacs-version "unknown"
    "Placeholder variable when notmuch-version.el[c] is not available."))

(autoload 'notmuch-jump-search "notmuch-jump"
  "Jump to a saved search by shortcut key." t)

(defgroup notmuch nil
  "Notmuch mail reader for Emacs."
  :group 'mail)

(defgroup notmuch-hello nil
  "Overview of saved searches, tags, etc."
  :group 'notmuch)

(defgroup notmuch-search nil
  "Searching and sorting mail."
  :group 'notmuch)

(defgroup notmuch-show nil
  "Showing messages and threads."
  :group 'notmuch)

(defgroup notmuch-send nil
  "Sending messages from Notmuch."
  :group 'notmuch)

(custom-add-to-group 'notmuch-send 'message 'custom-group)

(defgroup notmuch-tag nil
  "Tags and tagging in Notmuch."
  :group 'notmuch)

(defgroup notmuch-crypto nil
  "Processing and display of cryptographic MIME parts."
  :group 'notmuch)

(defgroup notmuch-hooks nil
  "Running custom code on well-defined occasions."
  :group 'notmuch)

(defgroup notmuch-external nil
  "Running external commands from within Notmuch."
  :group 'notmuch)

(defgroup notmuch-address nil
  "Address completion."
  :group 'notmuch)

(defgroup notmuch-faces nil
  "Graphical attributes for displaying text"
  :group 'notmuch)

(defcustom notmuch-command "notmuch"
  "Name of the notmuch binary.

This can be a relative or absolute path to the notmuch binary.
If this is a relative path, it will be searched for in all of the
directories given in `exec-path' (which is, by default, based on
$PATH)."
  :type 'string
  :group 'notmuch-external)

(defcustom notmuch-search-oldest-first t
  "Show the oldest mail first when searching.

This variable defines the default sort order for displaying
search results. Note that any filtered searches created by
`notmuch-search-filter' retain the search order of the parent
search."
  :type 'boolean
  :group 'notmuch-search)

(defcustom notmuch-poll-script nil
  "[Deprecated] Command to run to incorporate new mail into the notmuch database.

This option has been deprecated in favor of \"notmuch new\"
hooks (see man notmuch-hooks).  To change the path to the notmuch
binary, customize `notmuch-command'.

This variable controls the action invoked by
`notmuch-poll-and-refresh-this-buffer' (bound by default to 'G')
to incorporate new mail into the notmuch database.

If set to nil (the default), new mail is processed by invoking
\"notmuch new\". Otherwise, this should be set to a string that
gives the name of an external script that processes new mail. If
set to the empty string, no command will be run.

The external script could do any of the following depending on
the user's needs:

1. Invoke a program to transfer mail to the local mail store
2. Invoke \"notmuch new\" to incorporate the new mail
3. Invoke one or more \"notmuch tag\" commands to classify the mail"
  :type '(choice (const :tag "notmuch new" nil)
		 (const :tag "Disabled" "")
		 (string :tag "Custom script"))
  :group 'notmuch-external)

;;

(defvar notmuch-search-history nil
  "Variable to store notmuch searches history.")

(defcustom notmuch-archive-tags '("-inbox")
  "List of tag changes to apply to a message or a thread when it is archived.

Tags starting with \"+\" (or not starting with either \"+\" or
\"-\") in the list will be added, and tags starting with \"-\"
will be removed from the message or thread being archived.

For example, if you wanted to remove an \"inbox\" tag and add an
\"archived\" tag, you would set:
    (\"-inbox\" \"+archived\")"
  :type '(repeat string)
  :group 'notmuch-search
  :group 'notmuch-show)

(defvar notmuch-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'notmuch-help)
    (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
    (define-key map "s" 'notmuch-search)
    (define-key map "z" 'notmuch-tree)
    (define-key map "m" 'notmuch-mua-new-mail)
    (define-key map "=" 'notmuch-refresh-this-buffer)
    (define-key map (kbd "M-=") 'notmuch-refresh-all-buffers)
    (define-key map "G" 'notmuch-poll-and-refresh-this-buffer)
    (define-key map "j" 'notmuch-jump-search)
    map)
  "Keymap shared by all notmuch modes.")

;; By default clicking on a button does not select the window
;; containing the button (as opposed to clicking on a widget which
;; does). This means that the button action is then executed in the
;; current selected window which can cause problems if the button
;; changes the buffer (e.g., id: links) or moves point.
;;
;; This provides a button type which overrides mouse-action so that
;; the button's window is selected before the action is run. Other
;; notmuch buttons can get the same behaviour by inheriting from this
;; button type.
(define-button-type 'notmuch-button-type
  'mouse-action (lambda (button)
		  (select-window (posn-window (event-start last-input-event)))
		  (button-activate button)))

(defun notmuch-command-to-string (&rest args)
  "Synchronously invoke \"notmuch\" with the given list of arguments.

If notmuch exits with a non-zero status, output from the process
will appear in a buffer named \"*Notmuch errors*\" and an error
will be signaled.

Otherwise the output will be returned"
  (with-temp-buffer
    (let* ((status (apply #'call-process notmuch-command nil t nil args))
	   (output (buffer-string)))
      (notmuch-check-exit-status status (cons notmuch-command args) output)
      output)))

(defvar notmuch--cli-sane-p nil
  "Cache whether the CLI seems to be configured sanely.")

(defun notmuch-cli-sane-p ()
  "Return t if the cli seems to be configured sanely."
  (unless notmuch--cli-sane-p
    (let ((status (call-process notmuch-command nil nil nil
				"config" "get" "user.primary_email")))
      (setq notmuch--cli-sane-p (= status 0))))
  notmuch--cli-sane-p)

(defun notmuch-assert-cli-sane ()
  (unless (notmuch-cli-sane-p)
    (notmuch-logged-error
     "notmuch cli seems misconfigured or unconfigured."
"Perhaps you haven't run \"notmuch setup\" yet? Try running this
on the command line, and then retry your notmuch command")))

(defun notmuch-cli-version ()
  "Return a string with the notmuch cli command version number."
  (let ((long-string
	 ;; Trim off the trailing newline.
	 (substring (notmuch-command-to-string "--version") 0 -1)))
    (if (string-match "^notmuch\\( version\\)? \\(.*\\)$"
		      long-string)
	(match-string 2 long-string)
      "unknown")))

(defun notmuch-config-get (item)
  "Return a value from the notmuch configuration."
  (let* ((val (notmuch-command-to-string "config" "get" item))
	 (len (length val)))
    ;; Trim off the trailing newline (if the value is empty or not
    ;; configured, there will be no newline)
    (if (and (> len 0) (= (aref val (- len 1)) ?\n))
	(substring val 0 -1)
      val)))

(defun notmuch-database-path ()
  "Return the database.path value from the notmuch configuration."
  (notmuch-config-get "database.path"))

(defun notmuch-user-name ()
  "Return the user.name value from the notmuch configuration."
  (notmuch-config-get "user.name"))

(defun notmuch-user-primary-email ()
  "Return the user.primary_email value from the notmuch configuration."
  (notmuch-config-get "user.primary_email"))

(defun notmuch-user-other-email ()
  "Return the user.other_email value (as a list) from the notmuch configuration."
  (split-string (notmuch-config-get "user.other_email") "\n" t))

(defun notmuch-user-emails ()
  (cons (notmuch-user-primary-email) (notmuch-user-other-email)))

(defun notmuch-poll ()
  "Run \"notmuch new\" or an external script to import mail.

Invokes `notmuch-poll-script', \"notmuch new\", or does nothing
depending on the value of `notmuch-poll-script'."
  (interactive)
  (if (stringp notmuch-poll-script)
      (unless (string= notmuch-poll-script "")
	(unless (equal (call-process notmuch-poll-script nil nil) 0)
	  (error "Notmuch: poll script `%s' failed!" notmuch-poll-script)))
    (notmuch-call-notmuch-process "new")))

(defun notmuch-bury-or-kill-this-buffer ()
  "Undisplay the current buffer.

Bury the current buffer, unless there is only one window showing
it, in which case it is killed."
  (interactive)
  (if (> (length (get-buffer-window-list nil nil t)) 1)
      (bury-buffer)
    (kill-buffer)))

(defun notmuch-documentation-first-line (symbol)
  "Return the first line of the documentation string for SYMBOL."
  (let ((doc (documentation symbol)))
    (if doc
	(with-temp-buffer
	  (insert (documentation symbol t))
	  (goto-char (point-min))
	  (let ((beg (point)))
	    (end-of-line)
	    (buffer-substring beg (point))))
      "")))

(defun notmuch-prefix-key-description (key)
  "Given a prefix key code, return a human-readable string representation.

This is basically just `format-kbd-macro' but we also convert ESC to M-."
  (let* ((key-vector (if (vectorp key) key (vector key)))
	 (desc (format-kbd-macro key-vector)))
    (if (string= desc "ESC")
	"M-"
      (concat desc " "))))


(defun notmuch-describe-key (actual-key binding prefix ua-keys tail)
  "Prepend cons cells describing prefix-arg ACTUAL-KEY and ACTUAL-KEY to TAIL

It does not prepend if ACTUAL-KEY is already listed in TAIL."
  (let ((key-string (concat prefix (format-kbd-macro actual-key))))
    ;; We don't include documentation if the key-binding is
    ;; over-ridden. Note, over-riding a binding automatically hides the
    ;; prefixed version too.
    (unless (assoc key-string tail)
      (when (and ua-keys (symbolp binding)
		 (get binding 'notmuch-prefix-doc))
	;; Documentation for prefixed command
	(let ((ua-desc (key-description ua-keys)))
	  (push (cons (concat ua-desc " " prefix (format-kbd-macro actual-key))
		      (get binding 'notmuch-prefix-doc))
		tail)))
      ;; Documentation for command
      (push (cons key-string
		  (or (and (symbolp binding) (get binding 'notmuch-doc))
		      (notmuch-documentation-first-line binding)))
	    tail)))
    tail)

(defun notmuch-describe-remaps (remap-keymap ua-keys base-keymap prefix tail)
  ;; Remappings are represented as a binding whose first "event" is
  ;; 'remap.  Hence, if the keymap has any remappings, it will have a
  ;; binding whose "key" is 'remap, and whose "binding" is itself a
  ;; keymap that maps not from keys to commands, but from old (remapped)
  ;; functions to the commands to use in their stead.
  (map-keymap
   (lambda (command binding)
     (mapc
      (lambda (actual-key)
	(setq tail (notmuch-describe-key actual-key binding prefix ua-keys tail)))
      (where-is-internal command base-keymap)))
   remap-keymap)
  tail)

(defun notmuch-describe-keymap (keymap ua-keys base-keymap &optional prefix tail)
  "Return a list of cons cells, each describing one binding in KEYMAP.

Each cons cell consists of a string giving a human-readable
description of the key, and a one-line description of the bound
function.  See `notmuch-help' for an overview of how this
documentation is extracted.

UA-KEYS should be a key sequence bound to `universal-argument'.
It will be used to describe bindings of commands that support a
prefix argument.  PREFIX and TAIL are used internally."
  (map-keymap
   (lambda (key binding)
     (cond ((mouse-event-p key) nil)
	   ((keymapp binding)
	    (setq tail
		  (if (eq key 'remap)
		      (notmuch-describe-remaps
		       binding ua-keys base-keymap prefix tail)
		    (notmuch-describe-keymap
		     binding ua-keys base-keymap (notmuch-prefix-key-description key) tail))))
	   (binding
	    (setq tail (notmuch-describe-key (vector key) binding prefix ua-keys tail)))))
   keymap)
  tail)

(defun notmuch-substitute-command-keys (doc)
  "Like `substitute-command-keys' but with documentation, not function names."
  (let ((beg 0))
    (while (string-match "\\\\{\\([^}[:space:]]*\\)}" doc beg)
      (let ((desc
	     (save-match-data
	       (let* ((keymap-name (substring doc (match-beginning 1) (match-end 1)))
		      (keymap (symbol-value (intern keymap-name)))
		      (ua-keys (where-is-internal 'universal-argument keymap t))
		      (desc-alist (notmuch-describe-keymap keymap ua-keys keymap))
		      (desc-list (mapcar (lambda (arg) (concat (car arg) "\t" (cdr arg))) desc-alist)))
		 (mapconcat #'identity desc-list "\n")))))
	(setq doc (replace-match desc 1 1 doc)))
      (setq beg (match-end 0)))
    doc))

(defun notmuch-help ()
  "Display help for the current notmuch mode.

This is similar to `describe-function' for the current major
mode, but bindings tables are shown with documentation strings
rather than command names.  By default, this uses the first line
of each command's documentation string.  A command can override
this by setting the 'notmuch-doc property of its command symbol.
A command that supports a prefix argument can explicitly document
its prefixed behavior by setting the 'notmuch-prefix-doc property
of its command symbol."
  (interactive)
  (let* ((mode major-mode)
	 (doc (substitute-command-keys (notmuch-substitute-command-keys (documentation mode t)))))
    (with-current-buffer (generate-new-buffer "*notmuch-help*")
      (insert doc)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (view-buffer (current-buffer) 'kill-buffer-if-not-modified))))

(defun notmuch-subkeymap-help ()
  "Show help for a subkeymap."
  (interactive)
  (let* ((key (this-command-keys-vector))
	(prefix (make-vector (1- (length key)) nil))
	(i 0))
    (while (< i (length prefix))
      (aset prefix i (aref key i))
      (setq i (1+ i)))

    (let* ((subkeymap (key-binding prefix))
	   (ua-keys (where-is-internal 'universal-argument nil t))
	   (prefix-string (notmuch-prefix-key-description prefix))
	   (desc-alist (notmuch-describe-keymap subkeymap ua-keys subkeymap prefix-string))
	   (desc-list (mapcar (lambda (arg) (concat (car arg) "\t" (cdr arg))) desc-alist))
	   (desc (mapconcat #'identity desc-list "\n")))
      (with-help-window (help-buffer)
	(with-current-buffer standard-output
	  (insert "\nPress 'q' to quit this window.\n\n")
	  (insert desc)))
      (pop-to-buffer (help-buffer)))))

(defvar notmuch-buffer-refresh-function nil
  "Function to call to refresh the current buffer.")
(make-variable-buffer-local 'notmuch-buffer-refresh-function)

(defun notmuch-refresh-this-buffer ()
  "Refresh the current buffer."
  (interactive)
  (when notmuch-buffer-refresh-function
    ;; Pass prefix argument, etc.
    (call-interactively notmuch-buffer-refresh-function)))

(defun notmuch-poll-and-refresh-this-buffer ()
  "Invoke `notmuch-poll' to import mail, then refresh the current buffer."
  (interactive)
  (notmuch-poll)
  (notmuch-refresh-this-buffer))

(defun notmuch-refresh-all-buffers ()
  "Invoke `notmuch-refresh-this-buffer' on all notmuch major-mode buffers.

The buffers are silently refreshed, i.e. they are not forced to
be displayed."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (when (memq buffer-mode '(notmuch-show-mode
				notmuch-tree-mode
				notmuch-search-mode
				notmuch-hello-mode))
	(with-current-buffer buffer
	  (notmuch-refresh-this-buffer))))))

(defun notmuch-prettify-subject (subject)
  ;; This function is used by `notmuch-search-process-filter' which
  ;; requires that we not disrupt its' matching state.
  (save-match-data
    (if (and subject
	     (string-match "^[ \t]*$" subject))
	"[No Subject]"
      subject)))

(defun notmuch-sanitize (str)
  "Sanitize control character in STR.

This includes newlines, tabs, and other funny characters."
  (replace-regexp-in-string "[[:cntrl:]\x7f\u2028\u2029]+" " " str))

(defun notmuch-escape-boolean-term (term)
  "Escape a boolean term for use in a query.

The caller is responsible for prepending the term prefix and a
colon.  This performs minimal escaping in order to produce
user-friendly queries."

  (save-match-data
    (if (or (equal term "")
	    ;; To be pessimistic, only pass through terms composed
	    ;; entirely of ASCII printing characters other than ", (,
	    ;; and ).
	    (string-match "[^!#-'*-~]" term))
	;; Requires escaping
	(concat "\"" (replace-regexp-in-string "\"" "\"\"" term t t) "\"")
      term)))

(defun notmuch-id-to-query (id)
  "Return a query that matches the message with id ID."
  (concat "id:" (notmuch-escape-boolean-term id)))

(defun notmuch-hex-encode (str)
  "Hex-encode STR (e.g., as used by batch tagging).

This replaces spaces, percents, and double quotes in STR with
%NN where NN is the hexadecimal value of the character."
  (replace-regexp-in-string
   "[ %\"]" (lambda (match) (format "%%%02x" (aref match 0))) str))

;;

(defun notmuch-common-do-stash (text)
  "Common function to stash text in kill ring, and display in minibuffer."
  (if text
      (progn
	(kill-new text)
	(message "Stashed: %s" text))
    ;; There is nothing to stash so stash an empty string so the user
    ;; doesn't accidentally paste something else somewhere.
    (kill-new "")
    (message "Nothing to stash!")))

;;

(defun notmuch-remove-if-not (predicate list)
  "Return a copy of LIST with all items not satisfying PREDICATE removed."
  (let (out)
    (while list
      (when (funcall predicate (car list))
        (push (car list) out))
      (setq list (cdr list)))
    (nreverse out)))

(defun notmuch-plist-delete (plist property)
  (let* ((xplist (cons nil plist))
	 (pred xplist))
    (while (cdr pred)
      (when (eq (cadr pred) property)
	(setcdr pred (cdddr pred)))
      (setq pred (cddr pred)))
    (cdr xplist)))

(defun notmuch-split-content-type (content-type)
  "Split content/type into 'content' and 'type'"
  (split-string content-type "/"))

(defun notmuch-match-content-type (t1 t2)
  "Return t if t1 and t2 are matching content types, taking wildcards into account"
  (let ((st1 (notmuch-split-content-type t1))
	(st2 (notmuch-split-content-type t2)))
    (if (or (string= (cadr st1) "*")
	    (string= (cadr st2) "*"))
	;; Comparison of content types should be case insensitive.
	(string= (downcase (car st1)) (downcase (car st2)))
      (string= (downcase t1) (downcase t2)))))

(defvar notmuch-multipart/alternative-discouraged
  '(
    ;; Avoid HTML parts.
    "text/html"
    ;; multipart/related usually contain a text/html part and some associated graphics.
    "multipart/related"
    ))

(defun notmuch-multipart/alternative-determine-discouraged (msg)
  "Return the discouraged alternatives for the specified message."
  ;; If a function, return the result of calling it.
  (if (functionp notmuch-multipart/alternative-discouraged)
      (funcall notmuch-multipart/alternative-discouraged msg)
    ;; Otherwise simply return the value of the variable, which is
    ;; assumed to be a list of discouraged alternatives. This is the
    ;; default behaviour.
    notmuch-multipart/alternative-discouraged))

(defun notmuch-multipart/alternative-choose (msg types)
  "Return a list of preferred types from the given list of types
for this message, if present."
  ;; Based on `mm-preferred-alternative-precedence'.
  (let ((discouraged (notmuch-multipart/alternative-determine-discouraged msg))
	(seq types))
    (dolist (pref (reverse discouraged))
      (dolist (elem (copy-sequence seq))
	(when (string-match pref elem)
	  (setq seq (nconc (delete elem seq) (list elem))))))
    seq))

(defun notmuch-parts-filter-by-type (parts type)
  "Given a list of message parts, return a list containing the ones matching
the given type."
  (remove-if-not
   (lambda (part) (notmuch-match-content-type (plist-get part :content-type) type))
   parts))

(defun notmuch--get-bodypart-raw (msg part process-crypto binaryp cache)
  (let* ((plist-elem (if binaryp :content-binary :content))
	 (data (or (plist-get part plist-elem)
		   (with-temp-buffer
		     ;; Emacs internally uses a UTF-8-like multibyte string
		     ;; representation by default (regardless of the coding
		     ;; system, which only affects how it goes from outside data
		     ;; to this internal representation).  This *almost* never
		     ;; matters.  Annoyingly, it does matter if we use this data
		     ;; in an image descriptor, since Emacs will use its internal
		     ;; data buffer directly and this multibyte representation
		     ;; corrupts binary image formats.  Since the caller is
		     ;; asking for binary data, a unibyte string is a more
		     ;; appropriate representation anyway.
		     (when binaryp
		       (set-buffer-multibyte nil))
		     (let ((args `("show" "--format=raw"
				   ,(format "--part=%s" (plist-get part :id))
				   ,@(when process-crypto '("--decrypt=true"))
				   ,(notmuch-id-to-query (plist-get msg :id))))
			   (coding-system-for-read
			    (if binaryp 'no-conversion
			      (let ((coding-system (mm-charset-to-coding-system
						    (plist-get part :content-charset))))
				;; Sadly,
				;; `mm-charset-to-coding-system' seems
				;; to return things that are not
				;; considered acceptable values for
				;; `coding-system-for-read'.
				(if (coding-system-p coding-system)
				    coding-system
				  ;; RFC 2047 says that the default
				  ;; charset is US-ASCII. RFC6657
				  ;; complicates this somewhat.
				  'us-ascii)))))
		       (apply #'call-process notmuch-command nil '(t nil) nil args)
		       (buffer-string))))))
    (when (and cache data)
      (plist-put part plist-elem data))
    data))

(defun notmuch-get-bodypart-binary (msg part process-crypto &optional cache)
  "Return the unprocessed content of PART in MSG as a unibyte string.

This returns the \"raw\" content of the given part after content
transfer decoding, but with no further processing (see the
discussion of --format=raw in man notmuch-show).  In particular,
this does no charset conversion.

If CACHE is non-nil, the content of this part will be saved in
MSG (if it isn't already)."
  (notmuch--get-bodypart-raw msg part process-crypto t cache))

(defun notmuch-get-bodypart-text (msg part process-crypto &optional cache)
  "Return the text content of PART in MSG.

This returns the content of the given part as a multibyte Lisp
string after performing content transfer decoding and any
necessary charset decoding.

If CACHE is non-nil, the content of this part will be saved in
MSG (if it isn't already)."
  (notmuch--get-bodypart-raw msg part process-crypto nil cache))

;; Workaround: The call to `mm-display-part' below triggers a bug in
;; Emacs 24 if it attempts to use the shr renderer to display an HTML
;; part with images in it (demonstrated in 24.1 and 24.2 on Debian and
;; Fedora 17, though unreproducable in other configurations).
;; `mm-shr' references the variable `gnus-inhibit-images' without
;; first loading gnus-art, which defines it, resulting in a
;; void-variable error.  Hence, we advise `mm-shr' to ensure gnus-art
;; is loaded.
(if (>= emacs-major-version 24)
    (defadvice mm-shr (before load-gnus-arts activate)
      (require 'gnus-art nil t)
      (ad-disable-advice 'mm-shr 'before 'load-gnus-arts)
      (ad-activate 'mm-shr)))

(defun notmuch-mm-display-part-inline (msg part content-type process-crypto)
  "Use the mm-decode/mm-view functions to display a part in the
current buffer, if possible."
  (let ((display-buffer (current-buffer)))
    (with-temp-buffer
      ;; In case we already have :content, use it and tell mm-* that
      ;; it's already been charset-decoded by using the fake
      ;; `gnus-decoded' charset.  Otherwise, we'll fetch the binary
      ;; part content and let mm-* decode it.
      (let* ((have-content (plist-member part :content))
	     (charset (if have-content 'gnus-decoded
			(plist-get part :content-charset)))
	     (handle (mm-make-handle (current-buffer) `(,content-type (charset . ,charset)))))
	;; If the user wants the part inlined, insert the content and
	;; test whether we are able to inline it (which includes both
	;; capability and suitability tests).
	(when (mm-inlined-p handle)
	  (if have-content
	      (insert (notmuch-get-bodypart-text msg part process-crypto))
	    (insert (notmuch-get-bodypart-binary msg part process-crypto)))
	  (when (mm-inlinable-p handle)
	    (set-buffer display-buffer)
	    (mm-display-part handle)
	    t))))))

;; Converts a plist of headers to an alist of headers. The input plist should
;; have symbols of the form :Header as keys, and the resulting alist will have
;; symbols of the form 'Header as keys.
(defun notmuch-headers-plist-to-alist (plist)
  (loop for (key value . rest) on plist by #'cddr
	collect (cons (intern (substring (symbol-name key) 1)) value)))

(defun notmuch-face-ensure-list-form (face)
  "Return FACE in face list form.

If FACE is already a face list, it will be returned as-is.  If
FACE is a face name or face plist, it will be returned as a
single element face list."
  (if (and (listp face) (not (keywordp (car face))))
      face
    (list face)))

(defun notmuch-apply-face (object face &optional below start end)
  "Combine FACE into the 'face text property of OBJECT between START and END.

This function combines FACE with any existing faces between START
and END in OBJECT.  Attributes specified by FACE take precedence
over existing attributes unless BELOW is non-nil.

OBJECT may be a string, a buffer, or nil (which means the current
buffer).  If object is a string, START and END are 0-based;
otherwise they are buffer positions (integers or markers).  FACE
must be a face name (a symbol or string), a property list of face
attributes, or a list of these.  If START and/or END are omitted,
they default to the beginning/end of OBJECT.  For convenience
when applied to strings, this returns OBJECT."

  ;; A face property can have three forms: a face name (a string or
  ;; symbol), a property list, or a list of these two forms.  In the
  ;; list case, the faces will be combined, with the earlier faces
  ;; taking precedent.  Here we canonicalize everything to list form
  ;; to make it easy to combine.
  (let ((pos (cond (start start)
		   ((stringp object) 0)
		   (t 1)))
	(end (cond (end end)
		   ((stringp object) (length object))
		   (t (1+ (buffer-size object)))))
	(face-list (notmuch-face-ensure-list-form face)))
    (while (< pos end)
      (let* ((cur (get-text-property pos 'face object))
	     (cur-list (notmuch-face-ensure-list-form cur))
	     (new (cond ((null cur-list) face)
			(below (append cur-list face-list))
			(t (append face-list cur-list))))
	     (next (next-single-property-change pos 'face object end)))
	(put-text-property pos next 'face new object)
	(setq pos next))))
  object)

(defun notmuch-map-text-property (start end prop func &optional object)
  "Transform text property PROP using FUNC.

Applies FUNC to each distinct value of the text property PROP
between START and END of OBJECT, setting PROP to the value
returned by FUNC."
  (while (< start end)
    (let ((value (get-text-property start prop object))
	  (next (next-single-property-change start prop object end)))
      (put-text-property start next prop (funcall func value) object)
      (setq start next))))

(defun notmuch-logged-error (msg &optional extra)
  "Log MSG and EXTRA to *Notmuch errors* and signal MSG.

This logs MSG and EXTRA to the *Notmuch errors* buffer and
signals MSG as an error.  If EXTRA is non-nil, text referring the
user to the *Notmuch errors* buffer will be appended to the
signaled error.  This function does not return."

  (with-current-buffer (get-buffer-create "*Notmuch errors*")
    (goto-char (point-max))
    (unless (bobp)
      (newline))
    (save-excursion
      (insert "[" (current-time-string) "]\n" msg)
      (unless (bolp)
	(newline))
      (when extra
	(insert extra)
	(unless (bolp)
	  (newline)))))
  (error "%s" (concat msg (when extra
			    " (see *Notmuch errors* for more details)"))))

(defun notmuch-check-async-exit-status (proc msg &optional command err)
  "If PROC exited abnormally, pop up an error buffer and signal an error.

This is a wrapper around `notmuch-check-exit-status' for
asynchronous process sentinels.  PROC and MSG must be the
arguments passed to the sentinel.  COMMAND and ERR, if provided,
are passed to `notmuch-check-exit-status'.  If COMMAND is not
provided, it is taken from `process-command'."
  (let ((exit-status
	 (case (process-status proc)
	   ((exit) (process-exit-status proc))
	   ((signal) msg))))
    (when exit-status
      (notmuch-check-exit-status exit-status (or command (process-command proc))
				 nil err))))

(defun notmuch-check-exit-status (exit-status command &optional output err)
  "If EXIT-STATUS is non-zero, pop up an error buffer and signal an error.

If EXIT-STATUS is non-zero, pop up a notmuch error buffer
describing the error and signal an Elisp error.  EXIT-STATUS must
be a number indicating the exit status code of a process or a
string describing the signal that terminated the process (such as
returned by `call-process').  COMMAND must be a list giving the
command and its arguments.  OUTPUT, if provided, is a string
giving the output of command.  ERR, if provided, is the error
output of command.  OUTPUT and ERR will be included in the error
message."

  (cond
   ((eq exit-status 0) t)
   ((eq exit-status 20)
    (notmuch-logged-error "notmuch CLI version mismatch
Emacs requested an older output format than supported by the notmuch CLI.
You may need to restart Emacs or upgrade your notmuch Emacs package."))
   ((eq exit-status 21)
    (notmuch-logged-error "notmuch CLI version mismatch
Emacs requested a newer output format than supported by the notmuch CLI.
You may need to restart Emacs or upgrade your notmuch package."))
   (t
    (let* ((command-string
	    (mapconcat (lambda (arg)
			 (shell-quote-argument
			  (cond ((stringp arg) arg)
				((symbolp arg) (symbol-name arg))
				(t "*UNKNOWN ARGUMENT*"))))
		       command " "))
	   (extra
	    (concat "command: " command-string "\n"
	     (if (integerp exit-status)
		 (format "exit status: %s\n" exit-status)
	       (format "exit signal: %s\n" exit-status))
	     (when err
	       (concat "stderr:\n" err))
	     (when output
	       (concat "stdout:\n" output)))))
	(if err
	    ;; We have an error message straight from the CLI.
	    (notmuch-logged-error
	     (replace-regexp-in-string "[ \n\r\t\f]*\\'" "" err) extra)
	  ;; We only have combined output from the CLI; don't inundate
	  ;; the user with it.  Mimic `process-lines'.
	  (notmuch-logged-error (format "%s exited with status %s"
					(car command) exit-status)
				extra))
	;; `notmuch-logged-error' does not return.
	))))

(defun notmuch-call-notmuch--helper (destination args)
  "Helper for synchronous notmuch invocation commands.

This wraps `call-process'.  DESTINATION has the same meaning as
for `call-process'.  ARGS is as described for
`notmuch-call-notmuch-process'."

  (let (stdin-string)
    (while (keywordp (car args))
      (case (car args)
	(:stdin-string (setq stdin-string (cadr args)
			     args (cddr args)))
	(otherwise
	 (error "Unknown keyword argument: %s" (car args)))))
    (if (null stdin-string)
	(apply #'call-process notmuch-command nil destination nil args)
      (insert stdin-string)
      (apply #'call-process-region (point-min) (point-max)
	     notmuch-command t destination nil args))))

(defun notmuch-call-notmuch-process (&rest args)
  "Synchronously invoke `notmuch-command' with ARGS.

The caller may provide keyword arguments before ARGS.  Currently
supported keyword arguments are:

  :stdin-string STRING - Write STRING to stdin

If notmuch exits with a non-zero status, output from the process
will appear in a buffer named \"*Notmuch errors*\" and an error
will be signaled."
  (with-temp-buffer
    (let ((status (notmuch-call-notmuch--helper t args)))
      (notmuch-check-exit-status status (cons notmuch-command args)
				 (buffer-string)))))

(defun notmuch-call-notmuch-sexp (&rest args)
  "Invoke `notmuch-command' with ARGS and return the parsed S-exp output.

This is equivalent to `notmuch-call-notmuch-process', but parses
notmuch's output as an S-expression and returns the parsed value.
Like `notmuch-call-notmuch-process', if notmuch exits with a
non-zero status, this will report its output and signal an
error."

  (with-temp-buffer
    (let ((err-file (make-temp-file "nmerr")))
      (unwind-protect
	  (let ((status (notmuch-call-notmuch--helper (list t err-file) args))
		(err (with-temp-buffer
		       (insert-file-contents err-file)
		       (unless (eobp)
			 (buffer-string)))))
	    (notmuch-check-exit-status status (cons notmuch-command args)
				       (buffer-string) err)
	    (goto-char (point-min))
	    (read (current-buffer)))
	(delete-file err-file)))))

(defun notmuch-start-notmuch (name buffer sentinel &rest args)
  "Start and return an asynchronous notmuch command.

This starts and returns an asynchronous process running
`notmuch-command' with ARGS.  The exit status is checked via
`notmuch-check-async-exit-status'.  Output written to stderr is
redirected and displayed when the process exits (even if the
process exits successfully).  NAME and BUFFER are the same as in
`start-process'.  SENTINEL is a process sentinel function to call
when the process exits, or nil for none.  The caller must *not*
invoke `set-process-sentinel' directly on the returned process,
as that will interfere with the handling of stderr and the exit
status."

  (let (err-file err-buffer proc err-proc
	;; Find notmuch using Emacs' `exec-path'
	(command (or (executable-find notmuch-command)
		     (error "Command not found: %s" notmuch-command))))
    (if (fboundp 'make-process)
	(progn
	  (setq err-buffer (generate-new-buffer " *notmuch-stderr*"))
	  ;; Emacs 25 and newer has `make-process', which allows
	  ;; redirecting stderr independently from stdout to a
	  ;; separate buffer. As this allows us to avoid using a
	  ;; temporary file and shell invocation, use it when
	  ;; available.
	  (setq proc (make-process
		      :name name
		      :buffer buffer
		      :command (cons command args)
		      :connection-type 'pipe
		      :stderr err-buffer)
		err-proc (get-buffer-process err-buffer))
	  (process-put proc 'err-buffer err-buffer)

	  (process-put err-proc 'err-file err-file)
	  (process-put err-proc 'err-buffer err-buffer)
	  (set-process-sentinel err-proc #'notmuch-start-notmuch-error-sentinel))

      ;; On Emacs versions before 25, there is no way to capture
      ;; stdout and stderr separately for asynchronous processes, or
      ;; even to redirect stderr to a file, so we use a trivial shell
      ;; wrapper to send stderr to a temporary file and clean things
      ;; up in the sentinel.
      (setq err-file (make-temp-file "nmerr"))
      (let ((process-connection-type nil)) ;; Use a pipe
	(setq proc (apply #'start-process name buffer
			  "/bin/sh" "-c"
			  "exec 2>\"$1\"; shift; exec \"$0\" \"$@\""
			  command err-file args)))
      (process-put proc 'err-file err-file))

    (process-put proc 'sub-sentinel sentinel)
    (process-put proc 'real-command (cons notmuch-command args))
    (set-process-sentinel proc #'notmuch-start-notmuch-sentinel)
    proc))

(defun notmuch-start-notmuch-sentinel (proc event)
  "Process sentinel function used by `notmuch-start-notmuch'."
  (let* ((err-file (process-get proc 'err-file))
	 (err-buffer (or (process-get proc 'err-buffer)
			 (find-file-noselect err-file)))
	 (err (when (not (zerop (buffer-size err-buffer)))
		(with-current-buffer err-buffer (buffer-string))))
	 (sub-sentinel (process-get proc 'sub-sentinel))
	 (real-command (process-get proc 'real-command)))
    (condition-case err
	(progn
	  ;; Invoke the sub-sentinel, if any
	  (when sub-sentinel
	    (funcall sub-sentinel proc event))
	  ;; Check the exit status.  This will signal an error if the
	  ;; exit status is non-zero.  Don't do this if the process
	  ;; buffer is dead since that means Emacs killed the process
	  ;; and there's no point in telling the user that (but we
	  ;; still check for and report stderr output below).
	  (when (buffer-live-p (process-buffer proc))
	    (notmuch-check-async-exit-status proc event real-command err))
	  ;; If that didn't signal an error, then any error output was
	  ;; really warning output.  Show warnings, if any.
	  (let ((warnings
		 (when err
		   (with-current-buffer err-buffer
		     (goto-char (point-min))
		     (end-of-line)
		     ;; Show first line; stuff remaining lines in the
		     ;; errors buffer.
		     (let ((l1 (buffer-substring (point-min) (point))))
		       (skip-chars-forward "\n")
		       (cons l1 (unless (eobp)
				  (buffer-substring (point) (point-max)))))))))
	    (when warnings
	      (notmuch-logged-error (car warnings) (cdr warnings)))))
      (error
       ;; Emacs behaves strangely if an error escapes from a sentinel,
       ;; so turn errors into messages.
       (message "%s" (error-message-string err))))
    (when err-file (ignore-errors (delete-file err-file)))))

(defun notmuch-start-notmuch-error-sentinel (proc event)
  (let* ((err-file (process-get proc 'err-file))
	 ;; When `make-process' is available, use the error buffer
	 ;; associated with the process, otherwise the error file.
	 (err-buffer (or (process-get proc 'err-buffer)
			 (find-file-noselect err-file))))
    (when err-buffer (kill-buffer err-buffer))))

;; This variable is used only buffer local, but it needs to be
;; declared globally first to avoid compiler warnings.
(defvar notmuch-show-process-crypto nil)
(make-variable-buffer-local 'notmuch-show-process-crypto)

(provide 'notmuch-lib)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; notmuch-lib.el ends here

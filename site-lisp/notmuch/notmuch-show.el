;;; notmuch-show.el --- displaying notmuch forests.
;;
;; Copyright © Carl Worth
;; Copyright © David Edmondson
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
;;          David Edmondson <dme@dme.org>

;;; Code:

(eval-when-compile (require 'cl))
(require 'mm-view)
(require 'message)
(require 'mm-decode)
(require 'mailcap)
(require 'icalendar)
(require 'goto-addr)

(require 'notmuch-lib)
(require 'notmuch-tag)
(require 'notmuch-query)
(require 'notmuch-wash)
(require 'notmuch-mua)
(require 'notmuch-crypto)
(require 'notmuch-print)
(require 'notmuch-draft)

(declare-function notmuch-call-notmuch-process "notmuch" (&rest args))
(declare-function notmuch-search-next-thread "notmuch" nil)
(declare-function notmuch-search-previous-thread "notmuch" nil)
(declare-function notmuch-search-show-thread "notmuch" nil)
(declare-function notmuch-foreach-mime-part "notmuch" (function mm-handle))
(declare-function notmuch-count-attachments "notmuch" (mm-handle))
(declare-function notmuch-save-attachments "notmuch" (mm-handle &optional queryp))
(declare-function notmuch-tree "notmuch-tree"
		  (&optional query query-context target buffer-name open-target))
(declare-function notmuch-tree-get-message-properties "notmuch-tree" nil)
(declare-function notmuch-read-query "notmuch" (prompt))
(declare-function notmuch-draft-resume "notmuch-draft" (id))

(defcustom notmuch-message-headers '("Subject" "To" "Cc" "Date")
  "Headers that should be shown in a message, in this order.

For an open message, all of these headers will be made visible
according to `notmuch-message-headers-visible' or can be toggled
with `notmuch-show-toggle-visibility-headers'. For a closed message,
only the first header in the list will be visible."
  :type '(repeat string)
  :group 'notmuch-show)

(defcustom notmuch-message-headers-visible t
  "Should the headers be visible by default?

If this value is non-nil, then all of the headers defined in
`notmuch-message-headers' will be visible by default in the display
of each message. Otherwise, these headers will be hidden and
`notmuch-show-toggle-visibility-headers' can be used to make them
visible for any given message."
  :type 'boolean
  :group 'notmuch-show)

(defcustom notmuch-show-relative-dates t
  "Display relative dates in the message summary line."
  :type 'boolean
  :group 'notmuch-show)

(defvar notmuch-show-markup-headers-hook '(notmuch-show-colour-headers)
  "A list of functions called to decorate the headers listed in
`notmuch-message-headers'.")

(defcustom notmuch-show-hook '(notmuch-show-turn-on-visual-line-mode)
  "Functions called after populating a `notmuch-show' buffer."
  :type 'hook
  :options '(notmuch-show-turn-on-visual-line-mode)
  :group 'notmuch-show
  :group 'notmuch-hooks)

(defcustom notmuch-show-insert-text/plain-hook '(notmuch-wash-wrap-long-lines
						 notmuch-wash-tidy-citations
						 notmuch-wash-elide-blank-lines
						 notmuch-wash-excerpt-citations)
  "Functions used to improve the display of text/plain parts."
  :type 'hook
  :options '(notmuch-wash-convert-inline-patch-to-part
	     notmuch-wash-wrap-long-lines
	     notmuch-wash-tidy-citations
	     notmuch-wash-elide-blank-lines
	     notmuch-wash-excerpt-citations)
  :group 'notmuch-show
  :group 'notmuch-hooks)

(defcustom notmuch-show-max-text-part-size 100000
  "Maximum size of a text part to be shown by default in characters.

Set to 0 to show the part regardless of size."
  :type 'integer
  :group 'notmuch-show)

;; Mostly useful for debugging.
(defcustom notmuch-show-all-multipart/alternative-parts nil
  "Should all parts of multipart/alternative parts be shown?"
  :type 'boolean
  :group 'notmuch-show)

(defcustom notmuch-show-indent-messages-width 1
  "Width of message indentation in threads.

Messages are shown indented according to their depth in a thread.
This variable determines the width of this indentation measured
in number of blanks.  Defaults to `1', choose `0' to disable
indentation."
  :type 'integer
  :group 'notmuch-show)

(defcustom notmuch-show-indent-multipart nil
  "Should the sub-parts of a multipart/* part be indented?"
  ;; dme: Not sure which is a good default.
  :type 'boolean
  :group 'notmuch-show)

(defcustom notmuch-show-part-button-default-action 'notmuch-show-save-part
  "Default part header button action (on ENTER or mouse click)."
  :group 'notmuch-show
  :type '(choice (const :tag "Save part"
			notmuch-show-save-part)
		 (const :tag "View part"
			notmuch-show-view-part)
		 (const :tag "View interactively"
			notmuch-show-interactively-view-part)))

(defcustom notmuch-show-only-matching-messages nil
  "Only matching messages are shown by default."
  :type 'boolean
  :group 'notmuch-show)

;; By default, block all external images to prevent privacy leaks and
;; potential attacks.
(defcustom notmuch-show-text/html-blocked-images "."
  "Remote images that have URLs matching this regexp will be blocked."
  :type '(choice (const nil) regexp)
  :group 'notmuch-show)

(defvar notmuch-show-thread-id nil)
(make-variable-buffer-local 'notmuch-show-thread-id)

(defvar notmuch-show-parent-buffer nil)
(make-variable-buffer-local 'notmuch-show-parent-buffer)

(defvar notmuch-show-query-context nil)
(make-variable-buffer-local 'notmuch-show-query-context)

(defvar notmuch-show-process-crypto nil)
(make-variable-buffer-local 'notmuch-show-process-crypto)

(defvar notmuch-show-elide-non-matching-messages nil)
(make-variable-buffer-local 'notmuch-show-elide-non-matching-messages)

(defvar notmuch-show-indent-content t)
(make-variable-buffer-local 'notmuch-show-indent-content)

(defvar notmuch-show-attachment-debug nil
  "If t log stdout and stderr from attachment handlers

When set to nil (the default) stdout and stderr from attachment
handlers is discarded. When set to t the stdout and stderr from
each attachment handler is logged in buffers with names beginning
\" *notmuch-part*\". This option requires emacs version at least
24.3 to work.")

(defcustom notmuch-show-stash-mlarchive-link-alist
  '(("Gmane" . "https://mid.gmane.org/")
    ("MARC" . "https://marc.info/?i=")
    ("Mail Archive, The" . "https://mid.mail-archive.com/")
    ("LKML" . "https://lkml.kernel.org/r/")
    ;; FIXME: can these services be searched by `Message-Id' ?
    ;; ("MarkMail" . "http://markmail.org/")
    ;; ("Nabble" . "http://nabble.com/")
    ;; ("opensubscriber" . "http://opensubscriber.com/")
    )
  "List of Mailing List Archives to use when stashing links.

This list is used for generating a Mailing List Archive reference
URI with the current message's Message-Id in
`notmuch-show-stash-mlarchive-link'.

If the cdr of the alist element is not a function, the cdr is
expected to contain a URI that is concatenated with the current
message's Message-Id to create a ML archive reference URI.

If the cdr is a function, the function is called with the
Message-Id as the argument, and the function is expected to
return the ML archive reference URI."
  :type '(alist :key-type (string :tag "Name")
		:value-type (choice
			     (string :tag "URL")
			     (function :tag "Function returning the URL")))
  :group 'notmuch-show)

(defcustom notmuch-show-stash-mlarchive-link-default "Gmane"
  "Default Mailing List Archive to use when stashing links.

This is used when `notmuch-show-stash-mlarchive-link' isn't
provided with an MLA argument nor `completing-read' input."
  :type `(choice
	  ,@(mapcar
	     (lambda (mla)
	       (list 'const :tag (car mla) :value (car mla)))
	     notmuch-show-stash-mlarchive-link-alist))
  :group 'notmuch-show)

(defcustom notmuch-show-mark-read-tags '("-unread")
  "List of tag changes to apply to a message when it is marked as read.

Tags starting with \"+\" (or not starting with either \"+\" or
\"-\") in the list will be added, and tags starting with \"-\"
will be removed from the message being marked as read.

For example, if you wanted to remove an \"unread\" tag and add a
\"read\" tag (which would make little sense), you would set:
    (\"-unread\" \"+read\")"
  :type '(repeat string)
  :group 'notmuch-show)

(defcustom notmuch-show-mark-read-function #'notmuch-show-seen-current-message
  "Function to control which messages are marked read.

The function should take two arguments START and END which will
be the start and end of the visible portion of the buffer and
should mark the appropriate messages read by applying
`notmuch-show-mark-read'. This function will be called after
every user interaction with notmuch."
  :type 'function
  :group 'notmuch-show)

(defcustom notmuch-show-imenu-indent nil
  "Should Imenu display messages indented.

By default, Imenu (see Info node `(emacs) Imenu') in a
notmuch-show buffer displays all messages straight.  This is
because the default Emacs frontend for Imenu makes it difficult
to select an Imenu entry with spaces in front.  Other imenu
frontends such as counsel-imenu does not have this limitation.
In these cases, Imenu entries can be indented to reflect the
position of the message in the thread."
  :type 'boolean
  :group 'notmuch-show)

(defmacro with-current-notmuch-show-message (&rest body)
  "Evaluate body with current buffer set to the text of current message"
  `(save-excursion
     (let ((id (notmuch-show-get-message-id)))
       (let ((buf (generate-new-buffer (concat "*notmuch-msg-" id "*"))))
         (with-current-buffer buf
	   (let ((coding-system-for-read 'no-conversion))
	     (call-process notmuch-command nil t nil "show" "--format=raw" id))
	   ,@body)
	 (kill-buffer buf)))))

(defun notmuch-show-turn-on-visual-line-mode ()
  "Enable Visual Line mode."
  (visual-line-mode t))

;; DEPRECATED in Notmuch 0.16 since we now have convenient part
;; commands.  We'll keep the command around for a version or two in
;; case people want to bind it themselves.
(defun notmuch-show-view-all-mime-parts ()
  "Use external viewers to view all attachments from the current message."
  (interactive)
  (with-current-notmuch-show-message
   ;; We override the mm-inline-media-tests to indicate which message
   ;; parts are already sufficiently handled by the original
   ;; presentation of the message in notmuch-show mode. These parts
   ;; will be inserted directly into the temporary buffer of
   ;; with-current-notmuch-show-message and silently discarded.
   ;;
   ;; Any MIME part not explicitly mentioned here will be handled by an
   ;; external viewer as configured in the various mailcap files.
   (let ((mm-inline-media-tests '(
				  ("text/.*" ignore identity)
				  ("application/pgp-signature" ignore identity)
				  ("multipart/alternative" ignore identity)
				  ("multipart/mixed" ignore identity)
				  ("multipart/related" ignore identity)
				 )))
     (mm-display-parts (mm-dissect-buffer)))))

(defun notmuch-show-save-attachments ()
  "Save all attachments from the current message."
  (interactive)
  (with-current-notmuch-show-message
   (let ((mm-handle (mm-dissect-buffer)))
     (notmuch-save-attachments
      mm-handle (> (notmuch-count-attachments mm-handle) 1))))
  (message "Done"))

(defun notmuch-show-with-message-as-text (fn)
  "Apply FN to a text representation of the current message.

FN is called with one argument, the message properties. It should
operation on the contents of the current buffer."

  ;; Remake the header to ensure that all information is available.
  (let* ((to (notmuch-show-get-to))
	 (cc (notmuch-show-get-cc))
	 (from (notmuch-show-get-from))
	 (subject (notmuch-show-get-subject))
	 (date (notmuch-show-get-date))
	 (tags (notmuch-show-get-tags))
	 (depth (notmuch-show-get-depth))

	 (header (concat
		  "Subject: " subject "\n"
		  "To: " to "\n"
		  (if (not (string= cc ""))
		      (concat "Cc: " cc "\n")
		    "")
		  "From: " from "\n"
		  "Date: " date "\n"
		  (if tags
		      (concat "Tags: "
			      (mapconcat #'identity tags ", ") "\n")
		    "")))
	 (all (buffer-substring (notmuch-show-message-top)
				(notmuch-show-message-bottom)))

	 (props (notmuch-show-get-message-properties))
	 (indenting notmuch-show-indent-content))
    (with-temp-buffer
      (insert all)
      (if indenting
	  (indent-rigidly (point-min) (point-max) (- (* notmuch-show-indent-messages-width depth))))
      ;; Remove the original header.
      (goto-char (point-min))
      (re-search-forward "^$" (point-max) nil)
      (delete-region (point-min) (point))
      (insert header)
      (funcall fn props))))

(defun notmuch-show-print-message ()
  "Print the current message."
  (interactive)
  (notmuch-show-with-message-as-text 'notmuch-print-message))

(defun notmuch-show-fontify-header ()
  (let ((face (cond
	       ((looking-at "[Tt]o:")
		'message-header-to)
	       ((looking-at "[Bb]?[Cc][Cc]:")
		'message-header-cc)
	       ((looking-at "[Ss]ubject:")
		'message-header-subject)
	       (t
		'message-header-other))))

    (overlay-put (make-overlay (point) (re-search-forward ":"))
		 'face 'message-header-name)
    (overlay-put (make-overlay (point) (re-search-forward ".*$"))
		 'face face)))

(defun notmuch-show-colour-headers ()
  "Apply some colouring to the current headers."
  (goto-char (point-min))
  (while (looking-at "^[A-Za-z][-A-Za-z0-9]*:")
    (notmuch-show-fontify-header)
    (forward-line)))

(defun notmuch-show-spaces-n (n)
  "Return a string comprised of `n' spaces."
  (make-string n ? ))

(defun notmuch-show-update-tags (tags)
  "Update the displayed tags of the current message."
  (save-excursion
    (goto-char (notmuch-show-message-top))
    (if (re-search-forward "(\\([^()]*\\))$" (line-end-position) t)
	(let ((inhibit-read-only t))
	  (replace-match (concat "("
				 (notmuch-tag-format-tags tags (notmuch-show-get-prop :orig-tags))
				 ")"))))))

(defun notmuch-clean-address (address)
  "Try to clean a single email ADDRESS for display. Return a cons
cell of (AUTHOR_EMAIL AUTHOR_NAME). Return (ADDRESS nil) if
parsing fails."
  (condition-case nil
    (let (p-name p-address)
      ;; It would be convenient to use `mail-header-parse-address',
      ;; but that expects un-decoded mailbox parts, whereas our
      ;; mailbox parts are already decoded (and hence may contain
      ;; UTF-8). Given that notmuch should handle most of the awkward
      ;; cases, some simple string deconstruction should be sufficient
      ;; here.
      (cond
       ;; "User <user@dom.ain>" style.
       ((string-match "\\(.*\\) <\\(.*\\)>" address)
	(setq p-name (match-string 1 address)
	      p-address (match-string 2 address)))

       ;; "<user@dom.ain>" style.
       ((string-match "<\\(.*\\)>" address)
	(setq p-address (match-string 1 address)))

       ;; Everything else.
       (t
	(setq p-address address)))

      (when p-name
	;; Remove elements of the mailbox part that are not relevant for
	;; display, even if they are required during transport:
	;;
	;; Backslashes.
	(setq p-name (replace-regexp-in-string "\\\\" "" p-name))

	;; Outer single and double quotes, which might be nested.
	(loop
	 with start-of-loop
	 do (setq start-of-loop p-name)

	 when (string-match "^\"\\(.*\\)\"$" p-name)
	 do (setq p-name (match-string 1 p-name))

	 when (string-match "^'\\(.*\\)'$" p-name)
	 do (setq p-name (match-string 1 p-name))

	 until (string= start-of-loop p-name)))

      ;; If the address is 'foo@bar.com <foo@bar.com>' then show just
      ;; 'foo@bar.com'.
      (when (string= p-name p-address)
	(setq p-name nil))

      (cons p-address p-name))
    (error (cons address nil))))

(defun notmuch-show-clean-address (address)
  "Try to clean a single email ADDRESS for display.  Return
unchanged ADDRESS if parsing fails."
  (let* ((clean-address (notmuch-clean-address address))
	 (p-address (car clean-address))
	 (p-name (cdr clean-address)))
    ;; If no name, return just the address.
    (if (not p-name)
	p-address
      ;; Otherwise format the name and address together.
      (concat p-name " <" p-address ">"))))

(defun notmuch-show-insert-headerline (headers date tags depth)
  "Insert a notmuch style headerline based on HEADERS for a
message at DEPTH in the current thread."
  (let ((start (point)))
    (insert (notmuch-show-spaces-n (* notmuch-show-indent-messages-width depth))
	    (notmuch-sanitize
	     (notmuch-show-clean-address (plist-get headers :From)))
	    " ("
	    date
	    ") ("
	    (notmuch-tag-format-tags tags tags)
	    ")\n")
    (overlay-put (make-overlay start (point)) 'face 'notmuch-message-summary-face)))

(defun notmuch-show-insert-header (header header-value)
  "Insert a single header."
  (insert header ": " (notmuch-sanitize header-value) "\n"))

(defun notmuch-show-insert-headers (headers)
  "Insert the headers of the current message."
  (let ((start (point)))
    (mapc (lambda (header)
	    (let* ((header-symbol (intern (concat ":" header)))
		   (header-value (plist-get headers header-symbol)))
	      (if (and header-value
		       (not (string-equal "" header-value)))
		  (notmuch-show-insert-header header header-value))))
	  notmuch-message-headers)
    (save-excursion
      (save-restriction
	(narrow-to-region start (point-max))
	(run-hooks 'notmuch-show-markup-headers-hook)))))

(define-button-type 'notmuch-show-part-button-type
  'action 'notmuch-show-part-button-default
  'follow-link t
  'face 'message-mml
  :supertype 'notmuch-button-type)

(defun notmuch-show-insert-part-header (nth content-type declared-type &optional name comment)
  (let ((button)
	(base-label (concat (when name (concat name ": "))
			    declared-type
			    (unless (string-equal declared-type content-type)
			      (concat " (as " content-type ")"))
			    comment)))

    (setq button
	  (insert-button
	   (concat "[ " base-label " ]")
	   :base-label base-label
	   :type 'notmuch-show-part-button-type
	   :notmuch-part-hidden nil))
    (insert "\n")
    ;; return button
    button))

(defun notmuch-show-toggle-part-invisibility (&optional button)
  (interactive)
  (let ((button (or button (button-at (point)))))
    (when button
      (let ((overlay (button-get button 'overlay))
	    (lazy-part (button-get button :notmuch-lazy-part)))
	;; We have a part to toggle if there is an overlay or if there is a lazy part.
	;; If neither is present we cannot toggle the part so we just return nil.
	(when (or overlay lazy-part)
	  (let* ((show (button-get button :notmuch-part-hidden))
		 (new-start (button-start button))
		 (button-label (button-get button :base-label))
		 (old-point (point))
		 (properties (text-properties-at (button-start button)))
		 (inhibit-read-only t))
	    ;; Toggle the button itself.
	    (button-put button :notmuch-part-hidden (not show))
	    (goto-char new-start)
	    (insert "[ " button-label (if show " ]" " (hidden) ]"))
	    (set-text-properties new-start (point) properties)
	    (let ((old-end (button-end button)))
	      (move-overlay button new-start (point))
	      (delete-region (point) old-end))
	    (goto-char (min old-point (1- (button-end button))))
	    ;; Return nil if there is a lazy-part, it is empty, and we are
	    ;; trying to show it.  In all other cases return t.
	    (if lazy-part
		(when show
		  (button-put button :notmuch-lazy-part nil)
		  (notmuch-show-lazy-part lazy-part button))
	      ;; else there must be an overlay.
	      (overlay-put overlay 'invisible (not show))
	      t)))))))

;; Part content ID handling

(defvar notmuch-show--cids nil
  "Alist from raw content ID to (MSG PART).")
(make-variable-buffer-local 'notmuch-show--cids)

(defun notmuch-show--register-cids (msg part)
  "Register content-IDs in PART and all of PART's sub-parts."
  (let ((content-id (plist-get part :content-id)))
    (when content-id
      ;; Note that content-IDs are globally unique, except when they
      ;; aren't: RFC 2046 section 5.1.4 permits children of a
      ;; multipart/alternative to have the same content-ID, in which
      ;; case the MUA is supposed to pick the best one it can render.
      ;; We simply add the content-ID to the beginning of our alist;
      ;; so if this happens, we'll take the last (and "best")
      ;; alternative (even if we can't render it).
      (push (list content-id msg part) notmuch-show--cids)))
  ;; Recurse on sub-parts
  (let ((ctype (notmuch-split-content-type
		(downcase (plist-get part :content-type)))))
    (cond ((equal (first ctype) "multipart")
	   (mapc (apply-partially #'notmuch-show--register-cids msg)
		 (plist-get part :content)))
	  ((equal ctype '("message" "rfc822"))
	   (notmuch-show--register-cids
	    msg
	    (first (plist-get (first (plist-get part :content)) :body)))))))

(defun notmuch-show--get-cid-content (cid)
  "Return a list (CID-content content-type) or nil.

This will only find parts from messages that have been inserted
into the current buffer.  CID must be a raw content ID, without
enclosing angle brackets, a cid: prefix, or URL encoding.  This
will return nil if the CID is unknown or cannot be retrieved."
  (let ((descriptor (cdr (assoc cid notmuch-show--cids))))
    (when descriptor
      (let* ((msg (first descriptor))
	     (part (second descriptor))
	     ;; Request caching for this content, as some messages
	     ;; reference the same cid: part many times (hundreds!).
	     (content (notmuch-get-bodypart-binary
		       msg part notmuch-show-process-crypto 'cache))
	     (content-type (plist-get part :content-type)))
	(list content content-type)))))

(defun notmuch-show-setup-w3m ()
  "Instruct w3m how to retrieve content from a \"related\" part of a message."
  (interactive)
  (if (boundp 'w3m-cid-retrieve-function-alist)
    (unless (assq 'notmuch-show-mode w3m-cid-retrieve-function-alist)
      (push (cons 'notmuch-show-mode #'notmuch-show--cid-w3m-retrieve)
	    w3m-cid-retrieve-function-alist)))
  (setq mm-html-inhibit-images nil))

(defvar w3m-current-buffer) ;; From `w3m.el'.
(defun notmuch-show--cid-w3m-retrieve (url &rest args)
  ;; url includes the cid: prefix and is URL encoded (see RFC 2392).
  (let* ((cid (url-unhex-string (substring url 4)))
	 (content-and-type
	  (with-current-buffer w3m-current-buffer
	    (notmuch-show--get-cid-content cid))))
    (when content-and-type
      (insert (first content-and-type))
      (second content-and-type))))

;; MIME part renderers

(defun notmuch-show-multipart/*-to-list (part)
  (mapcar (lambda (inner-part) (plist-get inner-part :content-type))
	  (plist-get part :content)))

(defun notmuch-show-insert-part-multipart/alternative (msg part content-type nth depth button)
  (let ((chosen-type (car (notmuch-multipart/alternative-choose msg (notmuch-show-multipart/*-to-list part))))
	(inner-parts (plist-get part :content))
	(start (point)))
    ;; This inserts all parts of the chosen type rather than just one,
    ;; but it's not clear that this is the wrong thing to do - which
    ;; should be chosen if there are more than one that match?
    (mapc (lambda (inner-part)
	    (let* ((inner-type (plist-get inner-part :content-type))
		  (hide (not (or notmuch-show-all-multipart/alternative-parts
			   (string= chosen-type inner-type)))))
	      (notmuch-show-insert-bodypart msg inner-part depth hide)))
	  inner-parts)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-multipart/related (msg part content-type nth depth button)
  (let ((inner-parts (plist-get part :content))
	(start (point)))

    ;; Render the primary part.  FIXME: Support RFC 2387 Start header.
    (notmuch-show-insert-bodypart msg (car inner-parts) depth)
    ;; Add hidden buttons for the rest
    (mapc (lambda (inner-part)
	    (notmuch-show-insert-bodypart msg inner-part depth t))
	  (cdr inner-parts))

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-multipart/signed (msg part content-type nth depth button)
  (when button
    (button-put button 'face 'notmuch-crypto-part-header))

  ;; Insert a button detailing the signature status.
  (notmuch-crypto-insert-sigstatus-button (car (plist-get part :sigstatus))
					  (notmuch-show-get-header :From msg))

  (let ((inner-parts (plist-get part :content))
	(start (point)))
    ;; Show all of the parts.
    (mapc (lambda (inner-part)
	    (notmuch-show-insert-bodypart msg inner-part depth))
	  inner-parts)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-multipart/encrypted (msg part content-type nth depth button)
  (when button
    (button-put button 'face 'notmuch-crypto-part-header))

  ;; Insert a button detailing the encryption status.
  (notmuch-crypto-insert-encstatus-button (car (plist-get part :encstatus)))

  ;; Insert a button detailing the signature status.
  (notmuch-crypto-insert-sigstatus-button (car (plist-get part :sigstatus))
					  (notmuch-show-get-header :From msg))

  (let ((inner-parts (plist-get part :content))
	(start (point)))
    ;; Show all of the parts.
    (mapc (lambda (inner-part)
	    (notmuch-show-insert-bodypart msg inner-part depth))
	  inner-parts)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-application/pgp-encrypted (msg part content-type nth depth button)
  t)

(defun notmuch-show-insert-part-multipart/* (msg part content-type nth depth button)
  (let ((inner-parts (plist-get part :content))
	(start (point)))
    ;; Show all of the parts.
    (mapc (lambda (inner-part)
	    (notmuch-show-insert-bodypart msg inner-part depth))
	  inner-parts)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-message/rfc822 (msg part content-type nth depth button)
  (let* ((message (car (plist-get part :content)))
	 (body (car (plist-get message :body)))
	 (start (point)))

    ;; Override `notmuch-message-headers' to force `From' to be
    ;; displayed.
    (let ((notmuch-message-headers '("From" "Subject" "To" "Cc" "Date")))
      (notmuch-show-insert-headers (plist-get message :headers)))

    ;; Blank line after headers to be compatible with the normal
    ;; message display.
    (insert "\n")

    ;; Show the body
    (notmuch-show-insert-bodypart msg body depth)

    (when notmuch-show-indent-multipart
      (indent-rigidly start (point) 1)))
  t)

(defun notmuch-show-insert-part-text/plain (msg part content-type nth depth button)
  ;; For backward compatibility we want to apply the text/plain hook
  ;; to the whole of the part including the part button if there is
  ;; one.
  (let ((start (if button
		   (button-start button)
		 (point))))
    (insert (notmuch-get-bodypart-text msg part notmuch-show-process-crypto))
    (save-excursion
      (save-restriction
	(narrow-to-region start (point-max))
	(run-hook-with-args 'notmuch-show-insert-text/plain-hook msg depth))))
  t)

(defun notmuch-show-insert-part-text/calendar (msg part content-type nth depth button)
  (insert (with-temp-buffer
	    (insert (notmuch-get-bodypart-text msg part notmuch-show-process-crypto))
	    ;; notmuch-get-bodypart-text does no newline conversion.
	    ;; Replace CRLF with LF before icalendar can use it.
	    (goto-char (point-min))
	    (while (re-search-forward "\r\n" nil t)
	      (replace-match "\n" nil nil))
	    (let ((file (make-temp-file "notmuch-ical"))
		  result)
	      (unwind-protect
		  (progn
		    (unless (icalendar-import-buffer file t)
		      (error "Icalendar import error. See *icalendar-errors* for more information"))
		    (set-buffer (get-file-buffer file))
		    (setq result (buffer-substring (point-min) (point-max)))
		    (set-buffer-modified-p nil)
		    (kill-buffer (current-buffer)))
		(delete-file file))
	      result)))
  t)

;; For backwards compatibility.
(defun notmuch-show-insert-part-text/x-vcalendar (msg part content-type nth depth button)
  (notmuch-show-insert-part-text/calendar msg part content-type nth depth button))

(if (version< emacs-version "25.3")
    ;; https://bugs.gnu.org/28350
    ;;
    ;; For newer emacs, we fall back to notmuch-show-insert-part-*/*
    ;; (see notmuch-show-handlers-for)
    (defun notmuch-show-insert-part-text/enriched (msg part content-type nth depth button)
      ;; By requiring enriched below, we ensure that the function enriched-decode-display-prop
      ;; is defined before it will be shadowed by the letf below. Otherwise the version
      ;; in enriched.el may be loaded a bit later and used instead (for the first time).
      (require 'enriched)
      (letf (((symbol-function 'enriched-decode-display-prop)
		 (lambda (start end &optional param) (list start end))))
	(notmuch-show-insert-part-*/* msg part content-type nth depth button))))

(defun notmuch-show-get-mime-type-of-application/octet-stream (part)
  ;; If we can deduce a MIME type from the filename of the attachment,
  ;; we return that.
  (if (plist-get part :filename)
      (let ((extension (file-name-extension (plist-get part :filename)))
	    mime-type)
	(if extension
	    (progn
	      (mailcap-parse-mimetypes)
	      (setq mime-type (mailcap-extension-to-mime extension))
	      (if (and mime-type
		       (not (string-equal mime-type "application/octet-stream")))
		  mime-type
		nil))
	  nil))))

(defun notmuch-show-insert-part-text/html (msg part content-type nth depth button)
  (if (eq mm-text-html-renderer 'shr)
      ;; It's easier to drive shr ourselves than to work around the
      ;; goofy things `mm-shr' does (like irreversibly taking over
      ;; content ID handling).

      ;; FIXME: If we block an image, offer a button to load external
      ;; images.
      (let ((shr-blocked-images notmuch-show-text/html-blocked-images))
	(notmuch-show--insert-part-text/html-shr msg part))
    ;; Otherwise, let message-mode do the heavy lifting
    ;;
    ;; w3m sets up a keymap which "leaks" outside the invisible region
    ;; and causes strange effects in notmuch. We set
    ;; mm-inline-text-html-with-w3m-keymap to nil to tell w3m not to
    ;; set a keymap (so the normal notmuch-show-mode-map remains).
    (let ((mm-inline-text-html-with-w3m-keymap nil)
	  ;; FIXME: If we block an image, offer a button to load external
	  ;; images.
	  (gnus-blocked-images notmuch-show-text/html-blocked-images))
      (notmuch-show-insert-part-*/* msg part content-type nth depth button))))

;; These functions are used by notmuch-show--insert-part-text/html-shr
(declare-function libxml-parse-html-region "xml.c")
(declare-function shr-insert-document "shr")

(defun notmuch-show--insert-part-text/html-shr (msg part)
  ;; Make sure shr is loaded before we start let-binding its globals
  (require 'shr)
  (let ((dom (let ((process-crypto notmuch-show-process-crypto))
	       (with-temp-buffer
		 (insert (notmuch-get-bodypart-text msg part process-crypto))
		 (libxml-parse-html-region (point-min) (point-max)))))
	(shr-content-function
	 (lambda (url)
	   ;; shr strips the "cid:" part of URL, but doesn't
	   ;; URL-decode it (see RFC 2392).
	   (let ((cid (url-unhex-string url)))
	     (first (notmuch-show--get-cid-content cid))))))
    (shr-insert-document dom)
    t))

(defun notmuch-show-insert-part-*/* (msg part content-type nth depth button)
  ;; This handler _must_ succeed - it is the handler of last resort.
  (notmuch-mm-display-part-inline msg part content-type notmuch-show-process-crypto)
  t)

;; Functions for determining how to handle MIME parts.

(defun notmuch-show-handlers-for (content-type)
  "Return a list of content handlers for a part of type CONTENT-TYPE."
  (let (result)
    (mapc (lambda (func)
	    (if (functionp func)
		(push func result)))
	  ;; Reverse order of prefrence.
	  (list (intern (concat "notmuch-show-insert-part-*/*"))
		(intern (concat
			 "notmuch-show-insert-part-"
			 (car (notmuch-split-content-type content-type))
			 "/*"))
		(intern (concat "notmuch-show-insert-part-" content-type))))
    result))

;; 

(defun notmuch-show-insert-bodypart-internal (msg part content-type nth depth button)
  ;; Run the handlers until one of them succeeds.
  (loop for handler in (notmuch-show-handlers-for content-type)
	until (condition-case err
		  (funcall handler msg part content-type nth depth button)
		;; Specifying `debug' here lets the debugger run if
		;; `debug-on-error' is non-nil.
		((debug error)
		 (insert "!!! Bodypart handler `" (prin1-to-string handler) "' threw an error:\n"
			 "!!! " (error-message-string err) "\n")
		 nil))))

(defun notmuch-show-create-part-overlays (button beg end)
  "Add an overlay to the part between BEG and END"

  ;; If there is no button (i.e., the part is text/plain and the first
  ;; part) or if the part has no content then we don't make the part
  ;; toggleable.
  (when (and button (/= beg end))
    (button-put button 'overlay (make-overlay beg end))
    ;; Return true if we created an overlay.
    t))

(defun notmuch-show-record-part-information (part beg end)
  "Store PART as a text property from BEG to END"

  ;; Record part information.  Since we already inserted subparts,
  ;; don't override existing :notmuch-part properties.
  (notmuch-map-text-property beg end :notmuch-part
			     (lambda (v) (or v part)))
  ;; Make :notmuch-part front sticky and rear non-sticky so it stays
  ;; applied to the beginning of each line when we indent the
  ;; message.  Since we're operating on arbitrary renderer output,
  ;; watch out for sticky specs of t, which means all properties are
  ;; front-sticky/rear-nonsticky.
  (notmuch-map-text-property beg end 'front-sticky
			     (lambda (v) (if (listp v)
					     (pushnew :notmuch-part v)
					   v)))
  (notmuch-map-text-property beg end 'rear-nonsticky
			     (lambda (v) (if (listp v)
					     (pushnew :notmuch-part v)
					   v))))

(defun notmuch-show-lazy-part (part-args button)
  ;; Insert the lazy part after the button for the part. We would just
  ;; move to the start of the new line following the button and insert
  ;; the part but that point might have text properties (eg colours
  ;; from a message header etc) so instead we start from the last
  ;; character of the button by adding a newline and finish by
  ;; removing the extra newline from the end of the part.
  (save-excursion
    (goto-char (button-end button))
    (insert "\n")
    (let* ((inhibit-read-only t)
	   ;; We need to use markers for the start and end of the part
	   ;; because the part insertion functions do not guarantee
	   ;; to leave point at the end of the part.
	   (part-beg (copy-marker (point) nil))
	   (part-end (copy-marker (point) t))
	   ;; We have to save the depth as we can't find the depth
	   ;; when narrowed.
	   (depth (notmuch-show-get-depth)))
      (save-restriction
	(narrow-to-region part-beg part-end)
	(delete-region part-beg part-end)
	(apply #'notmuch-show-insert-bodypart-internal part-args)
	(indent-rigidly part-beg part-end (* notmuch-show-indent-messages-width depth)))
      (goto-char part-end)
      (delete-char 1)
      (notmuch-show-record-part-information (second part-args)
					    (button-start button)
					    part-end)
      ;; Create the overlay. If the lazy-part turned out to be empty/not
      ;; showable this returns nil.
      (notmuch-show-create-part-overlays button part-beg part-end))))

(defun notmuch-show-mime-type (part)
  "Return the correct mime-type to use for PART."
  (let ((content-type (downcase (plist-get part :content-type))))
    (or (and (string= content-type "application/octet-stream")
	     (notmuch-show-get-mime-type-of-application/octet-stream part))
	(and (string= content-type "inline patch")
	     "text/x-diff")
	content-type)))

;; The following variable can be overridden by let bindings.
(defvar notmuch-show-insert-header-p-function 'notmuch-show-insert-header-p
  "Specify which function decides which part headers get inserted.

The function should take two parameters, PART and HIDE, and
should return non-NIL if a header button should be inserted for
this part.")

(defun notmuch-show-insert-header-p (part hide)
  ;; Show all part buttons except for the first part if it is text/plain.
  (let ((mime-type (notmuch-show-mime-type part)))
    (not (and (string= mime-type "text/plain")
	      (<= (plist-get part :id) 1)))))

(defun notmuch-show-reply-insert-header-p-never (part hide)
  nil)

(defun notmuch-show-reply-insert-header-p-trimmed (part hide)
  (let ((mime-type (notmuch-show-mime-type part)))
    (and (not (notmuch-match-content-type mime-type "multipart/*"))
	 (not hide))))

(defun notmuch-show-reply-insert-header-p-minimal (part hide)
  (let ((mime-type (notmuch-show-mime-type part)))
    (and (notmuch-match-content-type mime-type "text/*")
	 (not hide))))

(defun notmuch-show-insert-bodypart (msg part depth &optional hide)
  "Insert the body part PART at depth DEPTH in the current thread.

HIDE determines whether to show or hide the part and the button
as follows: If HIDE is nil, show the part and the button. If HIDE
is t, hide the part initially and show the button."

  (let* ((content-type (downcase (plist-get part :content-type)))
	 (mime-type (notmuch-show-mime-type part))
	 (nth (plist-get part :id))
	 (long (and (notmuch-match-content-type mime-type "text/*")
		    (> notmuch-show-max-text-part-size 0)
		    (> (length (plist-get part :content)) notmuch-show-max-text-part-size)))
	 (beg (point))
	 ;; This default header-p function omits the part button for
	 ;; the first (or only) part if this is text/plain.
	 (button (when (funcall notmuch-show-insert-header-p-function part hide)
		   (notmuch-show-insert-part-header nth mime-type content-type (plist-get part :filename))))
	 ;; Hide the part initially if HIDE is t, or if it is too long
	 ;; and we have a button to allow toggling.
	 (show-part (not (or (equal hide t)
			     (and long button))))
	 (content-beg (point)))

    ;; Store the computed mime-type for later use (e.g. by attachment handlers).
    (plist-put part :computed-type mime-type)

    (if show-part
        (notmuch-show-insert-bodypart-internal msg part mime-type nth depth button)
      (when button
	(button-put button :notmuch-lazy-part
		    (list msg part mime-type nth depth button))))

    ;; Some of the body part handlers leave point somewhere up in the
    ;; part, so we make sure that we're down at the end.
    (goto-char (point-max))
    ;; Ensure that the part ends with a carriage return.
    (unless (bolp)
      (insert "\n"))
    ;; We do not create the overlay for hidden (lazy) parts until
    ;; they are inserted.
    (if show-part
	(notmuch-show-create-part-overlays button content-beg (point))
      (save-excursion
	(notmuch-show-toggle-part-invisibility button)))
    (notmuch-show-record-part-information part beg (point))))

(defun notmuch-show-insert-body (msg body depth)
  "Insert the body BODY at depth DEPTH in the current thread."

  ;; Register all content IDs for this message.  According to RFC
  ;; 2392, content IDs are *global*, but it's okay if an MUA treats
  ;; them as only global within a message.
  (notmuch-show--register-cids msg (first body))

  (mapc (lambda (part) (notmuch-show-insert-bodypart msg part depth)) body))

(defun notmuch-show-make-symbol (type)
  (make-symbol (concat "notmuch-show-" type)))

(defun notmuch-show-strip-re (string)
  (replace-regexp-in-string "^\\([Rr]e: *\\)+" "" string))

(defvar notmuch-show-previous-subject "")
(make-variable-buffer-local 'notmuch-show-previous-subject)

(defun notmuch-show-insert-msg (msg depth)
  "Insert the message MSG at depth DEPTH in the current thread."
  (let* ((headers (plist-get msg :headers))
	 ;; Indentation causes the buffer offset of the start/end
	 ;; points to move, so we must use markers.
	 message-start message-end
	 content-start content-end
	 headers-start headers-end
	 (bare-subject (notmuch-show-strip-re (plist-get headers :Subject))))

    (setq message-start (point-marker))

    (notmuch-show-insert-headerline headers
				    (or (if notmuch-show-relative-dates
					    (plist-get msg :date_relative)
					  nil)
					(plist-get headers :Date))
				    (plist-get msg :tags) depth)

    (setq content-start (point-marker))

    ;; Set `headers-start' to point after the 'Subject:' header to be
    ;; compatible with the existing implementation. This just sets it
    ;; to after the first header.
    (notmuch-show-insert-headers headers)
    (save-excursion
      (goto-char content-start)
      ;; If the subject of this message is the same as that of the
      ;; previous message, don't display it when this message is
      ;; collapsed.
      (when (not (string= notmuch-show-previous-subject
			  bare-subject))
	(forward-line 1))
      (setq headers-start (point-marker)))
    (setq headers-end (point-marker))

    (setq notmuch-show-previous-subject bare-subject)

    ;; A blank line between the headers and the body.
    (insert "\n")
    (notmuch-show-insert-body msg (plist-get msg :body)
			      (if notmuch-show-indent-content depth 0))
    ;; Ensure that the body ends with a newline.
    (unless (bolp)
      (insert "\n"))
    (setq content-end (point-marker))

    ;; Indent according to the depth in the thread.
    (if notmuch-show-indent-content
	(indent-rigidly content-start content-end (* notmuch-show-indent-messages-width depth)))

    (setq message-end (point-max-marker))

    ;; Save the extents of this message over the whole text of the
    ;; message.
    (put-text-property message-start message-end :notmuch-message-extent (cons message-start message-end))

    ;; Create overlays used to control visibility
    (plist-put msg :headers-overlay (make-overlay headers-start headers-end))
    (plist-put msg :message-overlay (make-overlay headers-start content-end))

    (plist-put msg :depth depth)

    ;; Save the properties for this message. Currently this saves the
    ;; entire message (augmented it with other stuff), which seems
    ;; like overkill. We might save a reduced subset (for example, not
    ;; the content).
    (notmuch-show-set-message-properties msg)

    ;; Set header visibility.
    (notmuch-show-headers-visible msg notmuch-message-headers-visible)

    ;; Message visibility depends on whether it matched the search
    ;; criteria.
    (notmuch-show-message-visible msg (and (plist-get msg :match)
					   (not (plist-get msg :excluded))))))

(defun notmuch-show-toggle-process-crypto ()
  "Toggle the processing of cryptographic MIME parts."
  (interactive)
  (setq notmuch-show-process-crypto (not notmuch-show-process-crypto))
  (message (if notmuch-show-process-crypto
	       "Processing cryptographic MIME parts."
	     "Not processing cryptographic MIME parts."))
  (notmuch-show-refresh-view))

(defun notmuch-show-toggle-elide-non-matching ()
  "Toggle the display of non-matching messages."
  (interactive)
  (setq notmuch-show-elide-non-matching-messages (not notmuch-show-elide-non-matching-messages))
  (message (if notmuch-show-elide-non-matching-messages
	       "Showing matching messages only."
	     "Showing all messages."))
  (notmuch-show-refresh-view))

(defun notmuch-show-toggle-thread-indentation ()
  "Toggle the indentation of threads."
  (interactive)
  (setq notmuch-show-indent-content (not notmuch-show-indent-content))
  (message (if notmuch-show-indent-content
	       "Content is indented."
	     "Content is not indented."))
  (notmuch-show-refresh-view))

(defun notmuch-show-insert-tree (tree depth)
  "Insert the message tree TREE at depth DEPTH in the current thread."
  (let ((msg (car tree))
	(replies (cadr tree)))
    ;; We test whether there is a message or just some replies.
    (when msg
      (notmuch-show-insert-msg msg depth))
    (notmuch-show-insert-thread replies (1+ depth))))

(defun notmuch-show-insert-thread (thread depth)
  "Insert the thread THREAD at depth DEPTH in the current forest."
  (mapc (lambda (tree) (notmuch-show-insert-tree tree depth)) thread))

(defun notmuch-show-insert-forest (forest)
  "Insert the forest of threads FOREST."
  (mapc (lambda (thread) (notmuch-show-insert-thread thread 0)) forest))

(defvar notmuch-id-regexp
  (concat
   ;; Match the id: prefix only if it begins a word (to disallow, for
   ;; example, matching cid:).
   "\\<id:\\("
   ;; If the term starts with a ", then parse Xapian's quoted boolean
   ;; term syntax, which allows for anything as long as embedded
   ;; double quotes escaped by doubling them.  We also disallow
   ;; newlines (which Xapian allows) to prevent runaway terms.
   "\"\\([^\"\n]\\|\"\"\\)*\""
   ;; Otherwise, parse Xapian's unquoted syntax, which goes up to the
   ;; next space or ).  We disallow [.,;] as the last character
   ;; because these are probably part of the surrounding text, and not
   ;; part of the id.  This doesn't match single character ids; meh.
   "\\|[^\"[:space:])][^[:space:])]*[^])[:space:].,:;?!]"
   "\\)")
  "The regexp used to match id: links in messages.")

(defvar notmuch-mid-regexp
  ;; goto-address-url-regexp matched cid: links, which have the same
  ;; grammar as the message ID part of a mid: link.  Construct the
  ;; regexp using the same technique as goto-address-url-regexp.
  (concat "\\<mid:\\(" thing-at-point-url-path-regexp "\\)")
  "The regexp used to match mid: links in messages.

See RFC 2392.")

(defun notmuch-show-buttonise-links (start end)
  "Buttonise URLs and mail addresses between START and END.

This also turns id:\"<message id>\"-parts and mid: links into
buttons for a corresponding notmuch search."
  (goto-address-fontify-region start end)
  (save-excursion
    (let (links
	  (beg-line (progn (goto-char start) (line-beginning-position)))
	  (end-line (progn (goto-char end) (line-end-position))))
      (goto-char beg-line)
      (while (re-search-forward notmuch-id-regexp end-line t)
	(push (list (match-beginning 0) (match-end 0)
		    (match-string-no-properties 0)) links))
      (goto-char beg-line)
      (while (re-search-forward notmuch-mid-regexp end-line t)
	(let* ((mid-cid (match-string-no-properties 1))
	       (mid (save-match-data
		      (string-match "^[^/]*" mid-cid)
		      (url-unhex-string (match-string 0 mid-cid)))))
	  (push (list (match-beginning 0) (match-end 0)
		      (notmuch-id-to-query mid)) links)))
      (dolist (link links)
	;; Remove the overlay created by goto-address-mode
	(remove-overlays (first link) (second link) 'goto-address t)
	(make-text-button (first link) (second link)
			  :type 'notmuch-button-type
			  'action `(lambda (arg)
				     (notmuch-show ,(third link) current-prefix-arg))
			  'follow-link t
			  'help-echo "Mouse-1, RET: search for this message"
			  'face goto-address-mail-face)))))

;;;###autoload
(defun notmuch-show (thread-id &optional elide-toggle parent-buffer query-context buffer-name)
  "Run \"notmuch show\" with the given thread ID and display results.

ELIDE-TOGGLE, if non-nil, inverts the default elide behavior.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).

The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.

The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used.

Returns the buffer containing the messages, or NIL if no messages
matched."
  (interactive "sNotmuch show: \nP")
  (let ((buffer-name (generate-new-buffer-name
		      (or buffer-name
			  (concat "*notmuch-" thread-id "*"))))
	;; We override mm-inline-override-types to stop application/*
	;; parts from being displayed unless the user has customized
	;; it themselves.
	(mm-inline-override-types
	 (if (equal mm-inline-override-types
		    (eval (car (get 'mm-inline-override-types 'standard-value))))
	     (cons "application/*" mm-inline-override-types)
	   mm-inline-override-types)))
    (switch-to-buffer (get-buffer-create buffer-name))
    ;; No need to track undo information for this buffer.
    (setq buffer-undo-list t)

    (notmuch-show-mode)

    ;; Set various buffer local variables to their appropriate initial
    ;; state. Do this after enabling `notmuch-show-mode' so that they
    ;; aren't wiped out.
    (setq notmuch-show-thread-id thread-id
	  notmuch-show-parent-buffer parent-buffer
	  notmuch-show-query-context (if (or (string= query-context "")
					     (string= query-context "*"))
					 nil query-context)

	  notmuch-show-process-crypto notmuch-crypto-process-mime
	  ;; If `elide-toggle', invert the default value.
	  notmuch-show-elide-non-matching-messages
	  (if elide-toggle
	      (not notmuch-show-only-matching-messages)
	    notmuch-show-only-matching-messages))

    (add-hook 'post-command-hook #'notmuch-show-command-hook nil t)
    (jit-lock-register #'notmuch-show-buttonise-links)

    (notmuch-tag-clear-cache)

    (let ((inhibit-read-only t))
      (if (notmuch-show--build-buffer)
	  ;; Messages were inserted into the buffer.
	  (current-buffer)

	;; No messages were inserted - presumably none matched the
	;; query.
	(kill-buffer (current-buffer))
	(ding)
	(message "No messages matched the query!")
	nil))))

(defun notmuch-show--build-queries (thread context)
  "Return a list of queries to try for this search.

THREAD and CONTEXT are both strings, though CONTEXT may be nil.
When CONTEXT is not nil, the first query is the conjunction of it
and THREAD.  The next query is THREAD alone, and serves as a
fallback if the prior matches no messages."
  (let (queries)
    (push (list thread) queries)
    (if context (push (list thread "and (" context ")") queries))
    queries))

(defun notmuch-show--build-buffer (&optional state)
  "Display messages matching the current buffer context.

Apply the previously saved STATE if supplied, otherwise show the
first relevant message.

If no messages match the query return NIL."
  (let* ((cli-args (cons "--exclude=false"
			 (when notmuch-show-elide-non-matching-messages
			   (list "--entire-thread=false"))))
	 (queries (notmuch-show--build-queries
		   notmuch-show-thread-id notmuch-show-query-context))
	 (forest nil)
	 ;; Must be reset every time we are going to start inserting
	 ;; messages into the buffer.
	 (notmuch-show-previous-subject ""))
    ;; Use results from the first query that returns some.
    (while (and (not forest) queries)
      (setq forest (notmuch-query-get-threads
		    (append cli-args (list "'") (car queries) (list "'"))))
      (setq queries (cdr queries)))
    (when forest
      (notmuch-show-insert-forest forest)

      ;; Store the original tags for each message so that we can
      ;; display changes.
      (notmuch-show-mapc
       (lambda () (notmuch-show-set-prop :orig-tags (notmuch-show-get-tags))))

      ;; Set the header line to the subject of the first message.
      (setq header-line-format
	    (replace-regexp-in-string "%" "%%"
				      (notmuch-sanitize
				       (notmuch-show-strip-re
					(notmuch-show-get-subject)))))

      (run-hooks 'notmuch-show-hook)

      (if state
	  (notmuch-show-apply-state state)
	;; With no state to apply, just go to the first message.
	(notmuch-show-goto-first-wanted-message)))

    ;; Report back to the caller whether any messages matched.
    forest))

(defun notmuch-show-capture-state ()
  "Capture the state of the current buffer.

This includes:
 - the list of open messages,
 - the combination of current message id with/for each visible window."
  (let* ((win-list (get-buffer-window-list (current-buffer) nil t))
	 (win-id-combo (mapcar (lambda (win)
				 (with-selected-window win
				   (list win (notmuch-show-get-message-id))))
			       win-list)))
    (list win-id-combo (notmuch-show-get-message-ids-for-open-messages))))

(defun notmuch-show-get-query ()
  "Return the current query in this show buffer"
  (if notmuch-show-query-context
      (concat notmuch-show-thread-id
	      " and ("
	      notmuch-show-query-context
	      ")")
    notmuch-show-thread-id))

(defun notmuch-show-goto-message (msg-id)
  "Go to message with msg-id."
  (goto-char (point-min))
  (unless (loop if (string= msg-id (notmuch-show-get-message-id))
		return t
		until (not (notmuch-show-goto-message-next)))
    (goto-char (point-min))
    (message "Message-id not found."))
  (notmuch-show-message-adjust))

(defun notmuch-show-apply-state (state)
  "Apply STATE to the current buffer.

This includes:
 - opening the messages previously opened,
 - closing all other messages,
 - moving to the correct current message in every displayed window."
  (let ((win-msg-alist (car state))
	(open (cadr state)))

    ;; Open those that were open.
    (goto-char (point-min))
    (loop do (notmuch-show-message-visible (notmuch-show-get-message-properties)
					   (member (notmuch-show-get-message-id) open))
	  until (not (notmuch-show-goto-message-next)))

    (dolist (win-msg-pair win-msg-alist)
      (with-selected-window (car win-msg-pair)
	;; Go to the previously open message in this window
	(notmuch-show-goto-message (cadr win-msg-pair))))))

(defun notmuch-show-refresh-view (&optional reset-state)
  "Refresh the current view.

Refreshes the current view, observing changes in display
preferences. If invoked with a prefix argument (or RESET-STATE is
non-nil) then the state of the buffer (open/closed messages) is
reset based on the original query."
  (interactive "P")
  (let ((inhibit-read-only t)
	(state (unless reset-state
		 (notmuch-show-capture-state))))
    ;; `erase-buffer' does not seem to remove overlays, which can lead
    ;; to weird effects such as remaining images, so remove them
    ;; manually.
    (remove-overlays)
    (erase-buffer)

    (unless (notmuch-show--build-buffer state)
      ;; No messages were inserted.
      (kill-buffer (current-buffer))
      (ding)
      (message "Refreshing the buffer resulted in no messages!"))))

(defvar notmuch-show-stash-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'notmuch-show-stash-cc)
    (define-key map "d" 'notmuch-show-stash-date)
    (define-key map "F" 'notmuch-show-stash-filename)
    (define-key map "f" 'notmuch-show-stash-from)
    (define-key map "i" 'notmuch-show-stash-message-id)
    (define-key map "I" 'notmuch-show-stash-message-id-stripped)
    (define-key map "s" 'notmuch-show-stash-subject)
    (define-key map "T" 'notmuch-show-stash-tags)
    (define-key map "t" 'notmuch-show-stash-to)
    (define-key map "l" 'notmuch-show-stash-mlarchive-link)
    (define-key map "L" 'notmuch-show-stash-mlarchive-link-and-go)
    (define-key map "G" 'notmuch-show-stash-git-send-email)
    (define-key map "?" 'notmuch-subkeymap-help)
    map)
  "Submap for stash commands")
(fset 'notmuch-show-stash-map notmuch-show-stash-map)

(defvar notmuch-show-part-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'notmuch-show-save-part)
    (define-key map "v" 'notmuch-show-view-part)
    (define-key map "o" 'notmuch-show-interactively-view-part)
    (define-key map "|" 'notmuch-show-pipe-part)
    (define-key map "m" 'notmuch-show-choose-mime-of-part)
    (define-key map "?" 'notmuch-subkeymap-help)
    map)
  "Submap for part commands")
(fset 'notmuch-show-part-map notmuch-show-part-map)

(defvar notmuch-show-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map notmuch-common-keymap)
    (define-key map "Z" 'notmuch-tree-from-show-current-query)
    (define-key map (kbd "<C-tab>") 'widget-backward)
    (define-key map (kbd "M-TAB") 'notmuch-show-previous-button)
    (define-key map (kbd "<backtab>") 'notmuch-show-previous-button)
    (define-key map (kbd "TAB") 'notmuch-show-next-button)
    (define-key map "f" 'notmuch-show-forward-message)
    (define-key map "F" 'notmuch-show-forward-open-messages)
    (define-key map "b" 'notmuch-show-resend-message)
    (define-key map "l" 'notmuch-show-filter-thread)
    (define-key map "r" 'notmuch-show-reply-sender)
    (define-key map "R" 'notmuch-show-reply)
    (define-key map "|" 'notmuch-show-pipe-message)
    (define-key map "w" 'notmuch-show-save-attachments)
    (define-key map "V" 'notmuch-show-view-raw-message)
    (define-key map "e" 'notmuch-show-resume-message)
    (define-key map "c" 'notmuch-show-stash-map)
    (define-key map "h" 'notmuch-show-toggle-visibility-headers)
    (define-key map "k" 'notmuch-tag-jump)
    (define-key map "*" 'notmuch-show-tag-all)
    (define-key map "-" 'notmuch-show-remove-tag)
    (define-key map "+" 'notmuch-show-add-tag)
    (define-key map "X" 'notmuch-show-archive-thread-then-exit)
    (define-key map "x" 'notmuch-show-archive-message-then-next-or-exit)
    (define-key map "A" 'notmuch-show-archive-thread-then-next)
    (define-key map "a" 'notmuch-show-archive-message-then-next-or-next-thread)
    (define-key map "N" 'notmuch-show-next-message)
    (define-key map "P" 'notmuch-show-previous-message)
    (define-key map "n" 'notmuch-show-next-open-message)
    (define-key map "p" 'notmuch-show-previous-open-message)
    (define-key map (kbd "M-n") 'notmuch-show-next-thread-show)
    (define-key map (kbd "M-p") 'notmuch-show-previous-thread-show)
    (define-key map (kbd "DEL") 'notmuch-show-rewind)
    (define-key map " " 'notmuch-show-advance-and-archive)
    (define-key map (kbd "M-RET") 'notmuch-show-open-or-close-all)
    (define-key map (kbd "RET") 'notmuch-show-toggle-message)
    (define-key map "#" 'notmuch-show-print-message)
    (define-key map "!" 'notmuch-show-toggle-elide-non-matching)
    (define-key map "$" 'notmuch-show-toggle-process-crypto)
    (define-key map "<" 'notmuch-show-toggle-thread-indentation)
    (define-key map "t" 'toggle-truncate-lines)
    (define-key map "." 'notmuch-show-part-map)
    map)
  "Keymap for \"notmuch show\" buffers.")
(fset 'notmuch-show-mode-map notmuch-show-mode-map)

(define-derived-mode notmuch-show-mode fundamental-mode "notmuch-show"
  "Major mode for viewing a thread with notmuch.

This buffer contains the results of the \"notmuch show\" command
for displaying a single thread of email from your email archives.

By default, various components of email messages, (citations,
signatures, already-read messages), are hidden. You can make
these parts visible by clicking with the mouse button or by
pressing RET after positioning the cursor on a hidden part, (for
which \\[notmuch-show-next-button] and \\[notmuch-show-previous-button] are helpful).

Reading the thread sequentially is well-supported by pressing
\\[notmuch-show-advance-and-archive]. This will scroll the current message (if necessary), advance
to the next message, or advance to the next thread (if already on
the last message of a thread).

Other commands are available to read or manipulate the thread
more selectively, (such as '\\[notmuch-show-next-message]' and '\\[notmuch-show-previous-message]' to advance to messages
without removing any tags, and '\\[notmuch-show-archive-thread]' to archive an entire thread
without scrolling through with \\[notmuch-show-advance-and-archive]).

You can add or remove arbitrary tags from the current message with
'\\[notmuch-show-add-tag]' or '\\[notmuch-show-remove-tag]'.

All currently available key bindings:

\\{notmuch-show-mode-map}"
  (setq notmuch-buffer-refresh-function #'notmuch-show-refresh-view)
  (setq buffer-read-only t
	truncate-lines t)
  (setq imenu-prev-index-position-function
        #'notmuch-show-imenu-prev-index-position-function)
  (setq imenu-extract-index-name-function
        #'notmuch-show-imenu-extract-index-name-function))

(defun notmuch-tree-from-show-current-query ()
  "Call notmuch tree with the current query"
  (interactive)
  (notmuch-tree notmuch-show-thread-id
		notmuch-show-query-context
		(notmuch-show-get-message-id)))

(defun notmuch-show-move-to-message-top ()
  (goto-char (notmuch-show-message-top)))

(defun notmuch-show-move-to-message-bottom ()
  (goto-char (notmuch-show-message-bottom)))

(defun notmuch-show-message-adjust ()
  (recenter 0))

;; Movement related functions.

;; There's some strangeness here where a text property applied to a
;; region a->b is not found when point is at b. We walk backwards
;; until finding the property.
(defun notmuch-show-message-extent ()
  (let (r)
    (save-excursion
      (while (not (setq r (get-text-property (point) :notmuch-message-extent)))
	(backward-char)))
    r))

(defun notmuch-show-message-top ()
  (car (notmuch-show-message-extent)))

(defun notmuch-show-message-bottom ()
  (cdr (notmuch-show-message-extent)))

(defun notmuch-show-goto-message-next ()
  (let ((start (point)))
    (notmuch-show-move-to-message-bottom)
    (if (not (eobp))
	t
      (goto-char start)
      nil)))

(defun notmuch-show-goto-message-previous ()
  (notmuch-show-move-to-message-top)
  (if (bobp)
      nil
    (backward-char)
    (notmuch-show-move-to-message-top)
    t))

(defun notmuch-show-mapc (function)
  "Iterate through all messages in the current thread with
`notmuch-show-goto-message-next' and call FUNCTION for side
effects."
  (save-excursion
    (goto-char (point-min))
    (loop do (funcall function)
	  while (notmuch-show-goto-message-next))))

;; Functions relating to the visibility of messages and their
;; components.

(defun notmuch-show-message-visible (props visible-p)
  (overlay-put (plist-get props :message-overlay) 'invisible (not visible-p))
  (notmuch-show-set-prop :message-visible visible-p props))

(defun notmuch-show-headers-visible (props visible-p)
  (overlay-put (plist-get props :headers-overlay) 'invisible (not visible-p))
  (notmuch-show-set-prop :headers-visible visible-p props))

;; Functions for setting and getting attributes of the current
;; message.

(defun notmuch-show-set-message-properties (props)
  (save-excursion
    (notmuch-show-move-to-message-top)
    (put-text-property (point) (+ (point) 1) :notmuch-message-properties props)))

(defun notmuch-show-get-message-properties ()
  "Return the properties of the current message as a plist.

Some useful entries are:
:headers - Property list containing the headers :Date, :Subject, :From, etc.
:body - Body of the message
:tags - Tags for this message"
  (save-excursion
    (notmuch-show-move-to-message-top)
    (get-text-property (point) :notmuch-message-properties)))

(defun notmuch-show-get-part-properties ()
  "Return the properties of the innermost part containing point.

This is the part property list retrieved from the CLI.  Signals
an error if there is no part containing point."
  (or (get-text-property (point) :notmuch-part)
      (error "No message part here")))

(defun notmuch-show-set-prop (prop val &optional props)
  (let ((inhibit-read-only t)
	(props (or props
		   (notmuch-show-get-message-properties))))
    (plist-put props prop val)
    (notmuch-show-set-message-properties props)))

(defun notmuch-show-get-prop (prop &optional props)
  "Get property PROP from current message in show or tree mode.

It gets property PROP from PROPS or, if PROPS is nil, the current
message in either tree or show. This means that several utility
functions in notmuch-show can be used directly by notmuch-tree as
they just need the correct message properties."
  (let ((props (or props
		   (cond ((eq major-mode 'notmuch-show-mode)
			  (notmuch-show-get-message-properties))
			 ((eq major-mode 'notmuch-tree-mode)
			  (notmuch-tree-get-message-properties))
			 (t nil)))))
    (plist-get props prop)))

(defun notmuch-show-get-message-id (&optional bare)
  "Return an id: query for the Message-Id of the current message.

If optional argument BARE is non-nil, return
the Message-Id without id: prefix and escaping."
  (if bare
      (notmuch-show-get-prop :id)
    (notmuch-id-to-query (notmuch-show-get-prop :id))))

(defun notmuch-show-get-messages-ids ()
  "Return all id: queries of messages in the current thread."
  (let ((message-ids))
    (notmuch-show-mapc
     (lambda () (push (notmuch-show-get-message-id) message-ids)))
    message-ids))

(defun notmuch-show-get-messages-ids-search ()
  "Return a search string for all message ids of messages in the
current thread."
  (mapconcat 'identity (notmuch-show-get-messages-ids) " or "))

;; dme: Would it make sense to use a macro for many of these?

;; XXX TODO figure out what to do about multiple filenames
(defun notmuch-show-get-filename ()
  "Return the filename of the current message."
  (car (notmuch-show-get-prop :filename)))

(defun notmuch-show-get-header (header &optional props)
  "Return the named header of the current message, if any."
  (plist-get (notmuch-show-get-prop :headers props) header))

(defun notmuch-show-get-cc ()
  (notmuch-show-get-header :Cc))

(defun notmuch-show-get-date ()
  (notmuch-show-get-header :Date))

(defun notmuch-show-get-timestamp ()
  (notmuch-show-get-prop :timestamp))

(defun notmuch-show-get-from ()
  (notmuch-show-get-header :From))

(defun notmuch-show-get-subject ()
  (notmuch-show-get-header :Subject))

(defun notmuch-show-get-to ()
  (notmuch-show-get-header :To))

(defun notmuch-show-get-depth ()
  (notmuch-show-get-prop :depth))

(defun notmuch-show-set-tags (tags)
  "Set the tags of the current message."
  (notmuch-show-set-prop :tags tags)
  (notmuch-show-update-tags tags))

(defun notmuch-show-get-tags ()
  "Return the tags of the current message."
  (notmuch-show-get-prop :tags))

(defun notmuch-show-message-visible-p ()
  "Is the current message visible?"
  (notmuch-show-get-prop :message-visible))

(defun notmuch-show-headers-visible-p ()
  "Are the headers of the current message visible?"
  (notmuch-show-get-prop :headers-visible))

(put 'notmuch-show-mark-read 'notmuch-prefix-doc
     "Mark the current message as unread.")
(defun notmuch-show-mark-read (&optional unread)
  "Mark the current message as read.

Mark the current message as read by applying the tag changes in
`notmuch-show-mark-read-tags' to it (remove the \"unread\" tag by
default). If a prefix argument is given, the message will be
marked as unread, i.e. the tag changes in
`notmuch-show-mark-read-tags' will be reversed."
  (interactive "P")
  (when notmuch-show-mark-read-tags
    (apply 'notmuch-show-tag-message
	   (notmuch-tag-change-list notmuch-show-mark-read-tags unread))))

(defun notmuch-show-seen-current-message (start end)
  "Mark the current message read if it is open.

We only mark it read once: if it is changed back then that is a
user decision and we should not override it."
  (when (and (notmuch-show-message-visible-p)
	     (not (notmuch-show-get-prop :seen)))
	(notmuch-show-mark-read)
	(notmuch-show-set-prop :seen t)))

(defvar notmuch-show--seen-has-errored nil)
(make-variable-buffer-local 'notmuch-show--seen-has-errored)

(defun notmuch-show-command-hook ()
  (when (eq major-mode 'notmuch-show-mode)
    ;; We need to redisplay to get window-start and window-end correct.
    (redisplay)
    (save-excursion
      (condition-case err
	  (funcall notmuch-show-mark-read-function (window-start) (window-end))
	((debug error)
	 (unless notmuch-show--seen-has-errored
	   (setq notmuch-show--seen-has-errored 't)
	   (setq header-line-format
		 (concat header-line-format
			 (propertize "  [some mark read tag changes may have failed]"
				     'face font-lock-warning-face)))))))))

(defun notmuch-show-filter-thread (query)
  "Filter or LIMIT the current thread based on a new query string.

Reshows the current thread with matches defined by the new query-string."
  (interactive (list (notmuch-read-query "Filter thread: ")))
  (let ((msg-id (notmuch-show-get-message-id)))
    (setq notmuch-show-query-context (if (string= query "") nil query))
    (notmuch-show-refresh-view t)
    (notmuch-show-goto-message msg-id)))

;; Functions for getting attributes of several messages in the current
;; thread.

(defun notmuch-show-get-message-ids-for-open-messages ()
  "Return a list of all id: queries for open messages in the current thread."
  (save-excursion
    (let (message-ids done)
      (goto-char (point-min))
      (while (not done)
	(if (notmuch-show-message-visible-p)
	    (setq message-ids (append message-ids (list (notmuch-show-get-message-id)))))
	(setq done (not (notmuch-show-goto-message-next)))
	)
      message-ids
      )))

;; Commands typically bound to keys.

(defun notmuch-show-advance ()
  "Advance through thread.

If the current message in the thread is not yet fully visible,
scroll by a near screenful to read more of the message.

Otherwise, (the end of the current message is already within the
current window), advance to the next open message."
  (interactive)
  (let* ((end-of-this-message (notmuch-show-message-bottom))
	 (visible-end-of-this-message (1- end-of-this-message))
	 (ret nil))
    (while (invisible-p visible-end-of-this-message)
      (setq visible-end-of-this-message
	    (max (point-min)
		 (1- (previous-single-char-property-change
		      visible-end-of-this-message 'invisible)))))
    (cond
     ;; Ideally we would test `end-of-this-message' against the result
     ;; of `window-end', but that doesn't account for the fact that
     ;; the end of the message might be hidden.
     ((and visible-end-of-this-message
	   (> visible-end-of-this-message (window-end)))
      ;; The bottom of this message is not visible - scroll.
      (scroll-up nil))

     ((not (= end-of-this-message (point-max)))
      ;; This is not the last message - move to the next visible one.
      (notmuch-show-next-open-message))

     ((not (= (point) (point-max)))
      ;; This is the last message, but the cursor is not at the end of
      ;; the buffer. Move it there.
      (goto-char (point-max)))

     (t
      ;; This is the last message - change the return value
      (setq ret t)))
    ret))

(defun notmuch-show-advance-and-archive ()
  "Advance through thread and archive.

This command is intended to be one of the simplest ways to
process a thread of email. It works exactly like
notmuch-show-advance, in that it scrolls through messages in a
show buffer, except that when it gets to the end of the buffer it
archives the entire current thread, (apply changes in
`notmuch-archive-tags'), kills the buffer, and displays the next
thread from the search from which this thread was originally
shown."
  (interactive)
  (if (notmuch-show-advance)
      (notmuch-show-archive-thread-then-next)))

(defun notmuch-show-rewind ()
  "Backup through the thread (reverse scrolling compared to \\[notmuch-show-advance-and-archive]).

Specifically, if the beginning of the previous email is fewer
than `window-height' lines from the current point, move to it
just like `notmuch-show-previous-message'.

Otherwise, just scroll down a screenful of the current message.

This command does not modify any message tags, (it does not undo
any effects from previous calls to
`notmuch-show-advance-and-archive'."
  (interactive)
  (let ((start-of-message (notmuch-show-message-top))
	(start-of-window (window-start)))
    (cond
      ;; Either this message is properly aligned with the start of the
      ;; window or the start of this message is not visible on the
      ;; screen - scroll.
     ((or (= start-of-message start-of-window)
	  (< start-of-message start-of-window))
      (scroll-down)
      ;; If a small number of lines from the previous message are
      ;; visible, realign so that the top of the current message is at
      ;; the top of the screen.
      (when (<= (count-screen-lines (window-start) start-of-message)
		next-screen-context-lines)
	(goto-char (notmuch-show-message-top))
	(notmuch-show-message-adjust))
      ;; Move to the top left of the window.
      (goto-char (window-start)))
     (t
      ;; Move to the previous message.
      (notmuch-show-previous-message)))))

(put 'notmuch-show-reply 'notmuch-prefix-doc "... and prompt for sender")
(defun notmuch-show-reply (&optional prompt-for-sender)
  "Reply to the sender and all recipients of the current message."
  (interactive "P")
  (notmuch-mua-new-reply (notmuch-show-get-message-id) prompt-for-sender t))

(put 'notmuch-show-reply-sender 'notmuch-prefix-doc "... and prompt for sender")
(defun notmuch-show-reply-sender (&optional prompt-for-sender)
  "Reply to the sender of the current message."
  (interactive "P")
  (notmuch-mua-new-reply (notmuch-show-get-message-id) prompt-for-sender nil))

(put 'notmuch-show-forward-message 'notmuch-prefix-doc
     "... and prompt for sender")
(defun notmuch-show-forward-message (&optional prompt-for-sender)
  "Forward the current message."
  (interactive "P")
  (notmuch-mua-new-forward-messages (list (notmuch-show-get-message-id))
				    prompt-for-sender))

(put 'notmuch-show-forward-open-messages 'notmuch-prefix-doc
     "... and prompt for sender")
(defun notmuch-show-forward-open-messages (&optional prompt-for-sender)
  "Forward the currently open messages."
  (interactive "P")
  (let ((open-messages (notmuch-show-get-message-ids-for-open-messages)))
    (unless open-messages
      (error "No open messages to forward."))
    (notmuch-mua-new-forward-messages open-messages prompt-for-sender)))

(defun notmuch-show-resend-message (addresses)
  "Resend the current message."
  (interactive (list (notmuch-address-from-minibuffer "Resend to: ")))
  (when (y-or-n-p (concat "Confirm resend to " addresses " "))
    (notmuch-show-view-raw-message)
    (message-resend addresses)
    (notmuch-bury-or-kill-this-buffer)))

(defun notmuch-show-next-message (&optional pop-at-end)
  "Show the next message.

If a prefix argument is given and this is the last message in the
thread, navigate to the next thread in the parent search buffer."
  (interactive "P")
  (if (notmuch-show-goto-message-next)
      (notmuch-show-message-adjust)
    (if pop-at-end
	(notmuch-show-next-thread)
      (goto-char (point-max)))))

(defun notmuch-show-previous-message ()
  "Show the previous message or the start of the current message."
  (interactive)
  (if (= (point) (notmuch-show-message-top))
      (notmuch-show-goto-message-previous)
    (notmuch-show-move-to-message-top))
  (notmuch-show-message-adjust))

(defun notmuch-show-next-open-message (&optional pop-at-end)
  "Show the next open message.

If a prefix argument is given and this is the last open message
in the thread, navigate to the next thread in the parent search
buffer. Return t if there was a next open message in the thread
to show, nil otherwise."
  (interactive "P")
  (let (r)
    (while (and (setq r (notmuch-show-goto-message-next))
		(not (notmuch-show-message-visible-p))))
    (if r
	(notmuch-show-message-adjust)
      (if pop-at-end
	  (notmuch-show-next-thread)
	(goto-char (point-max))))
    r))

(defun notmuch-show-next-matching-message ()
  "Show the next matching message."
  (interactive)
  (let (r)
    (while (and (setq r (notmuch-show-goto-message-next))
		(not (notmuch-show-get-prop :match))))
    (if r
	(notmuch-show-message-adjust)
      (goto-char (point-max)))))

(defun notmuch-show-open-if-matched ()
  "Open a message if it is matched (whether or not excluded)."
  (let ((props (notmuch-show-get-message-properties)))
    (notmuch-show-message-visible props (plist-get props :match))))

(defun notmuch-show-goto-first-wanted-message ()
  "Move to the first open message and mark it read"
  (goto-char (point-min))
  (unless (notmuch-show-message-visible-p)
    (notmuch-show-next-open-message))
  (when (eobp)
    ;; There are no matched non-excluded messages so open all matched
    ;; (necessarily excluded) messages and go to the first.
    (notmuch-show-mapc 'notmuch-show-open-if-matched)
    (force-window-update)
    (goto-char (point-min))
    (unless (notmuch-show-message-visible-p)
      (notmuch-show-next-open-message))))

(defun notmuch-show-previous-open-message ()
  "Show the previous open message."
  (interactive)
  (while (and (if (= (point) (notmuch-show-message-top))
		  (notmuch-show-goto-message-previous)
		(notmuch-show-move-to-message-top))
	      (not (notmuch-show-message-visible-p))))
  (notmuch-show-message-adjust))

(defun notmuch-show-view-raw-message ()
  "View the original source of the current message."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
	 (buf (get-buffer-create (concat "*notmuch-raw-" id "*")))
	 (inhibit-read-only t))
    (switch-to-buffer buf)
    (erase-buffer)
    (let ((coding-system-for-read 'no-conversion))
      (call-process notmuch-command nil t nil "show" "--format=raw" id))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (view-buffer buf 'kill-buffer-if-not-modified)))

(defun notmuch-show-resume-message ()
  "Resume EDITING the current draft message."
  (interactive)
  (notmuch-draft-resume (notmuch-show-get-message-id)))

(put 'notmuch-show-pipe-message 'notmuch-doc
     "Pipe the contents of the current message to a command.")
(put 'notmuch-show-pipe-message 'notmuch-prefix-doc
     "Pipe the thread as an mbox to a command.")
(defun notmuch-show-pipe-message (entire-thread command)
  "Pipe the contents of the current message (or thread) to COMMAND.

COMMAND will be executed with the raw contents of the current
email message as stdin. Anything printed by the command to stdout
or stderr will appear in the *notmuch-pipe* buffer.

If ENTIRE-THREAD is non-nil (or when invoked with a prefix
argument), COMMAND will receive all open messages in the current
thread (formatted as an mbox) rather than only the current
message."
  (interactive (let ((query-string (if current-prefix-arg
				       "Pipe all open messages to command: "
				     "Pipe message to command: ")))
		 (list current-prefix-arg (read-string query-string))))
  (let (shell-command)
    (if entire-thread
	(setq shell-command
	      (concat notmuch-command " show --format=mbox --exclude=false "
		      (shell-quote-argument
		       (mapconcat 'identity (notmuch-show-get-message-ids-for-open-messages) " OR "))
		      " | " command))
      (setq shell-command
	    (concat notmuch-command " show --format=raw "
		    (shell-quote-argument (notmuch-show-get-message-id)) " | " command)))
    (let ((cwd default-directory)
	  (buf (get-buffer-create (concat "*notmuch-pipe*"))))
      (with-current-buffer buf
	(setq buffer-read-only nil)
	(erase-buffer)
	;; Use the originating buffer's working directory instead of
	;; that of the pipe buffer.
	(cd cwd)
	(let ((exit-code (call-process-shell-command shell-command nil buf)))
	  (goto-char (point-max))
	  (set-buffer-modified-p nil)
	  (setq buffer-read-only t)
	  (unless (zerop exit-code)
	    (switch-to-buffer-other-window buf)
	    (message (format "Command '%s' exited abnormally with code %d"
			     shell-command exit-code))))))))

(defun notmuch-show-tag-message (&rest tag-changes)
  "Change tags for the current message.

TAG-CHANGES is a list of tag operations for `notmuch-tag'."
  (let* ((current-tags (notmuch-show-get-tags))
	 (new-tags (notmuch-update-tags current-tags tag-changes)))
    (unless (equal current-tags new-tags)
      (notmuch-tag (notmuch-show-get-message-id) tag-changes)
      (notmuch-show-set-tags new-tags))))

(defun notmuch-show-tag (tag-changes)
  "Change tags for the current message.

See `notmuch-tag' for information on the format of TAG-CHANGES."
  (interactive (list (notmuch-read-tag-changes (notmuch-show-get-tags)
					       "Tag message")))
  (notmuch-tag (notmuch-show-get-message-id) tag-changes)
  (let* ((current-tags (notmuch-show-get-tags))
	 (new-tags (notmuch-update-tags current-tags tag-changes)))
    (unless (equal current-tags new-tags)
      (notmuch-show-set-tags new-tags))))

(defun notmuch-show-tag-all (tag-changes)
  "Change tags for all messages in the current show buffer.

See `notmuch-tag' for information on the format of TAG-CHANGES."
  (interactive
   (list (let (tags)
	   (notmuch-show-mapc
	    (lambda () (setq tags (append (notmuch-show-get-tags) tags))))
	   (notmuch-read-tag-changes tags "Tag thread"))))
  (notmuch-tag (notmuch-show-get-messages-ids-search) tag-changes)
  (notmuch-show-mapc
   (lambda ()
     (let* ((current-tags (notmuch-show-get-tags))
	    (new-tags (notmuch-update-tags current-tags tag-changes)))
       (unless (equal current-tags new-tags)
	 (notmuch-show-set-tags new-tags))))))

(defun notmuch-show-add-tag (tag-changes)
  "Change tags for the current message (defaulting to add).

Same as `notmuch-show-tag' but sets initial input to '+'."
  (interactive
   (list (notmuch-read-tag-changes (notmuch-show-get-tags) "Tag message" "+")))
  (notmuch-show-tag tag-changes))

(defun notmuch-show-remove-tag (tag-changes)
  "Change tags for the current message (defaulting to remove).

Same as `notmuch-show-tag' but sets initial input to '-'."
  (interactive
   (list (notmuch-read-tag-changes (notmuch-show-get-tags) "Tag message" "-")))
  (notmuch-show-tag tag-changes))

(defun notmuch-show-toggle-visibility-headers ()
  "Toggle the visibility of the current message headers."
  (interactive)
  (let ((props (notmuch-show-get-message-properties)))
    (notmuch-show-headers-visible
     props
     (not (plist-get props :headers-visible))))
  (force-window-update))

(defun notmuch-show-toggle-message ()
  "Toggle the visibility of the current message."
  (interactive)
  (let ((props (notmuch-show-get-message-properties)))
    (notmuch-show-message-visible
     props
     (not (plist-get props :message-visible))))
  (force-window-update))

(put 'notmuch-show-open-or-close-all 'notmuch-doc "Show all messages.")
(put 'notmuch-show-open-or-close-all 'notmuch-prefix-doc "Hide all messages.")
(defun notmuch-show-open-or-close-all ()
  "Set the visibility all of the messages in the current thread.

By default make all of the messages visible. With a prefix
argument, hide all of the messages."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (loop do (notmuch-show-message-visible (notmuch-show-get-message-properties)
					   (not current-prefix-arg))
	  until (not (notmuch-show-goto-message-next))))
  (force-window-update))

(defun notmuch-show-next-button ()
  "Advance point to the next button in the buffer."
  (interactive)
  (forward-button 1))

(defun notmuch-show-previous-button ()
  "Move point back to the previous button in the buffer."
  (interactive)
  (backward-button 1))

(defun notmuch-show-next-thread (&optional show previous)
  "Move to the next item in the search results, if any.

If SHOW is non-nil, open the next item in a show
buffer. Otherwise just highlight the next item in the search
buffer. If PREVIOUS is non-nil, move to the previous item in the
search results instead."
  (interactive "P")
  (let ((parent-buffer notmuch-show-parent-buffer))
    (notmuch-bury-or-kill-this-buffer)
    (when (buffer-live-p parent-buffer)
      (switch-to-buffer parent-buffer)
      (and (if previous
	       (notmuch-search-previous-thread)
	     (notmuch-search-next-thread))
	   show
	   (notmuch-search-show-thread)))))

(defun notmuch-show-next-thread-show ()
  "Show the next thread in the search results, if any."
  (interactive)
  (notmuch-show-next-thread t))

(defun notmuch-show-previous-thread-show ()
  "Show the previous thread in the search results, if any."
  (interactive)
  (notmuch-show-next-thread t t))

(put 'notmuch-show-archive-thread 'notmuch-prefix-doc
     "Un-archive each message in thread.")
(defun notmuch-show-archive-thread (&optional unarchive)
  "Archive each message in thread.

Archive each message currently shown by applying the tag changes
in `notmuch-archive-tags' to each. If a prefix argument is given,
the messages will be \"unarchived\", i.e. the tag changes in
`notmuch-archive-tags' will be reversed.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not archive the
entire thread, but only the messages shown in the current
buffer."
  (interactive "P")
  (when notmuch-archive-tags
    (notmuch-show-tag-all
     (notmuch-tag-change-list notmuch-archive-tags unarchive))))

(defun notmuch-show-archive-thread-then-next ()
  "Archive all messages in the current buffer, then show next thread from search."
  (interactive)
  (notmuch-show-archive-thread)
  (notmuch-show-next-thread t))

(defun notmuch-show-archive-thread-then-exit ()
  "Archive all messages in the current buffer, then exit back to search results."
  (interactive)
  (notmuch-show-archive-thread)
  (notmuch-show-next-thread))

(put 'notmuch-show-archive-message 'notmuch-prefix-doc
     "Un-archive the current message.")
(defun notmuch-show-archive-message (&optional unarchive)
  "Archive the current message.

Archive the current message by applying the tag changes in
`notmuch-archive-tags' to it. If a prefix argument is given, the
message will be \"unarchived\", i.e. the tag changes in
`notmuch-archive-tags' will be reversed."
  (interactive "P")
  (when notmuch-archive-tags
    (apply 'notmuch-show-tag-message
	   (notmuch-tag-change-list notmuch-archive-tags unarchive))))

(defun notmuch-show-archive-message-then-next-or-exit ()
  "Archive the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then exit back
to search results."
  (interactive)
  (notmuch-show-archive-message)
  (notmuch-show-next-open-message t))

(defun notmuch-show-archive-message-then-next-or-next-thread ()
  "Archive the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then show next
thread from search."
  (interactive)
  (notmuch-show-archive-message)
  (unless (notmuch-show-next-open-message)
    (notmuch-show-next-thread t)))

(defun notmuch-show-stash-cc ()
  "Copy CC field of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-cc)))

(put 'notmuch-show-stash-date 'notmuch-prefix-doc
     "Copy timestamp of current message to kill-ring.")
(defun notmuch-show-stash-date (&optional stash-timestamp)
  "Copy date of current message to kill-ring.

If invoked with a prefix argument, copy timestamp of current
message to kill-ring."
  (interactive "P")
  (if stash-timestamp
      (notmuch-common-do-stash (format "%d" (notmuch-show-get-timestamp)))
    (notmuch-common-do-stash (notmuch-show-get-date))))

(defun notmuch-show-stash-filename ()
  "Copy filename of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-filename)))

(defun notmuch-show-stash-from ()
  "Copy From address of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-from)))

(put 'notmuch-show-stash-message-id 'notmuch-prefix-doc
     "Copy thread: query matching current thread to kill-ring.")
(defun notmuch-show-stash-message-id (&optional stash-thread-id)
  "Copy id: query matching the current message to kill-ring.

If invoked with a prefix argument (or STASH-THREAD-ID is
non-nil), copy thread: query matching the current thread to
kill-ring."
  (interactive "P")
  (if stash-thread-id
      (notmuch-common-do-stash notmuch-show-thread-id)
    (notmuch-common-do-stash (notmuch-show-get-message-id))))

(defun notmuch-show-stash-message-id-stripped ()
  "Copy message ID of current message (sans `id:' prefix) to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-message-id t)))

(defun notmuch-show-stash-subject ()
  "Copy Subject field of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-subject)))

(defun notmuch-show-stash-tags ()
  "Copy tags of current message to kill-ring as a comma separated list."
  (interactive)
  (notmuch-common-do-stash (mapconcat 'identity (notmuch-show-get-tags) ",")))

(defun notmuch-show-stash-to ()
  "Copy To address of current message to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-show-get-to)))

(defun notmuch-show-stash-mlarchive-link (&optional mla)
  "Copy an ML Archive URI for the current message to the kill-ring.

This presumes that the message is available at the selected Mailing List Archive.

If optional argument MLA is non-nil, use the provided key instead of prompting
the user (see `notmuch-show-stash-mlarchive-link-alist')."
  (interactive)
  (let ((url (cdr (assoc
		   (or mla
		       (let ((completion-ignore-case t))
			 (completing-read
			  "Mailing List Archive: "
			  notmuch-show-stash-mlarchive-link-alist
			  nil t nil nil
			  notmuch-show-stash-mlarchive-link-default)))
		   notmuch-show-stash-mlarchive-link-alist))))
    (notmuch-common-do-stash
     (if (functionp url)
	 (funcall url (notmuch-show-get-message-id t))
       (concat url (notmuch-show-get-message-id t))))))

(defun notmuch-show-stash-mlarchive-link-and-go (&optional mla)
  "Copy an ML Archive URI for the current message to the kill-ring and visit it.

This presumes that the message is available at the selected Mailing List Archive.

If optional argument MLA is non-nil, use the provided key instead of prompting
the user (see `notmuch-show-stash-mlarchive-link-alist')."
  (interactive)
  (notmuch-show-stash-mlarchive-link mla)
  (browse-url (current-kill 0 t)))

(defun notmuch-show-stash-git-helper (addresses prefix)
  "Escape, trim, quote, and add PREFIX to each address in list of ADDRESSES, and return the result as a single string."
  (mapconcat (lambda (x)
	       (concat prefix "\""
		       ;; escape double-quotes
		       (replace-regexp-in-string
			"\"" "\\\\\""
			;; trim leading and trailing spaces
			(replace-regexp-in-string
			 "\\(^ *\\| *$\\)" ""
			 x)) "\""))
	     addresses " "))

(put 'notmuch-show-stash-git-send-email 'notmuch-prefix-doc
     "Copy From/To/Cc of current message to kill-ring in a form suitable for pasting to git send-email command line.")

(defun notmuch-show-stash-git-send-email (&optional no-in-reply-to)
  "Copy From/To/Cc/Message-Id of current message to kill-ring in a form suitable for pasting to git send-email command line.

If invoked with a prefix argument (or NO-IN-REPLY-TO is non-nil),
omit --in-reply-to=<Message-Id>."
  (interactive "P")
  (notmuch-common-do-stash
   (mapconcat 'identity
	      (remove ""
		      (list
		       (notmuch-show-stash-git-helper
			(message-tokenize-header (notmuch-show-get-from)) "--to=")
		       (notmuch-show-stash-git-helper
			(message-tokenize-header (notmuch-show-get-to)) "--to=")
		       (notmuch-show-stash-git-helper
			(message-tokenize-header (notmuch-show-get-cc)) "--cc=")
		       (unless no-in-reply-to
			 (notmuch-show-stash-git-helper
			  (list (notmuch-show-get-message-id t)) "--in-reply-to="))))
	      " ")))

;; Interactive part functions and their helpers

(defun notmuch-show-generate-part-buffer (msg part)
  "Return a temporary buffer containing the specified part's content."
  (let ((buf (generate-new-buffer " *notmuch-part*"))
	(process-crypto notmuch-show-process-crypto))
    (with-current-buffer buf
      ;; This is always used in the content of mm handles, which
      ;; expect undecoded, binary part content.
      (insert (notmuch-get-bodypart-binary msg part process-crypto)))
    buf))

(defun notmuch-show-current-part-handle (&optional mime-type)
  "Return an mm-handle for the part containing point.

This creates a temporary buffer for the part's content; the
caller is responsible for killing this buffer as appropriate.  If
MIME-TYPE is given then set the handle's mime-type to MIME-TYPE."
  (let* ((msg (notmuch-show-get-message-properties))
	 (part (notmuch-show-get-part-properties))
	 (buf (notmuch-show-generate-part-buffer msg part))
	 (computed-type (or mime-type (plist-get part :computed-type)))
	 (filename (plist-get part :filename))
	 (disposition (if filename `(attachment (filename . ,filename)))))
    (mm-make-handle buf (list computed-type) nil nil disposition)))

(defun notmuch-show-apply-to-current-part-handle (fn &optional mime-type)
  "Apply FN to an mm-handle for the part containing point.

This ensures that the temporary buffer created for the mm-handle
is destroyed when FN returns. If MIME-TYPE is given then force
part to be treated as if it had that mime-type."
  (let ((handle (notmuch-show-current-part-handle mime-type)))
    ;; emacs 24.3+ puts stdout/stderr into the calling buffer so we
    ;; call it from a temp-buffer, unless
    ;; notmuch-show-attachment-debug is non-nil in which case we put
    ;; it in " *notmuch-part*".
    (unwind-protect
	(if notmuch-show-attachment-debug
	    (with-current-buffer (generate-new-buffer " *notmuch-part*")
	      (funcall fn handle))
	  (with-temp-buffer
	    (funcall fn handle)))
      (kill-buffer (mm-handle-buffer handle)))))

(defun notmuch-show-part-button-default (&optional button)
  (interactive)
  (let ((button (or button (button-at (point)))))
    ;; Try to toggle the part, if that fails then call the default
    ;; action. The toggle fails if the part has no emacs renderable
    ;; content.
    (unless (notmuch-show-toggle-part-invisibility button)
      (call-interactively notmuch-show-part-button-default-action))))

(defun notmuch-show-save-part ()
  "Save the MIME part containing point to a file."
  (interactive)
  (notmuch-show-apply-to-current-part-handle #'mm-save-part))

(defun notmuch-show-view-part ()
  "View the MIME part containing point in an external viewer."
  (interactive)
  ;; Set mm-inlined-types to nil to force an external viewer
  (let ((mm-inlined-types nil))
    (notmuch-show-apply-to-current-part-handle #'mm-display-part)))

(defun notmuch-show-interactively-view-part ()
  "View the MIME part containing point, prompting for a viewer."
  (interactive)
  (notmuch-show-apply-to-current-part-handle #'mm-interactively-view-part))

(defun notmuch-show-pipe-part ()
  "Pipe the MIME part containing point to an external command."
  (interactive)
  (notmuch-show-apply-to-current-part-handle #'mm-pipe-part))


(defun notmuch-show--mm-display-part (handle)
  "Use mm-display-part to display HANDLE in a new buffer.

If the part is displayed in an external application then close
the new buffer."
  (let ((buf (get-buffer-create (generate-new-buffer-name
				 (concat " *notmuch-internal-part*")))))
    (switch-to-buffer buf)
    (if (eq (mm-display-part handle) 'external)
	(kill-buffer buf)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (view-buffer buf 'kill-buffer-if-not-modified))))

(defun notmuch-show-choose-mime-of-part (mime-type)
  "Choose the mime type to use for displaying part"
  (interactive
   (list (completing-read "Mime type to use (default text/plain): "
			  (mailcap-mime-types) nil nil nil nil "text/plain")))
  (notmuch-show-apply-to-current-part-handle #'notmuch-show--mm-display-part mime-type))

(defun notmuch-show-imenu-prev-index-position-function ()
  "Move point to previous message in notmuch-show buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (if (bobp)
      nil
    (notmuch-show-previous-message)
    t))

(defun notmuch-show-imenu-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (back-to-indentation)
  (buffer-substring-no-properties (if notmuch-show-imenu-indent
				      (line-beginning-position)
				    (point))
				  (line-end-position)))

(provide 'notmuch-show)

;;; notmuch-show.el ends here

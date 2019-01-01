;;; notmuch-address.el --- address completion with notmuch
;;
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
;; Authors: David Edmondson <dme@dme.org>

;;; Code:

(require 'message)
(require 'notmuch-parser)
(require 'notmuch-lib)
(require 'notmuch-company)
;;
(declare-function company-manual-begin "company")

(defvar notmuch-address-last-harvest 0
  "Time of last address harvest")

(defvar notmuch-address-completions (make-hash-table :test 'equal)
  "Hash of email addresses for completion during email composition.
  This variable is set by calling `notmuch-address-harvest'.")

(defvar notmuch-address-full-harvest-finished nil
  "t indicates that full completion address harvesting has been
finished. Use notmuch-address--harvest-ready to access as that
will load a saved hash if necessary (and available).")

(defun notmuch-address--harvest-ready ()
  "Return t if there is a full address hash available.

If the hash is not present it attempts to load a saved hash."
  (or notmuch-address-full-harvest-finished
      (notmuch-address--load-address-hash)))

(defcustom notmuch-address-command 'internal
  "Determines how address completion candidates are generated.

If it is a string then that string should be an external program
which must take a single argument (searched string) and output a
list of completion candidates, one per line.

Alternatively, it can be the symbol 'internal, in which case
internal completion is used; the variable
`notmuch-address-internal-completion` can be used to customize
this case.

Finally, if this variable is nil then address completion is
disabled."
  :type '(radio
	  (const :tag "Use internal address completion" internal)
	  (const :tag "Disable address completion" nil)
	  (string :tag "Use external completion command"))
  :group 'notmuch-send
  :group 'notmuch-address
  :group 'notmuch-external)

(defcustom notmuch-address-internal-completion '(sent nil)
  "Determines how internal address completion generates candidates.

This should be a list of the form '(DIRECTION FILTER), where
 DIRECTION is either sent or received and specifies whether the
 candidates are searched in messages sent by the user or received
 by the user (note received by is much faster), and FILTER is
 either nil or a filter-string, such as \"date:1y..\" to append
 to the query."
  :type '(list :tag "Use internal address completion"
	       (radio
		:tag "Base completion on messages you have"
		:value sent
		(const :tag "sent (more accurate)" sent)
		(const :tag "received (faster)" received))
	       (radio :tag "Filter messages used for completion"
		      (const :tag "Use all messages" nil)
		      (string :tag "Filter query")))
  ;; We override set so that we can clear the cache when this changes
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq notmuch-address-last-harvest 0)
	 (setq notmuch-address-completions (clrhash notmuch-address-completions))
	 (setq notmuch-address-full-harvest-finished nil))
  :group 'notmuch-send
  :group 'notmuch-address
  :group 'notmuch-external)

(defcustom notmuch-address-save-filename nil
  "Filename to save the cached completion addresses.

All the addresses notmuch uses for address completion will be
cached in this file. This has obvious privacy implications so you
should make sure it is not somewhere publicly readable."
  :type '(choice (const :tag "Off" nil)
		 (file :tag "Filename"))
  :group 'notmuch-send
  :group 'notmuch-address
  :group 'notmuch-external)

(defcustom notmuch-address-selection-function 'notmuch-address-selection-function
  "The function to select address from given list. The function is
called with PROMPT, COLLECTION, and INITIAL-INPUT as arguments
(subset of what `completing-read' can be called with).
While executed the value of `completion-ignore-case' is t.
See documentation of function `notmuch-address-selection-function'
to know how address selection is made by default."
  :type 'function
  :group 'notmuch-send
  :group 'notmuch-address
  :group 'notmuch-external)

(defcustom notmuch-address-post-completion-functions nil
  "Functions called after completing address.

The completed address is passed as an argument to each function.
Note that this hook will be invoked for completion in headers
matching `notmuch-address-completion-headers-regexp'.
"
  :type 'hook
  :group 'notmuch-address
  :group 'notmuch-hooks)

(defun notmuch-address-selection-function (prompt collection initial-input)
  "Call (`completing-read'
      PROMPT COLLECTION nil nil INITIAL-INPUT 'notmuch-address-history)"
  (completing-read
   prompt collection nil nil initial-input 'notmuch-address-history))

(defvar notmuch-address-completion-headers-regexp
  "^\\(Resent-\\)?\\(To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):")

(defvar notmuch-address-history nil)

(defun notmuch-address-message-insinuate ()
  (message "calling notmuch-address-message-insinuate is no longer needed"))

(defcustom notmuch-address-use-company t
  "If available, use company mode for address completion"
  :type 'boolean
  :group 'notmuch-send
  :group 'notmuch-address)

(defun notmuch-address-setup ()
  (let* ((setup-company (and notmuch-address-use-company
			   (require 'company nil t)))
	 (pair (cons notmuch-address-completion-headers-regexp
		       #'notmuch-address-expand-name)))
      (when setup-company
	(notmuch-company-setup))
      (unless (member pair message-completion-alist)
	(setq message-completion-alist
	      (push pair message-completion-alist)))))

(defun notmuch-address-toggle-internal-completion ()
  "Toggle use of internal completion for current buffer.

This overrides the global setting for address completion and
toggles the setting in this buffer."
  (interactive)
  (if (local-variable-p 'notmuch-address-command)
      (kill-local-variable 'notmuch-address-command)
    (notmuch-setq-local notmuch-address-command 'internal))
  (if (boundp 'company-idle-delay)
      (if (local-variable-p 'company-idle-delay)
	  (kill-local-variable 'company-idle-delay)
	(notmuch-setq-local company-idle-delay nil))))

(defun notmuch-address-matching (substring)
  "Returns a list of completion candidates matching SUBSTRING.
The candidates are taken from `notmuch-address-completions'."
  (let ((candidates)
	(re (regexp-quote substring)))
    (maphash (lambda (key val)
	       (when (string-match re key)
		 (push key candidates)))
	     notmuch-address-completions)
    candidates))

(defun notmuch-address-options (original)
  "Returns a list of completion candidates. Uses either
elisp-based implementation or older implementation requiring
external commands."
  (cond
   ((eq notmuch-address-command 'internal)
    (unless (notmuch-address--harvest-ready)
      ;; First, run quick synchronous harvest based on what the user
      ;; entered so far
      (notmuch-address-harvest original t))
    (prog1 (notmuch-address-matching original)
      ;; Then start the (potentially long-running) full asynchronous harvest if necessary
      (notmuch-address-harvest-trigger)))
   (t
    (process-lines notmuch-address-command original))))

(defun notmuch-address-expand-name ()
  (cond
   ((and (eq notmuch-address-command 'internal)
	 notmuch-address-use-company
	 (bound-and-true-p company-mode))
    (company-manual-begin))
   (notmuch-address-command
    (let* ((end (point))
	   (beg (save-excursion
		  (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
		  (goto-char (match-end 0))
		  (point)))
	   (orig (buffer-substring-no-properties beg end))
	   (completion-ignore-case t)
	   (options (with-temp-message "Looking for completion candidates..."
		      (notmuch-address-options orig)))
	   (num-options (length options))
	   (chosen (cond
		    ((eq num-options 0)
		     nil)
		    ((eq num-options 1)
		     (car options))
		    (t
		     (funcall notmuch-address-selection-function
			      (format "Address (%s matches): " num-options)
			      ;; We put the first match as the initial
			      ;; input; we put all the matches as
			      ;; possible completions, moving the
			      ;; first match to the end of the list
			      ;; makes cursor up/down in the list work
			      ;; better.
			      (append (cdr options) (list (car options)))
			      (car options))))))
      (if chosen
	  (progn
	    (push chosen notmuch-address-history)
	    (delete-region beg end)
	    (insert chosen)
	    (run-hook-with-args 'notmuch-address-post-completion-functions chosen))
	(message "No matches.")
	(ding))))
   (t nil)))

;; Copied from `w3m-which-command'.
(defun notmuch-address-locate-command (command)
  "Return non-nil if `command' is an executable either on
`exec-path' or an absolute pathname."
  (when (stringp command)
    (if (and (file-name-absolute-p command)
	     (file-executable-p command))
	command
      (setq command (file-name-nondirectory command))
      (catch 'found-command
	(let (bin)
	  (dolist (dir exec-path)
	    (setq bin (expand-file-name command dir))
	    (when (or (and (file-executable-p bin)
			   (not (file-directory-p bin)))
		      (and (file-executable-p (setq bin (concat bin ".exe")))
			   (not (file-directory-p bin))))
	      (throw 'found-command bin))))))))

(defun notmuch-address-harvest-addr (result)
  (let ((name-addr (plist-get result :name-addr)))
    (puthash name-addr t notmuch-address-completions)))

(defun notmuch-address-harvest-handle-result (obj)
  (notmuch-address-harvest-addr obj))

(defun notmuch-address-harvest-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
	(goto-char (point-max))
	(insert string))
      (notmuch-sexp-parse-partial-list
       'notmuch-address-harvest-handle-result (process-buffer proc)))))

(defvar notmuch-address-harvest-procs '(nil . nil)
  "The currently running harvests.

The car is a partial harvest, and the cdr is a full harvest")

(defun notmuch-address-harvest (&optional addr-prefix synchronous callback)
  "Collect addresses completion candidates.

It queries the notmuch database for messages sent/received (as
configured with `notmuch-address-command`) by the user, collects
destination/source addresses from those messages and stores them
in `notmuch-address-completions'.

If ADDR-PREFIX is not nil, only messages with to/from addresses
matching ADDR-PREFIX*' are queried.

Address harvesting may take some time so the address collection runs
asynchronously unless SYNCHRONOUS is t. In case of asynchronous
execution, CALLBACK is called when harvesting finishes."

  (let* ((sent (eq (car notmuch-address-internal-completion) 'sent))
	 (config-query (cadr notmuch-address-internal-completion))
	 (prefix-query (when addr-prefix
			 (format "%s:%s*" (if sent "to" "from") addr-prefix)))
	 (from-or-to-me-query
	  (mapconcat (lambda (x)
		       (concat (if sent "from:" "to:") x))
		     (notmuch-user-emails) " or "))
	 (query (if (or prefix-query config-query)
		    (concat (format "(%s)" from-or-to-me-query)
			    (when prefix-query
			      (format " and (%s)" prefix-query))
			    (when config-query
			      (format " and (%s)" config-query)))
		  from-or-to-me-query))
	 (args `("address" "--format=sexp" "--format-version=4"
		 ,(if sent "--output=recipients" "--output=sender")
		 "--deduplicate=address"
		 ,query)))
    (if synchronous
	(mapc #'notmuch-address-harvest-addr
				   (apply 'notmuch-call-notmuch-sexp args))
      ;; Asynchronous
      (let* ((current-proc (if addr-prefix
			       (car notmuch-address-harvest-procs)
			     (cdr notmuch-address-harvest-procs)))
	     (proc-name (format "notmuch-address-%s-harvest"
				(if addr-prefix "partial" "full")))
	     (proc-buf (concat " *" proc-name "*")))
	;; Kill any existing process
	(when current-proc
	  (kill-buffer (process-buffer current-proc))) ; this also kills the process

	(setq current-proc
	      (apply 'notmuch-start-notmuch proc-name proc-buf
		     callback				; process sentinel
		     args))
	(set-process-filter current-proc 'notmuch-address-harvest-filter)
	(set-process-query-on-exit-flag current-proc nil)
	(if addr-prefix
	    (setcar notmuch-address-harvest-procs current-proc)
	  (setcdr notmuch-address-harvest-procs current-proc)))))
  ;; return value
  nil)

(defvar notmuch-address--save-hash-version 1
  "Version format of the save hash.")

(defun notmuch-address--get-address-hash ()
  "Returns the saved address hash as a plist.

Returns nil if the save file does not exist, or it does not seem
to be a saved address hash."
  (when notmuch-address-save-filename
    (condition-case nil
	(with-temp-buffer
	  (insert-file-contents notmuch-address-save-filename)
	  (let ((name (read (current-buffer)))
		(plist (read (current-buffer))))
	    ;; We do two simple sanity checks on the loaded file. We just
	    ;; check a version is specified, not that it is the current
	    ;; version, as we are allowed to over-write and a save-file with
	    ;; an older version.
	    (when (and (string= name "notmuch-address-hash")
		       (plist-get plist :version))
	      plist)))
      ;; The error case catches any of the reads failing.
      (error nil))))

(defun notmuch-address--load-address-hash ()
  "Read the saved address hash and set the corresponding variables."
  (let ((load-plist (notmuch-address--get-address-hash)))
    (when (and load-plist
	       ;; If the user's setting have changed, or the version
	       ;; has changed, return nil to make sure the new settings
	       ;; take effect.
	       (equal (plist-get load-plist :completion-settings)
		      notmuch-address-internal-completion)
	       (equal (plist-get load-plist :version)
		      notmuch-address--save-hash-version))
      (setq notmuch-address-last-harvest (plist-get load-plist :last-harvest)
	    notmuch-address-completions (plist-get load-plist :completions)
	    notmuch-address-full-harvest-finished t)
      ;; Return t to say load was successful.
      t)))

(defun notmuch-address--save-address-hash ()
  (when notmuch-address-save-filename
    (if (or (not (file-exists-p notmuch-address-save-filename))
	      ;; The file exists, check it is a file we saved
	    (notmuch-address--get-address-hash))
	(with-temp-file notmuch-address-save-filename
	  (let ((save-plist (list :version notmuch-address--save-hash-version
				  :completion-settings notmuch-address-internal-completion
				  :last-harvest notmuch-address-last-harvest
				  :completions notmuch-address-completions)))
	    (print "notmuch-address-hash" (current-buffer))
	    (print save-plist (current-buffer))))
      (message "\
Warning: notmuch-address-save-filename %s exists but doesn't
appear to be an address savefile.  Not overwriting."
	       notmuch-address-save-filename))))

(defun notmuch-address-harvest-trigger ()
  (let ((now (float-time)))
    (when (> (- now notmuch-address-last-harvest) 86400)
      (setq notmuch-address-last-harvest now)
      (notmuch-address-harvest nil nil
			       (lambda (proc event)
				 ;; If harvest fails, we want to try
				 ;; again when the trigger is next
				 ;; called
				 (if (string= event "finished\n")
				     (progn
				       (notmuch-address--save-address-hash)
				       (setq notmuch-address-full-harvest-finished t))
				   (setq notmuch-address-last-harvest 0)))))))

;;

(defun notmuch-address-from-minibuffer (prompt)
  (if (not notmuch-address-command)
      (read-string prompt)
    (let ((rmap (copy-keymap minibuffer-local-map))
	  (omap minibuffer-local-map))
      ;; Configure TAB to start completion when executing read-string.
      ;; "Original" minibuffer keymap is restored just before calling
      ;; notmuch-address-expand-name as it may also use minibuffer-local-map
      ;; (completing-read probably does not but if something else is used there).
      (define-key rmap (kbd "TAB") (lambda ()
				     (interactive)
				     (let ((enable-recursive-minibuffers t)
					   (minibuffer-local-map omap))
				       (notmuch-address-expand-name))))
      (let ((minibuffer-local-map rmap))
	(read-string prompt)))))

;;

(provide 'notmuch-address)

;;; notmuch-address.el ends here

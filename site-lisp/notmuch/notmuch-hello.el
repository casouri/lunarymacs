;;; notmuch-hello.el --- welcome to notmuch, a frontend
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

(eval-when-compile (require 'cl))
(require 'widget)
(require 'wid-edit) ; For `widget-forward'.

(require 'notmuch-lib)
(require 'notmuch-mua)

(declare-function notmuch-search "notmuch" (&optional query oldest-first target-thread target-line continuation))
(declare-function notmuch-poll "notmuch" ())
(declare-function notmuch-tree "notmuch-tree"
                  (&optional query query-context target buffer-name open-target))

(defun notmuch-saved-search-get (saved-search field)
  "Get FIELD from SAVED-SEARCH.

If SAVED-SEARCH is a plist, this is just `plist-get', but for
backwards compatibility, this also deals with the two other
possible formats for SAVED-SEARCH: cons cells (NAME . QUERY) and
lists (NAME QUERY COUNT-QUERY)."
  (cond
   ((keywordp (car saved-search))
    (plist-get saved-search field))
   ;; It is not a plist so it is an old-style entry.
   ((consp (cdr saved-search)) ;; It is a list (NAME QUERY COUNT-QUERY)
    (case field
      (:name (first saved-search))
      (:query (second saved-search))
      (:count-query (third saved-search))
      (t nil)))
   (t  ;; It is a cons-cell (NAME . QUERY)
    (case field
      (:name (car saved-search))
      (:query (cdr saved-search))
      (t nil)))))

(defun notmuch-hello-saved-search-to-plist (saved-search)
  "Return a copy of SAVED-SEARCH in plist form.

If saved search is a plist then just return a copy. In other
cases, for backwards compatibility, convert to plist form and
return that."
  (if (keywordp (car saved-search))
      (copy-seq saved-search)
    (let ((fields (list :name :query :count-query))
	  plist-search)
      (dolist (field fields plist-search)
	(let ((string (notmuch-saved-search-get saved-search field)))
	  (when string
	    (setq plist-search (append plist-search (list field string)))))))))

(defun notmuch-hello--saved-searches-to-plist (symbol)
  "Extract a saved-search variable into plist form.

The new style saved search is just a plist, but for backwards
compatibility we use this function to extract old style saved
searches so they still work in customize."
  (let ((saved-searches (default-value symbol)))
    (mapcar #'notmuch-hello-saved-search-to-plist saved-searches)))

(define-widget 'notmuch-saved-search-plist 'list
  "A single saved search property list."
  :tag "Saved Search"
  :args '((list :inline t
		:format "%v"
		(group :format "%v" :inline t (const :format "   Name: " :name) (string :format "%v"))
		(group :format "%v" :inline t (const :format "  Query: " :query) (string :format "%v")))
	  (checklist :inline t
		     :format "%v"
		     (group :format "%v" :inline t (const :format "Shortcut key: " :key) (key-sequence :format "%v"))
		     (group :format "%v" :inline t (const :format "Count-Query: " :count-query) (string :format "%v"))
		     (group :format "%v" :inline t (const :format "" :sort-order)
			    (choice :tag " Sort Order"
				    (const :tag "Default" nil)
				    (const :tag "Oldest-first" oldest-first)
				    (const :tag "Newest-first" newest-first)))
		     (group :format "%v" :inline t (const :format "" :search-type)
			    (choice :tag " Search Type"
				    (const :tag "Search mode" nil)
				    (const :tag "Tree mode" tree))))))

(defcustom notmuch-saved-searches
  `((:name "inbox" :query "tag:inbox" :key ,(kbd "i"))
    (:name "unread" :query "tag:unread" :key ,(kbd "u"))
    (:name "flagged" :query "tag:flagged" :key ,(kbd "f"))
    (:name "sent" :query "tag:sent" :key ,(kbd "t"))
    (:name "drafts" :query "tag:draft" :key ,(kbd "d"))
    (:name "all mail" :query "*" :key ,(kbd "a")))
  "A list of saved searches to display.

The saved search can be given in 3 forms. The preferred way is as
a plist. Supported properties are

  :name            Name of the search (required).
  :query           Search to run (required).
  :key             Optional shortcut key for `notmuch-jump-search'.
  :count-query     Optional extra query to generate the count
                   shown. If not present then the :query property
                   is used.
  :sort-order      Specify the sort order to be used for the search.
                   Possible values are 'oldest-first 'newest-first or
                   nil. Nil means use the default sort order.
  :search-type     Specify whether to run the search in search-mode
                   or tree mode. Set to 'tree to specify tree
                   mode, set to nil (or anything except tree) to
                   specify search mode.

Other accepted forms are a cons cell of the form (NAME . QUERY)
or a list of the form (NAME QUERY COUNT-QUERY)."
;; The saved-search format is also used by the all-tags notmuch-hello
;; section. This section generates its own saved-search list in one of
;; the latter two forms.

  :get 'notmuch-hello--saved-searches-to-plist
  :type '(repeat notmuch-saved-search-plist)
  :tag "List of Saved Searches"
  :group 'notmuch-hello)

(defcustom notmuch-hello-recent-searches-max 10
  "The number of recent searches to display."
  :type 'integer
  :group 'notmuch-hello)

(defcustom notmuch-show-empty-saved-searches nil
  "Should saved searches with no messages be listed?"
  :type 'boolean
  :group 'notmuch-hello)

(defun notmuch-sort-saved-searches (saved-searches)
  "Generate an alphabetically sorted saved searches list."
  (sort (copy-sequence saved-searches)
	(lambda (a b)
	  (string< (notmuch-saved-search-get a :name)
		   (notmuch-saved-search-get b :name)))))

(defcustom notmuch-saved-search-sort-function nil
  "Function used to sort the saved searches for the notmuch-hello view.

This variable controls how saved searches should be sorted. No
sorting (nil) displays the saved searches in the order they are
stored in `notmuch-saved-searches'. Sort alphabetically sorts the
saved searches in alphabetical order. Custom sort function should
be a function or a lambda expression that takes the saved
searches list as a parameter, and returns a new saved searches
list to be used. For compatibility with the various saved-search
formats it should use notmuch-saved-search-get to access the
fields of the search."
  :type '(choice (const :tag "No sorting" nil)
		 (const :tag "Sort alphabetically" notmuch-sort-saved-searches)
		 (function :tag "Custom sort function"
			   :value notmuch-sort-saved-searches))
  :group 'notmuch-hello)

(defvar notmuch-hello-indent 4
  "How much to indent non-headers.")

(defcustom notmuch-show-logo t
  "Should the notmuch logo be shown?"
  :type 'boolean
  :group 'notmuch-hello)

(defcustom notmuch-show-all-tags-list nil
  "Should all tags be shown in the notmuch-hello view?"
  :type 'boolean
  :group 'notmuch-hello)

(defcustom notmuch-hello-tag-list-make-query nil
  "Function or string to generate queries for the all tags list.

This variable controls which query results are shown for each tag
in the \"all tags\" list. If nil, it will use all messages with
that tag. If this is set to a string, it is used as a filter for
messages having that tag (equivalent to \"tag:TAG and (THIS-VARIABLE)\").
Finally this can be a function that will be called for each tag and
should return a filter for that tag, or nil to hide the tag."
  :type '(choice (const :tag "All messages" nil)
		 (const :tag "Unread messages" "tag:unread")
		 (string :tag "Custom filter"
			 :value "tag:unread")
		 (function :tag "Custom filter function"))
  :group 'notmuch-hello)

(defcustom notmuch-hello-hide-tags nil
  "List of tags to be hidden in the \"all tags\"-section."
  :type '(repeat string)
  :group 'notmuch-hello)

(defface notmuch-hello-logo-background
  '((((class color)
      (background dark))
     (:background "#5f5f5f"))
    (((class color)
      (background light))
     (:background "white")))
  "Background colour for the notmuch logo."
  :group 'notmuch-hello
  :group 'notmuch-faces)

(defcustom notmuch-column-control t
  "Controls the number of columns for saved searches/tags in notmuch view.

This variable has three potential sets of values:

- t: automatically calculate the number of columns possible based
  on the tags to be shown and the window width,
- an integer: a lower bound on the number of characters that will
  be used to display each column,
- a float: a fraction of the window width that is the lower bound
  on the number of characters that should be used for each
  column.

So:
- if you would like two columns of tags, set this to 0.5.
- if you would like a single column of tags, set this to 1.0.
- if you would like tags to be 30 characters wide, set this to
  30.
- if you don't want to worry about all of this nonsense, leave
  this set to `t'."
  :type '(choice
	  (const :tag "Automatically calculated" t)
	  (integer :tag "Number of characters")
	  (float :tag "Fraction of window"))
  :group 'notmuch-hello)

(defcustom notmuch-hello-thousands-separator " "
  "The string used as a thousands separator.

Typically \",\" in the US and UK and \".\" or \" \" in Europe.
The latter is recommended in the SI/ISO 31-0 standard and by the
International Bureau of Weights and Measures."
  :type 'string
  :group 'notmuch-hello)

(defcustom notmuch-hello-mode-hook nil
  "Functions called after entering `notmuch-hello-mode'."
  :type 'hook
  :group 'notmuch-hello
  :group 'notmuch-hooks)

(defcustom notmuch-hello-refresh-hook nil
  "Functions called after updating a `notmuch-hello' buffer."
  :type 'hook
  :group 'notmuch-hello
  :group 'notmuch-hooks)

(defvar notmuch-hello-url "https://notmuchmail.org"
  "The `notmuch' web site.")

(defvar notmuch-hello-custom-section-options
  '((:filter (string :tag "Filter for each tag"))
    (:filter-count (string :tag "Different filter to generate message counts"))
    (:initially-hidden (const :tag "Hide this section on startup" t))
    (:show-empty-searches (const :tag "Show queries with no matching messages" t))
    (:hide-if-empty (const :tag "Hide this section if all queries are empty
\(and not shown by show-empty-searches)" t)))
  "Various customization-options for notmuch-hello-tags/query-section.")

(define-widget 'notmuch-hello-tags-section 'lazy
  "Customize-type for notmuch-hello tag-list sections."
  :tag "Customized tag-list section (see docstring for details)"
  :type
  `(list :tag ""
	 (const :tag "" notmuch-hello-insert-tags-section)
	 (string :tag "Title for this section")
	 (plist
	  :inline t
	  :options
	  ,(append notmuch-hello-custom-section-options
		   '((:hide-tags (repeat :tag "Tags that will be hidden"
					 string)))))))

(define-widget 'notmuch-hello-query-section 'lazy
  "Customize-type for custom saved-search-like sections"
  :tag "Customized queries section (see docstring for details)"
  :type
  `(list :tag ""
	 (const :tag "" notmuch-hello-insert-searches)
	 (string :tag "Title for this section")
	 (repeat :tag "Queries"
		 (cons (string :tag "Name") (string :tag "Query")))
	 (plist :inline t :options ,notmuch-hello-custom-section-options)))

(defcustom notmuch-hello-sections
  (list #'notmuch-hello-insert-header
	#'notmuch-hello-insert-saved-searches
	#'notmuch-hello-insert-search
	#'notmuch-hello-insert-recent-searches
	#'notmuch-hello-insert-alltags
	#'notmuch-hello-insert-footer)
  "Sections for notmuch-hello.

The list contains functions which are used to construct sections in
notmuch-hello buffer.  When notmuch-hello buffer is constructed,
these functions are run in the order they appear in this list.  Each
function produces a section simply by adding content to the current
buffer.  A section should not end with an empty line, because a
newline will be inserted after each section by `notmuch-hello'.

Each function should take no arguments. The return value is
ignored.

For convenience an element can also be a list of the form (FUNC ARG1
ARG2 .. ARGN) in which case FUNC will be applied to the rest of the
list.

A \"Customized tag-list section\" item in the customize-interface
displays a list of all tags, optionally hiding some of them. It
is also possible to filter the list of messages matching each tag
by an additional filter query. Similarly, the count of messages
displayed next to the buttons can be generated by applying a
different filter to the tag query. These filters are also
supported for \"Customized queries section\" items."
  :group 'notmuch-hello
  :type
  '(repeat
    (choice (function-item notmuch-hello-insert-header)
	    (function-item notmuch-hello-insert-saved-searches)
	    (function-item notmuch-hello-insert-search)
	    (function-item notmuch-hello-insert-recent-searches)
	    (function-item notmuch-hello-insert-alltags)
	    (function-item notmuch-hello-insert-footer)
	    (function-item notmuch-hello-insert-inbox)
	    notmuch-hello-tags-section
	    notmuch-hello-query-section
	    (function :tag "Custom section"))))

(defcustom notmuch-hello-auto-refresh t
  "Automatically refresh when returning to the notmuch-hello buffer."
  :group 'notmuch-hello
  :type 'boolean)

(defvar notmuch-hello-hidden-sections nil
  "List of sections titles whose contents are hidden")

(defvar notmuch-hello-first-run t
  "True if `notmuch-hello' is run for the first time, set to nil
afterwards.")

(defun notmuch-hello-nice-number (n)
  (let (result)
    (while (> n 0)
      (push (% n 1000) result)
      (setq n (/ n 1000)))
    (setq result (or result '(0)))
    (apply #'concat
     (number-to-string (car result))
     (mapcar (lambda (elem)
	      (format "%s%03d" notmuch-hello-thousands-separator elem))
	     (cdr result)))))

(defun notmuch-hello-trim (search)
  "Trim whitespace."
  (if (string-match "^[[:space:]]*\\(.*[^[:space:]]\\)[[:space:]]*$" search)
      (match-string 1 search)
    search))

(defun notmuch-hello-search (&optional search)
  (unless (null search)
    (setq search (notmuch-hello-trim search))
    (let ((history-delete-duplicates t))
      (add-to-history 'notmuch-search-history search)))
  (notmuch-search search notmuch-search-oldest-first))

(defun notmuch-hello-add-saved-search (widget)
  (interactive)
  (let ((search (widget-value
		 (symbol-value
		  (widget-get widget :notmuch-saved-search-widget))))
	(name (completing-read "Name for saved search: "
			       notmuch-saved-searches)))
    ;; If an existing saved search with this name exists, remove it.
    (setq notmuch-saved-searches
	  (loop for elem in notmuch-saved-searches
		if (not (equal name
			       (notmuch-saved-search-get elem :name)))
		collect elem))
    ;; Add the new one.
    (customize-save-variable 'notmuch-saved-searches
			     (add-to-list 'notmuch-saved-searches
					  (list :name name :query search) t))
    (message "Saved '%s' as '%s'." search name)
    (notmuch-hello-update)))

(defun notmuch-hello-delete-search-from-history (widget)
  (interactive)
  (let ((search (widget-value
		 (symbol-value
		  (widget-get widget :notmuch-saved-search-widget)))))
    (setq notmuch-search-history (delete search
					 notmuch-search-history))
    (notmuch-hello-update)))

(defun notmuch-hello-longest-label (searches-alist)
  (or (loop for elem in searches-alist
	    maximize (length (notmuch-saved-search-get elem :name)))
      0))

(defun notmuch-hello-reflect-generate-row (ncols nrows row list)
  (let ((len (length list)))
    (loop for col from 0 to (- ncols 1)
	  collect (let ((offset (+ (* nrows col) row)))
		    (if (< offset len)
			(nth offset list)
		      ;; Don't forget to insert an empty slot in the
		      ;; output matrix if there is no corresponding
		      ;; value in the input matrix.
		      nil)))))

(defun notmuch-hello-reflect (list ncols)
  "Reflect a `ncols' wide matrix represented by `list' along the
diagonal."
  ;; Not very lispy...
  (let ((nrows (ceiling (length list) ncols)))
    (loop for row from 0 to (- nrows 1)
	  append (notmuch-hello-reflect-generate-row ncols nrows row list))))

(defun notmuch-hello-widget-search (widget &rest ignore)
  (if (widget-get widget :notmuch-search-type)
      (notmuch-tree (widget-get widget
				:notmuch-search-terms))
    (notmuch-search (widget-get widget
				:notmuch-search-terms)
		    (widget-get widget
				:notmuch-search-oldest-first))))

(defun notmuch-saved-search-count (search)
  (car (process-lines notmuch-command "count" search)))

(defun notmuch-hello-tags-per-line (widest)
  "Determine how many tags to show per line and how wide they
should be. Returns a cons cell `(tags-per-line width)'."
  (let ((tags-per-line
	 (cond
	  ((integerp notmuch-column-control)
	   (max 1
		(/ (- (window-width) notmuch-hello-indent)
		   ;; Count is 9 wide (8 digits plus space), 1 for the space
		   ;; after the name.
		   (+ 9 1 (max notmuch-column-control widest)))))

	  ((floatp notmuch-column-control)
	   (let* ((available-width (- (window-width) notmuch-hello-indent))
		  (proposed-width (max (* available-width notmuch-column-control) widest)))
	     (floor available-width proposed-width)))

	  (t
	   (max 1
		(/ (- (window-width) notmuch-hello-indent)
		   ;; Count is 9 wide (8 digits plus space), 1 for the space
		   ;; after the name.
		   (+ 9 1 widest)))))))

    (cons tags-per-line (/ (max 1
				(- (window-width) notmuch-hello-indent
				   ;; Count is 9 wide (8 digits plus
				   ;; space), 1 for the space after the
				   ;; name.
				   (* tags-per-line (+ 9 1))))
			   tags-per-line))))

(defun notmuch-hello-filtered-query (query filter)
  "Constructs a query to search all messages matching QUERY and FILTER.

If FILTER is a string, it is directly used in the returned query.

If FILTER is a function, it is called with QUERY as a parameter and
the string it returns is used as the query. If nil is returned,
the entry is hidden.

Otherwise, FILTER is ignored.
"
  (cond
   ((functionp filter) (funcall filter query))
   ((stringp filter)
    (concat "(" query ") and (" filter ")"))
   (t query)))

(defun notmuch-hello-query-counts (query-list &rest options)
  "Compute list of counts of matched messages from QUERY-LIST.

QUERY-LIST must be a list of saved-searches. Ideally each of
these is a plist but other options are available for backwards
compatibility: see `notmuch-saved-searches' for details.

The result is a list of plists each of which includes the
properties :name NAME, :query QUERY and :count COUNT, together
with any properties in the original saved-search.

The values :show-empty-searches, :filter and :filter-count from
options will be handled as specified for
`notmuch-hello-insert-searches'."
  (with-temp-buffer
    (dolist (elem query-list nil)
      (let ((count-query (or (notmuch-saved-search-get elem :count-query)
			     (notmuch-saved-search-get elem :query))))
	(insert
	 (replace-regexp-in-string
	  "\n" " "
	  (notmuch-hello-filtered-query count-query
					(or (plist-get options :filter-count)
					    (plist-get options :filter))))
	  "\n")))

    (unless (= (call-process-region (point-min) (point-max) notmuch-command
				    t t nil "count" "--batch") 0)
      (notmuch-logged-error "notmuch count --batch failed"
			    "Please check that the notmuch CLI is new enough to support `count
--batch'. In general we recommend running matching versions of
the CLI and emacs interface."))

    (goto-char (point-min))

    (notmuch-remove-if-not
     #'identity
     (mapcar
      (lambda (elem)
	(let* ((elem-plist (notmuch-hello-saved-search-to-plist elem))
	       (search-query (plist-get elem-plist :query))
	       (filtered-query (notmuch-hello-filtered-query
				search-query (plist-get options :filter)))
	       (message-count (prog1 (read (current-buffer))
				(forward-line 1))))
	  (when (and filtered-query (or (plist-get options :show-empty-searches) (> message-count 0)))
	    (setq elem-plist (plist-put elem-plist :query filtered-query))
	    (plist-put elem-plist :count message-count))))
      query-list))))

(defun notmuch-hello-insert-buttons (searches)
  "Insert buttons for SEARCHES.

SEARCHES must be a list of plists each of which should contain at
least the properties :name NAME :query QUERY and :count COUNT,
where QUERY is the query to start when the button for the
corresponding entry is activated, and COUNT should be the number
of messages matching the query.  Such a plist can be computed
with `notmuch-hello-query-counts'."
  (let* ((widest (notmuch-hello-longest-label searches))
	 (tags-and-width (notmuch-hello-tags-per-line widest))
	 (tags-per-line (car tags-and-width))
	 (column-width (cdr tags-and-width))
	 (column-indent 0)
	 (count 0)
	 (reordered-list (notmuch-hello-reflect searches tags-per-line))
	 ;; Hack the display of the buttons used.
	 (widget-push-button-prefix "")
	 (widget-push-button-suffix ""))
    ;; dme: It feels as though there should be a better way to
    ;; implement this loop than using an incrementing counter.
    (mapc (lambda (elem)
	    ;; (not elem) indicates an empty slot in the matrix.
	    (when elem
	      (if (> column-indent 0)
		  (widget-insert (make-string column-indent ? )))
	      (let* ((name (plist-get elem :name))
		     (query (plist-get elem :query))
		     (oldest-first (case (plist-get elem :sort-order)
				     (newest-first nil)
				     (oldest-first t)
				     (otherwise notmuch-search-oldest-first)))
		     (search-type (eq (plist-get elem :search-type) 'tree))
		     (msg-count (plist-get elem :count)))
		(widget-insert (format "%8s "
				       (notmuch-hello-nice-number msg-count)))
		(widget-create 'push-button
			       :notify #'notmuch-hello-widget-search
			       :notmuch-search-terms query
			       :notmuch-search-oldest-first oldest-first
			       :notmuch-search-type search-type
			       name)
		(setq column-indent
		      (1+ (max 0 (- column-width (length name)))))))
	    (setq count (1+ count))
	    (when (eq (% count tags-per-line) 0)
	      (setq column-indent 0)
	      (widget-insert "\n")))
	  reordered-list)

    ;; If the last line was not full (and hence did not include a
    ;; carriage return), insert one now.
    (unless (eq (% count tags-per-line) 0)
      (widget-insert "\n"))))

(defimage notmuch-hello-logo ((:type png :file "notmuch-logo.png")))

(defun notmuch-hello-update ()
  "Update the notmuch-hello buffer."
  ;; Lazy - rebuild everything.
  (interactive)
  (notmuch-hello t))

(defun notmuch-hello-window-configuration-change ()
  "Hook function to update the hello buffer when it is switched to."
  (let ((hello-buf (get-buffer "*notmuch-hello*"))
	(do-refresh nil))
    ;; Consider all windows in the currently selected frame, since
    ;; that's where the configuration change happened.  This also
    ;; refreshes our snapshot of all windows, so we have to do this
    ;; even if we know we won't refresh (e.g., hello-buf is null).
    (dolist (window (window-list))
      (let ((last-buf (window-parameter window 'notmuch-hello-last-buffer))
	    (cur-buf (window-buffer window)))
	(when (not (eq last-buf cur-buf))
	  ;; This window changed or is new.  Update recorded buffer
	  ;; for next time.
	  (set-window-parameter window 'notmuch-hello-last-buffer cur-buf)
	  (when (and (eq cur-buf hello-buf) last-buf)
	    ;; The user just switched to hello in this window (hello
	    ;; is currently visible, was not visible on the last
	    ;; configuration change, and this is not a new window)
	    (setq do-refresh t)))))
    (when (and do-refresh notmuch-hello-auto-refresh)
      ;; Refresh hello as soon as we get back to redisplay.  On Emacs
      ;; 24, we can't do it right here because something in this
      ;; hook's call stack overrides hello's point placement.
      (run-at-time nil nil #'notmuch-hello t))
    (when (null hello-buf)
      ;; Clean up hook
      (remove-hook 'window-configuration-change-hook
		   #'notmuch-hello-window-configuration-change))))

;; the following variable is defined as being defconst in notmuch-version.el
(defvar notmuch-emacs-version)

(defun notmuch-hello-versions ()
  "Display the notmuch version(s)"
  (interactive)
  (let ((notmuch-cli-version (notmuch-cli-version)))
    (message "notmuch version %s"
	     (if (string= notmuch-emacs-version notmuch-cli-version)
		 notmuch-cli-version
	       (concat notmuch-cli-version
		       " (emacs mua version " notmuch-emacs-version ")")))))

(defvar notmuch-hello-mode-map
  (let ((map (if (fboundp 'make-composed-keymap)
		 ;; Inherit both widget-keymap and
		 ;; notmuch-common-keymap. We have to use
		 ;; make-sparse-keymap to force this to be a new
		 ;; keymap (so that when we modify map it does not
		 ;; modify widget-keymap).
		 (make-composed-keymap (list (make-sparse-keymap) widget-keymap))
	       ;; Before Emacs 24, keymaps didn't support multiple
	       ;; inheritance,, so just copy the widget keymap since
	       ;; it's unlikely to change.
	       (copy-keymap widget-keymap))))
    (set-keymap-parent map notmuch-common-keymap)
    (define-key map "v" 'notmuch-hello-versions)
    (define-key map (kbd "<C-tab>") 'widget-backward)
    map)
  "Keymap for \"notmuch hello\" buffers.")
(fset 'notmuch-hello-mode-map notmuch-hello-mode-map)

(define-derived-mode notmuch-hello-mode fundamental-mode "notmuch-hello"
 "Major mode for convenient notmuch navigation. This is your entry portal into notmuch.

Saved searches are \"bookmarks\" for arbitrary queries. Hit RET
or click on a saved search to view matching threads. Edit saved
searches with the `edit' button. Type `\\[notmuch-jump-search]'
in any Notmuch screen for quick access to saved searches that
have shortcut keys.

Type new searches in the search box and hit RET to view matching
threads. Hit RET in a recent search box to re-submit a previous
search. Edit it first if you like. Save a recent search to saved
searches with the `save' button.

Hit `\\[notmuch-search]' or `\\[notmuch-tree]' in any Notmuch
screen to search for messages and view matching threads or
messages, respectively. Recent searches are available in the
minibuffer history.

Expand the all tags view with the `show' button (and collapse
again with the `hide' button). Hit RET or click on a tag name to
view matching threads.

Hit `\\[notmuch-refresh-this-buffer]' to refresh the screen and
`\\[notmuch-bury-or-kill-this-buffer]' to quit.

The screen may be customized via `\\[customize]'.

Complete list of currently available key bindings:

\\{notmuch-hello-mode-map}"
 (setq notmuch-buffer-refresh-function #'notmuch-hello-update)
 ;;(setq buffer-read-only t)
)

(defun notmuch-hello-generate-tag-alist (&optional hide-tags)
  "Return an alist from tags to queries to display in the all-tags section."
  (mapcar (lambda (tag)
	    (cons tag (concat "tag:" (notmuch-escape-boolean-term tag))))
	  (notmuch-remove-if-not
	   (lambda (tag)
	     (not (member tag hide-tags)))
	   (process-lines notmuch-command "search" "--output=tags" "*"))))

(defun notmuch-hello-insert-header ()
  "Insert the default notmuch-hello header."
  (when notmuch-show-logo
    (let ((image notmuch-hello-logo))
      ;; The notmuch logo uses transparency. That can display poorly
      ;; when inserting the image into an emacs buffer (black logo on
      ;; a black background), so force the background colour of the
      ;; image. We use a face to represent the colour so that
      ;; `defface' can be used to declare the different possible
      ;; colours, which depend on whether the frame has a light or
      ;; dark background.
      (setq image (cons 'image
			(append (cdr image)
				(list :background (face-background 'notmuch-hello-logo-background)))))
      (insert-image image))
    (widget-insert "  "))

  (widget-insert "Welcome to ")
  ;; Hack the display of the links used.
  (let ((widget-link-prefix "")
	(widget-link-suffix ""))
    (widget-create 'link
		   :notify (lambda (&rest ignore)
			     (browse-url notmuch-hello-url))
		   :help-echo "Visit the notmuch website."
		   "notmuch")
    (widget-insert ". ")
    (widget-insert "You have ")
    (widget-create 'link
		   :notify (lambda (&rest ignore)
			     (notmuch-hello-update))
		   :help-echo "Refresh"
		   (notmuch-hello-nice-number
		    (string-to-number (car (process-lines notmuch-command "count")))))
    (widget-insert " messages.\n")))


(defun notmuch-hello-insert-saved-searches ()
  "Insert the saved-searches section."
  (let ((searches (notmuch-hello-query-counts
		   (if notmuch-saved-search-sort-function
		       (funcall notmuch-saved-search-sort-function
				notmuch-saved-searches)
		     notmuch-saved-searches)
		   :show-empty-searches notmuch-show-empty-saved-searches)))
    (when searches
      (widget-insert "Saved searches: ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (customize-variable 'notmuch-saved-searches))
		     "edit")
      (widget-insert "\n\n")
      (let ((start (point)))
	(notmuch-hello-insert-buttons searches)
	(indent-rigidly start (point) notmuch-hello-indent)))))

(defun notmuch-hello-insert-search ()
  "Insert a search widget."
  (widget-insert "Search: ")
  (widget-create 'editable-field
		 ;; Leave some space at the start and end of the
		 ;; search boxes.
		 :size (max 8 (- (window-width) notmuch-hello-indent
				 (length "Search: ")))
		 :action (lambda (widget &rest ignore)
			   (notmuch-hello-search (widget-value widget))))
  ;; Add an invisible dot to make `widget-end-of-line' ignore
  ;; trailing spaces in the search widget field.  A dot is used
  ;; instead of a space to make `show-trailing-whitespace'
  ;; happy, i.e. avoid it marking the whole line as trailing
  ;; spaces.
  (widget-insert ".")
  (put-text-property (1- (point)) (point) 'invisible t)
  (widget-insert "\n"))

(defun notmuch-hello-insert-recent-searches ()
  "Insert recent searches."
  (when notmuch-search-history
    (widget-insert "Recent searches: ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (when (y-or-n-p "Are you sure you want to clear the searches? ")
			       (setq notmuch-search-history nil)
			       (notmuch-hello-update)))
		   "clear")
    (widget-insert "\n\n")
    (let ((start (point)))
      (loop for i from 1 to notmuch-hello-recent-searches-max
	    for search in notmuch-search-history do
	    (let ((widget-symbol (intern (format "notmuch-hello-search-%d" i))))
	      (set widget-symbol
		   (widget-create 'editable-field
				  ;; Don't let the search boxes be
				  ;; less than 8 characters wide.
				  :size (max 8
					     (- (window-width)
						;; Leave some space
						;; at the start and
						;; end of the
						;; boxes.
						(* 2 notmuch-hello-indent)
						;; 1 for the space
						;; before the
						;; `[save]' button. 6
						;; for the `[save]'
						;; button.
						1 6
						;; 1 for the space
						;; before the `[del]'
						;; button. 5 for the
						;; `[del]' button.
						1 5))
				  :action (lambda (widget &rest ignore)
					    (notmuch-hello-search (widget-value widget)))
				  search))
	      (widget-insert " ")
	      (widget-create 'push-button
			     :notify (lambda (widget &rest ignore)
				       (notmuch-hello-add-saved-search widget))
			     :notmuch-saved-search-widget widget-symbol
			     "save")
	      (widget-insert " ")
	      (widget-create 'push-button
			     :notify (lambda (widget &rest ignore)
				       (when (y-or-n-p "Are you sure you want to delete this search? ")
					 (notmuch-hello-delete-search-from-history widget)))
			     :notmuch-saved-search-widget widget-symbol
			     "del"))
	    (widget-insert "\n"))
      (indent-rigidly start (point) notmuch-hello-indent))
    nil))

(defun notmuch-hello-insert-searches (title query-list &rest options)
  "Insert a section with TITLE showing a list of buttons made from QUERY-LIST.

QUERY-LIST should ideally be a plist but for backwards
compatibility other forms are also accepted (see
`notmuch-saved-searches' for details).  The plist should
contain keys :name and :query; if :count-query is also present
then it specifies an alternate query to be used to generate the
count for the associated search.

Supports the following entries in OPTIONS as a plist:
:initially-hidden - if non-nil, section will be hidden on startup
:show-empty-searches - show buttons with no matching messages
:hide-if-empty - hide if no buttons would be shown
   (only makes sense without :show-empty-searches)
:filter - This can be a function that takes the search query as its argument and
   returns a filter to be used in conjunction with the query for that search or nil
   to hide the element. This can also be a string that is used as a combined with
   each query using \"and\".
:filter-count - Separate filter to generate the count displayed each search. Accepts
   the same values as :filter. If :filter and :filter-count are specified, this
   will be used instead of :filter, not in conjunction with it."
  (widget-insert title ": ")
  (if (and notmuch-hello-first-run (plist-get options :initially-hidden))
      (add-to-list 'notmuch-hello-hidden-sections title))
  (let ((is-hidden (member title notmuch-hello-hidden-sections))
	(start (point)))
    (if is-hidden
	(widget-create 'push-button
		       :notify `(lambda (widget &rest ignore)
				  (setq notmuch-hello-hidden-sections
					(delete ,title notmuch-hello-hidden-sections))
				  (notmuch-hello-update))
		       "show")
      (widget-create 'push-button
		     :notify `(lambda (widget &rest ignore)
				(add-to-list 'notmuch-hello-hidden-sections
					     ,title)
				(notmuch-hello-update))
		     "hide"))
    (widget-insert "\n")
    (when (not is-hidden)
      (let ((searches (apply 'notmuch-hello-query-counts query-list options)))
	(when (or (not (plist-get options :hide-if-empty))
		  searches)
	  (widget-insert "\n")
	  (notmuch-hello-insert-buttons searches)
	  (indent-rigidly start (point) notmuch-hello-indent))))))

(defun notmuch-hello-insert-tags-section (&optional title &rest options)
  "Insert a section displaying all tags with message counts.

TITLE defaults to \"All tags\".
Allowed options are those accepted by `notmuch-hello-insert-searches' and the
following:

:hide-tags - List of tags that should be excluded."
  (apply 'notmuch-hello-insert-searches
	 (or title "All tags")
	 (notmuch-hello-generate-tag-alist (plist-get options :hide-tags))
	 options))

(defun notmuch-hello-insert-inbox ()
  "Show an entry for each saved search and inboxed messages for each tag"
  (notmuch-hello-insert-searches "What's in your inbox"
				 (append
				  notmuch-saved-searches
				  (notmuch-hello-generate-tag-alist))
				 :filter "tag:inbox"))

(defun notmuch-hello-insert-alltags ()
  "Insert a section displaying all tags and associated message counts"
  (notmuch-hello-insert-tags-section
   nil
   :initially-hidden (not notmuch-show-all-tags-list)
   :hide-tags notmuch-hello-hide-tags
   :filter notmuch-hello-tag-list-make-query))

(defun notmuch-hello-insert-footer ()
  "Insert the notmuch-hello footer."
  (let ((start (point)))
    (widget-insert "Hit `?' for context-sensitive help in any Notmuch screen.\n")
    (widget-insert "Customize ")
    (widget-create 'link
		   :notify (lambda (&rest ignore)
			     (customize-group 'notmuch))
		   :button-prefix "" :button-suffix ""
		   "Notmuch")
    (widget-insert " or ")
    (widget-create 'link
		   :notify (lambda (&rest ignore)
			     (customize-variable 'notmuch-hello-sections))
		   :button-prefix "" :button-suffix ""
		   "this page.")
    (let ((fill-column (- (window-width) notmuch-hello-indent)))
      (center-region start (point)))))

;;;###autoload
(defun notmuch-hello (&optional no-display)
  "Run notmuch and display saved searches, known tags, etc."
  (interactive)

  (notmuch-assert-cli-sane)
  ;; This may cause a window configuration change, so if the
  ;; auto-refresh hook is already installed, avoid recursive refresh.
  (let ((notmuch-hello-auto-refresh nil))
    (if no-display
	(set-buffer "*notmuch-hello*")
      (switch-to-buffer "*notmuch-hello*")))

  ;; Install auto-refresh hook
  (when notmuch-hello-auto-refresh
    (add-hook 'window-configuration-change-hook
	      #'notmuch-hello-window-configuration-change))

  (let ((target-line (line-number-at-pos))
	(target-column (current-column))
	(inhibit-read-only t))

    ;; Delete all editable widget fields.  Editable widget fields are
    ;; tracked in a buffer local variable `widget-field-list' (and
    ;; others).  If we do `erase-buffer' without properly deleting the
    ;; widgets, some widget-related functions are confused later.
    (mapc 'widget-delete widget-field-list)

    (erase-buffer)

    (unless (eq major-mode 'notmuch-hello-mode)
      (notmuch-hello-mode))

    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapc 'delete-overlay (car all))
      (mapc 'delete-overlay (cdr all)))

    (mapc
     (lambda (section)
       (let ((point-before (point)))
	 (if (functionp section)
	     (funcall section)
	   (apply (car section) (cdr section)))
	 ;; don't insert a newline when the previous section didn't
	 ;; show anything.
	 (unless (eq (point) point-before)
	   (widget-insert "\n"))))
     notmuch-hello-sections)
    (widget-setup)

    ;; Move point back to where it was before refresh. Use line and
    ;; column instead of point directly to be insensitive to additions
    ;; and removals of text within earlier lines.
    (goto-char (point-min))
    (forward-line (1- target-line))
    (move-to-column target-column))
  (run-hooks 'notmuch-hello-refresh-hook)
  (setq notmuch-hello-first-run nil))

(defun notmuch-folder ()
  "Deprecated function for invoking notmuch---calling `notmuch' is preferred now."
  (interactive)
  (notmuch-hello))

;;

(provide 'notmuch-hello)

;;; notmuch-hello.el ends here

;;; notmuch-tag.el --- tag messages within emacs
;;
;; Copyright © Damien Cassou
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
;;          Damien Cassou <damien.cassou@gmail.com>
;;
;;; Code:
;;

(require 'cl)
(require 'crm)
(require 'notmuch-lib)

(declare-function notmuch-search-tag "notmuch" tag-changes)
(declare-function notmuch-show-tag "notmuch-show" tag-changes)
(declare-function notmuch-tree-tag "notmuch-tree" tag-changes)

(autoload 'notmuch-jump "notmuch-jump")

(define-widget 'notmuch-tag-key-type 'list
  "A single key tagging binding."
  :format "%v"
  :args '((list :inline t
		:format "%v"
		(key-sequence :tag "Key")
		(radio :tag "Tag operations" (repeat :tag "Tag list" (string :format "%v" :tag "change"))
		       (variable :tag "Tag variable"))
		(string :tag "Name"))))

(defcustom notmuch-tagging-keys
  `((,(kbd "a") notmuch-archive-tags "Archive")
    (,(kbd "u") notmuch-show-mark-read-tags "Mark read")
    (,(kbd "f") ("+flagged") "Flag")
    (,(kbd "s") ("+spam" "-inbox") "Mark as spam")
    (,(kbd "d") ("+deleted" "-inbox") "Delete"))
  "A list of keys and corresponding tagging operations.

For each key (or key sequence) you can specify a sequence of
tagging operations to apply, or a variable which contains a list
of tagging operations such as `notmuch-archive-tags'. The final
element is a name for this tagging operation. If the name is
omitted or empty then the list of tag changes, or the variable
name is used as the name.

The key `notmuch-tag-jump-reverse-key' (k by default) should not
be used (either as a key, or as the start of a key sequence) as
it is already bound: it switches the menu to a menu of the
reverse tagging operations. The reverse of a tagging operation is
the same list of individual tag-ops but with `+tag` replaced by
`-tag` and vice versa.

If setting this variable outside of customize then it should be a
list of triples (lists of three elements). Each triple should be
of the form (key-binding tagging-operations name). KEY-BINDING
can be a single character or a key sequence; TAGGING-OPERATIONS
should either be a list of individual tag operations each of the
form `+tag` or `-tag`, or the variable name of a variable that is
a list of tagging operations; NAME should be a name for the
tagging operation, if omitted or empty than then name is taken
from TAGGING-OPERATIONS."
  :tag "List of tagging bindings"
  :type '(repeat notmuch-tag-key-type)
  :group 'notmuch-tag)

(define-widget 'notmuch-tag-format-type 'lazy
  "Customize widget for notmuch-tag-format and friends"
  :type '(alist :key-type (regexp :tag "Tag")
		:extra-offset -3
		:value-type
		(radio :format "%v"
		       (const :tag "Hidden" nil)
		       (set :tag "Modified"
			    (string :tag "Display as")
			    (list :tag "Face" :extra-offset -4
				  (const :format "" :inline t
					 (notmuch-apply-face tag))
				  (list :format "%v"
					(const :format "" quote)
					custom-face-edit))
			    (list :format "%v" :extra-offset -4
				  (const :format "" :inline t
					 (notmuch-tag-format-image-data tag))
				  (choice :tag "Image"
					  (const :tag "Star"
						 (notmuch-tag-star-icon))
					  (const :tag "Empty star"
						 (notmuch-tag-star-empty-icon))
					  (const :tag "Tag"
						 (notmuch-tag-tag-icon))
					  (string :tag "Custom")))
			    (sexp :tag "Custom")))))

(defface notmuch-tag-unread
  '((t :foreground "red"))
  "Default face used for the unread tag.

Used in the default value of `notmuch-tag-formats`."
  :group 'notmuch-faces)

(defface notmuch-tag-flagged
  '((((class color)
      (background dark))
     (:foreground "LightBlue1"))
    (((class color)
      (background light))
     (:foreground "blue")))
  "Face used for the flagged tag.

Used in the default value of `notmuch-tag-formats`."
  :group 'notmuch-faces)

(defcustom notmuch-tag-formats
  '(("unread" (propertize tag 'face 'notmuch-tag-unread))
    ("flagged" (propertize tag 'face 'notmuch-tag-flagged)
     (notmuch-tag-format-image-data tag (notmuch-tag-star-icon))))
  "Custom formats for individual tags.

This is an association list that maps from tag name regexps to
lists of formatting expressions.  The first entry whose car
regexp-matches a tag will be used to format that tag.  The regexp
is implicitly anchored, so to match a literal tag name, just use
that tag name (if it contains special regexp characters like
\".\" or \"*\", these have to be escaped).  The cdr of the
matching entry gives a list of Elisp expressions that modify the
tag.  If the list is empty, the tag will simply be hidden.
Otherwise, each expression will be evaluated in order: for the
first expression, the variable `tag' will be bound to the tag
name; for each later expression, the variable `tag' will be bound
to the result of the previous expression.  In this way, each
expression can build on the formatting performed by the previous
expression.  The result of the last expression will displayed in
place of the tag.

For example, to replace a tag with another string, simply use
that string as a formatting expression.  To change the foreground
of a tag to red, use the expression
  (propertize tag 'face '(:foreground \"red\"))

See also `notmuch-tag-format-image', which can help replace tags
with images."
  :group 'notmuch-search
  :group 'notmuch-show
  :group 'notmuch-faces
  :type 'notmuch-tag-format-type)

(defface notmuch-tag-deleted
  '((((class color) (supports :strike-through "red")) :strike-through "red")
    (t :inverse-video t))
  "Face used to display deleted tags.

Used in the default value of `notmuch-tag-deleted-formats`."
  :group 'notmuch-faces)

(defcustom notmuch-tag-deleted-formats
  '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
    (".*" (notmuch-apply-face tag `notmuch-tag-deleted)))
  "Custom formats for tags when deleted.

For deleted tags the formats in `notmuch-tag-formats` are applied
first and then these formats are applied on top; that is `tag'
passed to the function is the tag with all these previous
formattings applied. The formatted can access the original
unformatted tag as `bare-tag'.

By default this shows deleted tags with strike-through in red,
unless strike-through is not available (e.g., emacs is running in
a terminal) in which case it uses inverse video. To hide deleted
tags completely set this to
  '((\".*\" nil))

See `notmuch-tag-formats' for full documentation."
  :group 'notmuch-show
  :group 'notmuch-faces
  :type 'notmuch-tag-format-type)

(defface notmuch-tag-added
  '((t :underline "green"))
  "Default face used for added tags.

Used in the default value for `notmuch-tag-added-formats`."
  :group 'notmuch-faces)

(defcustom notmuch-tag-added-formats
  '((".*" (notmuch-apply-face tag 'notmuch-tag-added)))
  "Custom formats for tags when added.

For added tags the formats in `notmuch-tag-formats` are applied
first and then these formats are applied on top.

To disable special formatting of added tags, set this variable to
nil.

See `notmuch-tag-formats' for full documentation."
  :group 'notmuch-show
  :group 'notmuch-faces
  :type 'notmuch-tag-format-type)

(defun notmuch-tag-format-image-data (tag data)
  "Replace TAG with image DATA, if available.

This function returns a propertized string that will display image
DATA in place of TAG.This is designed for use in
`notmuch-tag-formats'.

DATA is the content of an SVG picture (e.g., as returned by
`notmuch-tag-star-icon')."
  (propertize tag 'display
	      `(image :type svg
		      :data ,data
		      :ascent center
		      :mask heuristic)))

(defun notmuch-tag-star-icon ()
  "Return SVG data representing a star icon.
This can be used with `notmuch-tag-format-image-data'."
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<svg version=\"1.1\" width=\"16\" height=\"16\">
  <g transform=\"translate(-242.81601,-315.59635)\">
    <path
       d=\"m 290.25762,334.31206 -17.64143,-11.77975 -19.70508,7.85447 5.75171,-20.41814 -13.55925,-16.31348 21.19618,-0.83936 11.325,-17.93675 7.34825,19.89939 20.55849,5.22795 -16.65471,13.13786 z\"
       transform=\"matrix(0.2484147,-0.02623394,0.02623394,0.2484147,174.63605,255.37691)\"
       style=\"fill:#ffff00;fill-rule:evenodd;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" />
  </g>
</svg>")

(defun notmuch-tag-star-empty-icon ()
  "Return SVG data representing an empty star icon.
This can be used with `notmuch-tag-format-image-data'."
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<svg version=\"1.1\" width=\"16\" height=\"16\">
  <g transform=\"translate(-242.81601,-315.59635)\">
    <path
       d=\"m 290.25762,334.31206 -17.64143,-11.77975 -19.70508,7.85447 5.75171,-20.41814 -13.55925,-16.31348 21.19618,-0.83936 11.325,-17.93675 7.34825,19.89939 20.55849,5.22795 -16.65471,13.13786 z\"
       transform=\"matrix(0.2484147,-0.02623394,0.02623394,0.2484147,174.63605,255.37691)\"
       style=\"fill:#d6d6d1;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" />
  </g>
</svg>")

(defun notmuch-tag-tag-icon ()
  "Return SVG data representing a tag icon.
This can be used with `notmuch-tag-format-image-data'."
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<svg version=\"1.1\" width=\"16\" height=\"16\">
  <g transform=\"translate(0,-1036.3622)\">
    <path
       d=\"m 0.44642857,1040.9336 12.50000043,0 2.700893,3.6161 -2.700893,3.616 -12.50000043,0 z\"
       style=\"fill:#ffff00;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.25;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-opacity:1\" />
  </g>
</svg>")

(defvar notmuch-tag--format-cache (make-hash-table :test 'equal)
  "Cache of tag format lookup.  Internal to `notmuch-tag-format-tag'.")

(defun notmuch-tag-clear-cache ()
  "Clear the internal cache of tag formats."
  (clrhash notmuch-tag--format-cache))

(defun notmuch-tag--get-formats (tag format-alist)
  "Find the first item whose car regexp-matches TAG."
  (save-match-data
    ;; Don't use assoc-default since there's no way to distinguish a
    ;; missing key from a present key with a null cdr.
    (assoc* tag format-alist
	    :test (lambda (tag key)
		    (and (eq (string-match key tag) 0)
			 (= (match-end 0) (length tag)))))))

(defun notmuch-tag--do-format (tag formatted-tag formats)
  "Apply a tag-formats entry to TAG."
  (cond ((null formats)		;; - Tag not in `formats',
	 formatted-tag)	       	;;   the format is the tag itself.
	((null (cdr formats))	;; - Tag was deliberately hidden,
	 nil)			;;   no format must be returned
	(t
	 ;; Tag was found and has formats, we must apply all the
	 ;; formats.  TAG may be null so treat that as a special case.
	 (let ((bare-tag tag)
	       (tag (copy-sequence (or formatted-tag ""))))
	   (dolist (format (cdr formats))
	     (setq tag (eval format)))
	   (if (and (null formatted-tag) (equal tag ""))
	       nil
	     tag)))))

(defun notmuch-tag-format-tag (tags orig-tags tag)
  "Format TAG according to `notmuch-tag-formats'.

TAGS and ORIG-TAGS are lists of the current tags and the original
tags; tags which have been deleted (i.e., are in ORIG-TAGS but
are not in TAGS) are shown using formats from
`notmuch-tag-deleted-formats'; tags which have been added (i.e.,
are in TAGS but are not in ORIG-TAGS) are shown using formats
from `notmuch-tag-added-formats' and tags which have not been
changed (the normal case) are shown using formats from
`notmuch-tag-formats'"
  (let* ((tag-state (cond ((not (member tag tags)) 'deleted)
			  ((not (member tag orig-tags)) 'added)))
	 (formatted-tag (gethash (cons tag tag-state) notmuch-tag--format-cache 'missing)))
    (when (eq formatted-tag 'missing)
      (let ((base (notmuch-tag--get-formats tag notmuch-tag-formats))
	    (over (case tag-state
		    (deleted (notmuch-tag--get-formats
			      tag notmuch-tag-deleted-formats))
		    (added (notmuch-tag--get-formats
			    tag notmuch-tag-added-formats))
		    (otherwise nil))))
	(setq formatted-tag (notmuch-tag--do-format tag tag base))
	(setq formatted-tag (notmuch-tag--do-format tag formatted-tag over))

	(puthash (cons tag tag-state) formatted-tag notmuch-tag--format-cache)))
    formatted-tag))

(defun notmuch-tag-format-tags (tags orig-tags &optional face)
  "Return a string representing formatted TAGS."
  (let ((face (or face 'notmuch-tag-face))
	(all-tags (sort (delete-dups (append tags orig-tags nil)) #'string<)))
    (notmuch-apply-face
     (mapconcat #'identity
		;; nil indicated that the tag was deliberately hidden
		(delq nil (mapcar
			   (apply-partially #'notmuch-tag-format-tag tags orig-tags)
			   all-tags))
		" ")
     face
     t)))

(defcustom notmuch-before-tag-hook nil
  "Hooks that are run before tags of a message are modified.

'tag-changes' will contain the tags that are about to be added or removed as
a list of strings of the form \"+TAG\" or \"-TAG\".
'query' will be a string containing the search query that determines
the messages that are about to be tagged"

  :type 'hook
  :options '(notmuch-hl-line-mode)
  :group 'notmuch-hooks)

(defcustom notmuch-after-tag-hook nil
  "Hooks that are run after tags of a message are modified.

'tag-changes' will contain the tags that were added or removed as
a list of strings of the form \"+TAG\" or \"-TAG\".
'query' will be a string containing the search query that determines
the messages that were tagged"
  :type 'hook
  :options '(notmuch-hl-line-mode)
  :group 'notmuch-hooks)

(defvar notmuch-select-tag-history nil
  "Variable to store minibuffer history for
`notmuch-select-tag-with-completion' function.")

(defvar notmuch-read-tag-changes-history nil
  "Variable to store minibuffer history for
`notmuch-read-tag-changes' function.")

(defun notmuch-tag-completions (&rest search-terms)
  "Return a list of tags for messages matching SEARCH-TERMS.

Returns all tags if no search terms are given."
  (if (null search-terms)
      (setq search-terms (list "*")))
  (split-string
   (with-output-to-string
     (with-current-buffer standard-output
       (apply 'call-process notmuch-command nil t
	      nil "search" "--output=tags" "--exclude=false" search-terms)))
   "\n+" t))

(defun notmuch-select-tag-with-completion (prompt &rest search-terms)
  (let ((tag-list (apply #'notmuch-tag-completions search-terms)))
    (completing-read prompt tag-list nil nil nil 'notmuch-select-tag-history)))

(defun notmuch-read-tag-changes (current-tags &optional prompt initial-input)
  "Prompt for tag changes in the minibuffer.

CURRENT-TAGS is a list of tags that are present on the message or
messages to be changed.  These are offered as tag removal
completions.  CURRENT-TAGS may contain duplicates.  PROMPT, if
non-nil, is the query string to present in the minibuffer.  It
defaults to \"Tags\".  INITIAL-INPUT, if non-nil, will be the
initial input in the minibuffer."

  (let* ((all-tag-list (notmuch-tag-completions))
	 (add-tag-list (mapcar (apply-partially 'concat "+") all-tag-list))
	 (remove-tag-list (mapcar (apply-partially 'concat "-") current-tags))
	 (tag-list (append add-tag-list remove-tag-list))
	 (prompt (concat (or prompt "Tags") " (+add -drop): "))
	 (crm-separator " ")
	 ;; By default, space is bound to "complete word" function.
	 ;; Re-bind it to insert a space instead.  Note that <tab>
	 ;; still does the completion.
	 (crm-local-completion-map
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-parent map crm-local-completion-map)
	    (define-key map " " 'self-insert-command)
	    map)))
    (delete "" (completing-read-multiple
		prompt
		;; Append the separator to each completion so when the
		;; user completes a tag they can immediately begin
		;; entering another.  `completing-read-multiple'
		;; ultimately splits the input on crm-separator, so we
		;; don't need to strip this back off (we just need to
		;; delete "empty" entries caused by trailing spaces).
		(mapcar (lambda (tag-op) (concat tag-op crm-separator)) tag-list)
		nil nil initial-input
		'notmuch-read-tag-changes-history))))

(defun notmuch-update-tags (tags tag-changes)
  "Return a copy of TAGS with additions and removals from TAG-CHANGES.

TAG-CHANGES must be a list of tags names, each prefixed with
either a \"+\" to indicate the tag should be added to TAGS if not
present or a \"-\" to indicate that the tag should be removed
from TAGS if present."
  (let ((result-tags (copy-sequence tags)))
    (dolist (tag-change tag-changes)
      (let ((op (string-to-char tag-change))
	    (tag (unless (string= tag-change "") (substring tag-change 1))))
	(case op
	  (?+ (unless (member tag result-tags)
		(push tag result-tags)))
	  (?- (setq result-tags (delete tag result-tags)))
	  (otherwise
	   (error "Changed tag must be of the form `+this_tag' or `-that_tag'")))))
    (sort result-tags 'string<)))

(defconst notmuch-tag-argument-limit 1000
  "Use batch tagging if the tagging query is longer than this.

This limits the length of arguments passed to the notmuch CLI to
avoid system argument length limits and performance problems.")

(defun notmuch-tag (query tag-changes)
  "Add/remove tags in TAG-CHANGES to messages matching QUERY.

QUERY should be a string containing the search-terms.
TAG-CHANGES is a list of strings of the form \"+tag\" or
\"-tag\" to add or remove tags, respectively.

Note: Other code should always use this function to alter tags of
messages instead of running (notmuch-call-notmuch-process \"tag\" ..)
directly, so that hooks specified in notmuch-before-tag-hook and
notmuch-after-tag-hook will be run."
  ;; Perform some validation
  (mapc (lambda (tag-change)
	  (unless (string-match-p "^[-+]\\S-+$" tag-change)
	    (error "Tag must be of the form `+this_tag' or `-that_tag'")))
	tag-changes)
  (unless query
    (error "Nothing to tag!"))
  (unless (null tag-changes)
    (run-hooks 'notmuch-before-tag-hook)
    (if (<= (length query) notmuch-tag-argument-limit)
	(apply 'notmuch-call-notmuch-process "tag"
	       (append tag-changes (list "--" query)))
      ;; Use batch tag mode to avoid argument length limitations
      (let ((batch-op (concat (mapconcat #'notmuch-hex-encode tag-changes " ")
			      " -- " query)))
	(notmuch-call-notmuch-process :stdin-string batch-op "tag" "--batch")))
    (run-hooks 'notmuch-after-tag-hook)))

(defun notmuch-tag-change-list (tags &optional reverse)
  "Convert TAGS into a list of tag changes.

Add a \"+\" prefix to any tag in TAGS list that doesn't already
begin with a \"+\" or a \"-\". If REVERSE is non-nil, replace all
\"+\" prefixes with \"-\" and vice versa in the result."
  (mapcar (lambda (str)
	    (let ((s (if (string-match "^[+-]" str) str (concat "+" str))))
	      (if reverse
		  (concat (if (= (string-to-char s) ?-) "+" "-")
			  (substring s 1))
		s)))
	  tags))

(defvar notmuch-tag-jump-reverse-key "k"
  "The key in tag-jump to switch to the reverse tag changes.")

(defun notmuch-tag-jump (reverse)
  "Create a jump menu for tagging operations.

Creates and displays a jump menu for the tagging operations
specified in `notmuch-tagging-keys'. If REVERSE is set then it
offers a menu of the reverses of the operations specified in
`notmuch-tagging-keys'; i.e. each `+tag` is replaced by `-tag`
and vice versa."
  ;; In principle this function is simple, but it has to deal with
  ;; lots of cases: different modes (search/show/tree), whether a name
  ;; is specified, whether the tagging operations is a list of
  ;; tag-ops, or a symbol that evaluates to such a list, and whether
  ;; REVERSE is specified.
  (interactive "P")
  (let (action-map)
    (dolist (binding notmuch-tagging-keys)
      (let* ((tag-function (case major-mode
			     (notmuch-search-mode #'notmuch-search-tag)
			     (notmuch-show-mode #'notmuch-show-tag)
			     (notmuch-tree-mode #'notmuch-tree-tag)))
	     (key (first binding))
	     (forward-tag-change (if (symbolp (second binding))
				     (symbol-value (second binding))
				   (second binding)))
	     (tag-change (if reverse
			     (notmuch-tag-change-list forward-tag-change 't)
			   forward-tag-change))
	     (name (or (and (not (string= (third binding) ""))
			    (third binding))
		       (and (symbolp (second binding))
			    (symbol-name (second binding)))))
	     (name-string (if name
			      (if reverse (concat "Reverse " name)
				name)
			    (mapconcat #'identity tag-change " "))))
	(push (list key name-string
		     `(lambda () (,tag-function ',tag-change)))
	      action-map)))
    (push (list notmuch-tag-jump-reverse-key
		(if reverse
		    "Forward tag changes "
		  "Reverse tag changes")
		(apply-partially 'notmuch-tag-jump (not reverse)))
	  action-map)
    (setq action-map (nreverse action-map))
    (notmuch-jump action-map "Tag: ")))

;;

(provide 'notmuch-tag)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; notmuch.el --- run notmuch within emacs
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
;; Homepage: https://notmuchmail.org/

;;; Commentary:

;; This is an emacs-based interface to the notmuch mail system.
;;
;; You will first need to have the notmuch program installed and have a
;; notmuch database built in order to use this. See
;; https://notmuchmail.org for details.
;;
;; To install this software, copy it to a directory that is on the
;; `load-path' variable within emacs (a good candidate is
;; /usr/local/share/emacs/site-lisp). If you are viewing this from the
;; notmuch source distribution then you can simply run:
;;
;;	sudo make install-emacs
;;
;; to install it.
;;
;; Then, to actually run it, add:
;;
;;	(autoload 'notmuch "notmuch" "Notmuch mail" t)
;;
;; to your ~/.emacs file, and then run "M-x notmuch" from within emacs,
;; or run:
;;
;;	emacs -f notmuch
;;
;; Have fun, and let us know if you have any comment, questions, or
;; kudos: Notmuch list <notmuch@notmuchmail.org> (subscription is not
;; required, but is available from https://notmuchmail.org).
;;
;; Note for MELPA users (and others tracking the development version
;; of notmuch-emacs):
;;
;; This emacs package needs a fairly closely matched version of the
;; notmuch program. If you use the MELPA version of notmuch.el (as
;; opposed to MELPA stable), you should be prepared to track the
;; master development branch (i.e. build from git) for the notmuch
;; program as well. Upgrading notmuch-emacs too far beyond the notmuch
;; program can CAUSE YOUR EMAIL TO STOP WORKING.
;;
;; TL;DR: notmuch-emacs from MELPA and notmuch from distro packages is
;; NOT SUPPORTED.
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'mm-view)
(require 'message)

(require 'notmuch-lib)
(require 'notmuch-tag)
(require 'notmuch-show)
(require 'notmuch-tree)
(require 'notmuch-mua)
(require 'notmuch-hello)
(require 'notmuch-maildir-fcc)
(require 'notmuch-message)
(require 'notmuch-parser)

(defcustom notmuch-search-result-format
  `(("date" . "%12s ")
    ("count" . "%-7s ")
    ("authors" . "%-20s ")
    ("subject" . "%s ")
    ("tags" . "(%s)"))
  "Search result formatting. Supported fields are:
	date, count, authors, subject, tags
For example:
	(setq notmuch-search-result-format \(\(\"authors\" . \"%-40s\"\)
					     \(\"subject\" . \"%s\"\)\)\)
Line breaks are permitted in format strings (though this is
currently experimental).  Note that a line break at the end of an
\"authors\" field will get elided if the authors list is long;
place it instead at the beginning of the following field.  To
enter a line break when setting this variable with setq, use \\n.
To enter a line break in customize, press \\[quoted-insert] C-j."
  :type '(alist :key-type (string) :value-type (string))
  :group 'notmuch-search)

;; The name of this variable `notmuch-init-file' is consistent with the
;; convention used in e.g. emacs and gnus. The value, `notmuch-config[.el[c]]'
;; is consistent with notmuch cli configuration file `~/.notmuch-config'.
(defcustom notmuch-init-file (locate-user-emacs-file "notmuch-config")
  "Your Notmuch Emacs-Lisp configuration file name.
If a file with one of the suffixes defined by `get-load-suffixes' exists,
it will be read instead.
This file is read once when notmuch is loaded; the notmuch hooks added
there will be called at other points of notmuch execution."
  :type 'file
  :group 'notmuch)

(defvar notmuch-query-history nil
  "Variable to store minibuffer history for notmuch queries")

(defun notmuch-foreach-mime-part (function mm-handle)
  (cond ((stringp (car mm-handle))
         (dolist (part (cdr mm-handle))
           (notmuch-foreach-mime-part function part)))
        ((bufferp (car mm-handle))
         (funcall function mm-handle))
        (t (dolist (part mm-handle)
             (notmuch-foreach-mime-part function part)))))

(defun notmuch-count-attachments (mm-handle)
  (let ((count 0))
    (notmuch-foreach-mime-part
     (lambda (p)
       (let ((disposition (mm-handle-disposition p)))
         (and (listp disposition)
              (or (equal (car disposition) "attachment")
                  (and (equal (car disposition) "inline")
                       (assq 'filename disposition)))
              (incf count))))
     mm-handle)
    count))

(defun notmuch-save-attachments (mm-handle &optional queryp)
  (notmuch-foreach-mime-part
   (lambda (p)
     (let ((disposition (mm-handle-disposition p)))
       (and (listp disposition)
            (or (equal (car disposition) "attachment")
                (and (equal (car disposition) "inline")
                     (assq 'filename disposition)))
            (or (not queryp)
                (y-or-n-p
                 (concat "Save '" (cdr (assq 'filename disposition)) "' ")))
            (mm-save-part p))))
   mm-handle))

(require 'hl-line)

(defun notmuch-hl-line-mode ()
  (prog1 (hl-line-mode)
    (when hl-line-overlay
      (overlay-put hl-line-overlay 'priority 1))))

(defcustom notmuch-search-hook '(notmuch-hl-line-mode)
  "List of functions to call when notmuch displays the search results."
  :type 'hook
  :options '(notmuch-hl-line-mode)
  :group 'notmuch-search
  :group 'notmuch-hooks)

(defvar notmuch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map notmuch-common-keymap)
    (define-key map "x" 'notmuch-bury-or-kill-this-buffer)
    (define-key map (kbd "<DEL>") 'notmuch-search-scroll-down)
    (define-key map "b" 'notmuch-search-scroll-down)
    (define-key map " " 'notmuch-search-scroll-up)
    (define-key map "<" 'notmuch-search-first-thread)
    (define-key map ">" 'notmuch-search-last-thread)
    (define-key map "p" 'notmuch-search-previous-thread)
    (define-key map "n" 'notmuch-search-next-thread)
    (define-key map "r" 'notmuch-search-reply-to-thread-sender)
    (define-key map "R" 'notmuch-search-reply-to-thread)
    (define-key map "o" 'notmuch-search-toggle-order)
    (define-key map "c" 'notmuch-search-stash-map)
    (define-key map "t" 'notmuch-search-filter-by-tag)
    (define-key map "l" 'notmuch-search-filter)
    (define-key map [mouse-1] 'notmuch-search-show-thread)
    (define-key map "k" 'notmuch-tag-jump)
    (define-key map "*" 'notmuch-search-tag-all)
    (define-key map "a" 'notmuch-search-archive-thread)
    (define-key map "-" 'notmuch-search-remove-tag)
    (define-key map "+" 'notmuch-search-add-tag)
    (define-key map (kbd "RET") 'notmuch-search-show-thread)
    (define-key map "Z" 'notmuch-tree-from-search-current-query)
    map)
  "Keymap for \"notmuch search\" buffers.")
(fset 'notmuch-search-mode-map notmuch-search-mode-map)

(defvar notmuch-search-stash-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'notmuch-search-stash-thread-id)
    (define-key map "q" 'notmuch-stash-query)
    (define-key map "?" 'notmuch-subkeymap-help)
    map)
  "Submap for stash commands")
(fset 'notmuch-search-stash-map notmuch-search-stash-map)

(defun notmuch-search-stash-thread-id ()
  "Copy thread ID of current thread to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-search-find-thread-id)))

(defun notmuch-stash-query ()
  "Copy current query to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-search-get-query)))

(defvar notmuch-search-query-string)
(defvar notmuch-search-target-thread)
(defvar notmuch-search-target-line)

(defvar notmuch-search-disjunctive-regexp      "\\<[oO][rR]\\>")

(defun notmuch-search-scroll-up ()
  "Move forward through search results by one window's worth."
  (interactive)
  (condition-case nil
      (scroll-up nil)
    ((end-of-buffer) (notmuch-search-last-thread))))

(defun notmuch-search-scroll-down ()
  "Move backward through the search results by one window's worth."
  (interactive)
  ;; I don't know why scroll-down doesn't signal beginning-of-buffer
  ;; the way that scroll-up signals end-of-buffer, but c'est la vie.
  ;;
  ;; So instead of trapping a signal we instead check whether the
  ;; window begins on the first line of the buffer and if so, move
  ;; directly to that position. (We have to count lines since the
  ;; window-start position is not the same as point-min due to the
  ;; invisible thread-ID characters on the first line.
  (if (equal (count-lines (point-min) (window-start)) 0)
      (goto-char (point-min))
    (scroll-down nil)))

(defun notmuch-search-next-thread ()
  "Select the next thread in the search results."
  (interactive)
  (when (notmuch-search-get-result)
    (goto-char (notmuch-search-result-end))))

(defun notmuch-search-previous-thread ()
  "Select the previous thread in the search results."
  (interactive)
  (if (notmuch-search-get-result)
      (unless (bobp)
	(goto-char (notmuch-search-result-beginning (- (point) 1))))
    ;; We must be past the end; jump to the last result
    (notmuch-search-last-thread)))

(defun notmuch-search-last-thread ()
  "Select the last thread in the search results."
  (interactive)
  (goto-char (point-max))
  (forward-line -2)
  (let ((beg (notmuch-search-result-beginning)))
    (when beg (goto-char beg))))

(defun notmuch-search-first-thread ()
  "Select the first thread in the search results."
  (interactive)
  (goto-char (point-min)))

(defface notmuch-message-summary-face
 '((((class color) (background light)) (:background "#f0f0f0"))
   (((class color) (background dark)) (:background "#303030")))
 "Face for the single-line message summary in notmuch-show-mode."
 :group 'notmuch-show
 :group 'notmuch-faces)

(defface notmuch-search-date
  '((t :inherit default))
  "Face used in search mode for dates."
  :group 'notmuch-search
  :group 'notmuch-faces)

(defface notmuch-search-count
  '((t :inherit default))
  "Face used in search mode for the count matching the query."
  :group 'notmuch-search
  :group 'notmuch-faces)

(defface notmuch-search-subject
  '((t :inherit default))
  "Face used in search mode for subjects."
  :group 'notmuch-search
  :group 'notmuch-faces)

(defface notmuch-search-matching-authors
  '((t :inherit default))
  "Face used in search mode for authors matching the query."
  :group 'notmuch-search
  :group 'notmuch-faces)

(defface notmuch-search-non-matching-authors
  '((((class color)
      (background dark))
     (:foreground "grey30"))
    (((class color)
      (background light))
     (:foreground "grey60"))
    (t
     (:italic t)))
  "Face used in search mode for authors not matching the query."
  :group 'notmuch-search
  :group 'notmuch-faces)

(defface notmuch-tag-face
  '((((class color)
      (background dark))
     (:foreground "OliveDrab1"))
    (((class color)
      (background light))
     (:foreground "navy blue" :bold t))
    (t
     (:bold t)))
  "Face used in search mode face for tags."
  :group 'notmuch-search
  :group 'notmuch-faces)

(defface notmuch-search-flagged-face
  '((((class color)
      (background dark))
     (:foreground "LightBlue1"))
    (((class color)
      (background light))
     (:foreground "blue")))
  "Face used in search mode face for flagged threads.

This face is the default value for the \"flagged\" tag in
`notmuch-search-line-faces`."
  :group 'notmuch-search
  :group 'notmuch-faces)

(defface notmuch-search-unread-face
  '((t
     (:weight bold)))
  "Face used in search mode for unread threads.

This face is the default value for the \"unread\" tag in
`notmuch-search-line-faces`."
  :group 'notmuch-search
  :group 'notmuch-faces)

(define-derived-mode notmuch-search-mode fundamental-mode "notmuch-search"
  "Major mode displaying results of a notmuch search.

This buffer contains the results of a \"notmuch search\" of your
email archives. Each line in the buffer represents a single
thread giving a summary of the thread (a relative date, the
number of matched messages and total messages in the thread,
participants in the thread, a representative subject line, and
any tags).

Pressing \\[notmuch-search-show-thread] on any line displays that
thread. The '\\[notmuch-search-add-tag]' and
'\\[notmuch-search-remove-tag]' keys can be used to add or remove
tags from a thread. The '\\[notmuch-search-archive-thread]' key
is a convenience for archiving a thread (applying changes in
`notmuch-archive-tags'). The '\\[notmuch-search-tag-all]' key can
be used to add and/or remove tags from all messages (as opposed
to threads) that match the current query.  Use with caution, as
this will also tag matching messages that arrived *after*
constructing the buffer.

Other useful commands are '\\[notmuch-search-filter]' for
filtering the current search based on an additional query string,
'\\[notmuch-search-filter-by-tag]' for filtering to include only
messages with a given tag, and '\\[notmuch-search]' to execute a
new, global search.

Complete list of currently available key bindings:

\\{notmuch-search-mode-map}"
  (make-local-variable 'notmuch-search-query-string)
  (make-local-variable 'notmuch-search-oldest-first)
  (make-local-variable 'notmuch-search-target-thread)
  (make-local-variable 'notmuch-search-target-line)
  (setq notmuch-buffer-refresh-function #'notmuch-search-refresh-view)
  (set (make-local-variable 'scroll-preserve-screen-position) t)
  (add-to-invisibility-spec (cons 'ellipsis t))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq imenu-prev-index-position-function
        #'notmuch-search-imenu-prev-index-position-function)
  (setq imenu-extract-index-name-function
        #'notmuch-search-imenu-extract-index-name-function))

(defun notmuch-search-get-result (&optional pos)
  "Return the result object for the thread at POS (or point).

If there is no thread at POS (or point), returns nil."
  (get-text-property (or pos (point)) 'notmuch-search-result))

(defun notmuch-search-result-beginning (&optional pos)
  "Return the point at the beginning of the thread at POS (or point).

If there is no thread at POS (or point), returns nil."
  (when (notmuch-search-get-result pos)
    ;; We pass 1+point because previous-single-property-change starts
    ;; searching one before the position we give it.
    (previous-single-property-change (1+ (or pos (point)))
				     'notmuch-search-result nil (point-min))))

(defun notmuch-search-result-end (&optional pos)
  "Return the point at the end of the thread at POS (or point).

The returned point will be just after the newline character that
ends the result line.  If there is no thread at POS (or point),
returns nil"
  (when (notmuch-search-get-result pos)
    (next-single-property-change (or pos (point)) 'notmuch-search-result
				 nil (point-max))))

(defun notmuch-search-foreach-result (beg end fn)
  "Invoke FN for each result between BEG and END.

FN should take one argument.  It will be applied to the
character position of the beginning of each result that overlaps
the region between points BEG and END.  As a special case, if (=
BEG END), FN will be applied to the result containing point
BEG."

  (lexical-let ((pos (notmuch-search-result-beginning beg))
		;; End must be a marker in case fn changes the
		;; text.
		(end (copy-marker end))
		;; Make sure we examine at least one result, even if
		;; (= beg end).
		(first t))
    ;; We have to be careful if the region extends beyond the results.
    ;; In this case, pos could be null or there could be no result at
    ;; pos.
    (while (and pos (or (< pos end) first))
      (when (notmuch-search-get-result pos)
	(funcall fn pos))
      (setq pos (notmuch-search-result-end pos)
	    first nil))))
;; Unindent the function argument of notmuch-search-foreach-result so
;; the indentation of callers doesn't get out of hand.
(put 'notmuch-search-foreach-result 'lisp-indent-function 2)

(defun notmuch-search-properties-in-region (property beg end)
  (let (output)
    (notmuch-search-foreach-result beg end
      (lambda (pos)
	(push (plist-get (notmuch-search-get-result pos) property) output)))
    output))

(defun notmuch-search-find-thread-id (&optional bare)
  "Return the thread for the current thread

If BARE is set then do not prefix with \"thread:\""
  (let ((thread (plist-get (notmuch-search-get-result) :thread)))
    (when thread (concat (unless bare "thread:") thread))))

(defun notmuch-search-find-stable-query ()
  "Return the stable queries for the current thread.

This returns a list (MATCHED-QUERY UNMATCHED-QUERY) for the
matched and unmatched messages in the current thread."
  (plist-get (notmuch-search-get-result) :query))

(defun notmuch-search-find-stable-query-region (beg end &optional only-matched)
  "Return the stable query for the current region.

If ONLY-MATCHED is non-nil, include only matched messages.  If it
is nil, include both matched and unmatched messages. If there are
no messages in the region then return nil."
  (let ((query-list nil) (all (not only-matched)))
    (dolist (queries (notmuch-search-properties-in-region :query beg end))
      (when (first queries)
	(push (first queries) query-list))
      (when (and all (second queries))
	(push (second queries) query-list)))
    (when query-list
      (concat "(" (mapconcat 'identity query-list ") or (") ")"))))

(defun notmuch-search-find-authors ()
  "Return the authors for the current thread"
  (plist-get (notmuch-search-get-result) :authors))

(defun notmuch-search-find-authors-region (beg end)
  "Return a list of authors for the current region"
  (notmuch-search-properties-in-region :authors beg end))

(defun notmuch-search-find-subject ()
  "Return the subject for the current thread"
  (plist-get (notmuch-search-get-result) :subject))

(defun notmuch-search-find-subject-region (beg end)
  "Return a list of authors for the current region"
  (notmuch-search-properties-in-region :subject beg end))

(defun notmuch-search-show-thread (&optional elide-toggle)
  "Display the currently selected thread.

With a prefix argument, invert the default value of
`notmuch-show-only-matching-messages' when displaying the
thread."
  (interactive "P")
  (let ((thread-id (notmuch-search-find-thread-id))
	(subject (notmuch-search-find-subject)))
    (if (> (length thread-id) 0)
	(notmuch-show thread-id
		      elide-toggle
		      (current-buffer)
		      notmuch-search-query-string
		      ;; Name the buffer based on the subject.
		      (concat "*" (truncate-string-to-width subject 30 nil nil t) "*"))
      (message "End of search results."))))

(defun notmuch-tree-from-search-current-query ()
  "Call notmuch tree with the current query"
  (interactive)
  (notmuch-tree notmuch-search-query-string))

(defun notmuch-tree-from-search-thread ()
  "Show the selected thread with notmuch-tree"
  (interactive)
  (notmuch-tree (notmuch-search-find-thread-id)
                notmuch-search-query-string
		nil
                (notmuch-prettify-subject (notmuch-search-find-subject))
		t))

(defun notmuch-search-reply-to-thread (&optional prompt-for-sender)
  "Begin composing a reply-all to the entire current thread in a new buffer."
  (interactive "P")
  (let ((message-id (notmuch-search-find-thread-id)))
    (notmuch-mua-new-reply message-id prompt-for-sender t)))

(defun notmuch-search-reply-to-thread-sender (&optional prompt-for-sender)
  "Begin composing a reply to the entire current thread in a new buffer."
  (interactive "P")
  (let ((message-id (notmuch-search-find-thread-id)))
    (notmuch-mua-new-reply message-id prompt-for-sender nil)))

(defun notmuch-search-set-tags (tags &optional pos)
  (let ((new-result (plist-put (notmuch-search-get-result pos) :tags tags)))
    (notmuch-search-update-result new-result pos)))

(defun notmuch-search-get-tags (&optional pos)
  (plist-get (notmuch-search-get-result pos) :tags))

(defun notmuch-search-get-tags-region (beg end)
  (let (output)
    (notmuch-search-foreach-result beg end
      (lambda (pos)
	(setq output (append output (notmuch-search-get-tags pos)))))
    output))

(defun notmuch-search-interactive-region ()
  "Return the bounds of the current interactive region.

This returns (BEG END), where BEG and END are the bounds of the
region if the region is active, or both `point' otherwise."
  (if (region-active-p)
      (list (region-beginning) (region-end))
    (list (point) (point))))

(defun notmuch-search-interactive-tag-changes (&optional initial-input)
  "Prompt for tag changes for the current thread or region.

Returns (TAG-CHANGES REGION-BEGIN REGION-END)."
  (let* ((region (notmuch-search-interactive-region))
	 (beg (first region)) (end (second region))
	 (prompt (if (= beg end) "Tag thread" "Tag region")))
    (cons (notmuch-read-tag-changes
	   (notmuch-search-get-tags-region beg end) prompt initial-input)
	  region)))

(defun notmuch-search-tag (tag-changes &optional beg end only-matched)
  "Change tags for the currently selected thread or region.

See `notmuch-tag' for information on the format of TAG-CHANGES.
When called interactively, this uses the region if the region is
active.  When called directly, BEG and END provide the region.
If these are nil or not provided, then, if the region is active
this applied to all threads meeting the region, and if the region
is inactive this applies to the thread at point.

If ONLY-MATCHED is non-nil, only tag matched messages."
  (interactive (notmuch-search-interactive-tag-changes))
  (unless (and beg end)
    (setq beg (car (notmuch-search-interactive-region))
	  end (cadr (notmuch-search-interactive-region))))
  (let ((search-string (notmuch-search-find-stable-query-region
			beg end only-matched)))
    (notmuch-tag search-string tag-changes)
    (notmuch-search-foreach-result beg end
      (lambda (pos)
	(notmuch-search-set-tags
	 (notmuch-update-tags (notmuch-search-get-tags pos) tag-changes)
	 pos)))))

(defun notmuch-search-add-tag (tag-changes &optional beg end)
  "Change tags for the current thread or region (defaulting to add).

Same as `notmuch-search-tag' but sets initial input to '+'."
  (interactive (notmuch-search-interactive-tag-changes "+"))
  (notmuch-search-tag tag-changes beg end))

(defun notmuch-search-remove-tag (tag-changes &optional beg end)
  "Change tags for the current thread or region (defaulting to remove).

Same as `notmuch-search-tag' but sets initial input to '-'."
  (interactive (notmuch-search-interactive-tag-changes "-"))
  (notmuch-search-tag tag-changes beg end))

(put 'notmuch-search-archive-thread 'notmuch-prefix-doc
     "Un-archive the currently selected thread.")
(defun notmuch-search-archive-thread (&optional unarchive beg end)
  "Archive the currently selected thread or region.

Archive each message in the currently selected thread by applying
the tag changes in `notmuch-archive-tags' to each (remove the
\"inbox\" tag by default). If a prefix argument is given, the
messages will be \"unarchived\" (i.e. the tag changes in
`notmuch-archive-tags' will be reversed).

This function advances the next thread when finished."
  (interactive (cons current-prefix-arg (notmuch-search-interactive-region)))
  (when notmuch-archive-tags
    (notmuch-search-tag
     (notmuch-tag-change-list notmuch-archive-tags unarchive) beg end))
  (when (eq beg end)
    (notmuch-search-next-thread)))

(defun notmuch-search-update-result (result &optional pos)
  "Replace the result object of the thread at POS (or point) by
RESULT and redraw it.

This will keep point in a reasonable location.  However, if there
are enclosing save-excursions and the saved point is in the
result being updated, the point will be restored to the beginning
of the result."
  (let ((start (notmuch-search-result-beginning pos))
	(end (notmuch-search-result-end pos))
	(init-point (point))
	(inhibit-read-only t))
    ;; Delete the current thread
    (delete-region start end)
    ;; Insert the updated thread
    (notmuch-search-show-result result start)
    ;; If point was inside the old result, make an educated guess
    ;; about where to place it now.  Unfortunately, this won't work
    ;; with save-excursion (or any other markers that would be nice to
    ;; preserve, such as the window start), but there's nothing we can
    ;; do about that without a way to retrieve markers in a region.
    (when (and (>= init-point start) (<= init-point end))
      (let* ((new-end (notmuch-search-result-end start))
	     (new-point (if (= init-point end)
			    new-end
			  (min init-point (- new-end 1)))))
	(goto-char new-point)))))

(defun notmuch-search-process-sentinel (proc msg)
  "Add a message to let user know when \"notmuch search\" exits"
  (let ((buffer (process-buffer proc))
	(status (process-status proc))
	(exit-status (process-exit-status proc))
	(never-found-target-thread nil))
    (when (memq status '(exit signal))
      (catch 'return
	(kill-buffer (process-get proc 'parse-buf))
	(if (buffer-live-p buffer)
	    (with-current-buffer buffer
	      (save-excursion
		(let ((inhibit-read-only t)
		      (atbob (bobp)))
		  (goto-char (point-max))
		  (if (eq status 'signal)
		      (insert "Incomplete search results (search process was killed).\n"))
		  (when (eq status 'exit)
		    (insert "End of search results.\n")
		    ;; For version mismatch, there's no point in
		    ;; showing the search buffer
		    (when (or (= exit-status 20) (= exit-status 21))
		      (kill-buffer)
		      (throw 'return nil))
		    (if (and atbob
			     (not (string= notmuch-search-target-thread "found")))
			(set 'never-found-target-thread t)))))
	      (when (and never-found-target-thread
		       notmuch-search-target-line)
		  (goto-char (point-min))
		  (forward-line (1- notmuch-search-target-line)))))))))

(define-widget 'notmuch--custom-face-edit 'lazy
  "Custom face edit with a tag Edit Face"
  ;; I could not persuage custom-face-edit to respect the :tag
  ;; property so create a widget specially
  :tag "Manually specify face"
  :type 'custom-face-edit)

(defcustom notmuch-search-line-faces
  '(("unread" . notmuch-search-unread-face)
    ("flagged" . notmuch-search-flagged-face))
  "Alist of tags to faces for line highlighting in notmuch-search.
Each element looks like (TAG . FACE).
A thread with TAG will have FACE applied.

Here is an example of how to color search results based on tags.
 (the following text would be placed in your ~/.emacs file):

 (setq notmuch-search-line-faces '((\"unread\" . (:foreground \"green\"))
                                   (\"deleted\" . (:foreground \"red\"
						  :background \"blue\"))))

The FACE must be a face name (a symbol or string), a property
list of face attributes, or a list of these.  The faces for
matching tags are merged, with earlier attributes overriding
later. A message having both \"deleted\" and \"unread\" tags with
the above settings would have a green foreground and blue
background."
  :type '(alist :key-type (string)
		:value-type (radio (face :tag "Face name")
				    (notmuch--custom-face-edit)))
  :group 'notmuch-search
  :group 'notmuch-faces)

(defun notmuch-search-color-line (start end line-tag-list)
  "Colorize lines in `notmuch-show' based on tags."
  ;; Reverse the list so earlier entries take precedence
  (dolist (elem (reverse notmuch-search-line-faces))
    (let ((tag (car elem))
	  (face (cdr elem)))
      (when (member tag line-tag-list)
	(notmuch-apply-face nil face nil start end)))))

(defun notmuch-search-author-propertize (authors)
  "Split `authors' into matching and non-matching authors and
propertize appropriately. If no boundary between authors and
non-authors is found, assume that all of the authors match."
  (if (string-match "\\(.*\\)|\\(.*\\)" authors)
      (concat (propertize (concat (match-string 1 authors) ",")
			  'face 'notmuch-search-matching-authors)
	      (propertize (match-string 2 authors)
			  'face 'notmuch-search-non-matching-authors))
    (propertize authors 'face 'notmuch-search-matching-authors)))

(defun notmuch-search-insert-authors (format-string authors)
  ;; Save the match data to avoid interfering with
  ;; `notmuch-search-process-filter'.
  (save-match-data
    (let* ((formatted-authors (format format-string authors))
	   (formatted-sample (format format-string ""))
	   (visible-string formatted-authors)
	   (invisible-string "")
	   (padding ""))

      ;; Truncate the author string to fit the specification.
      (if (> (length formatted-authors)
	     (length formatted-sample))
	  (let ((visible-length (- (length formatted-sample)
				   (length "... "))))
	    ;; Truncate the visible string according to the width of
	    ;; the display string.
	    (setq visible-string (substring formatted-authors 0 visible-length)
		  invisible-string (substring formatted-authors visible-length))
	    ;; If possible, truncate the visible string at a natural
	    ;; break (comma or pipe), as incremental search doesn't
	    ;; match across the visible/invisible border.
	    (when (string-match "\\(.*\\)\\([,|] \\)\\([^,|]*\\)" visible-string)
	      ;; Second clause is destructive on `visible-string', so
	      ;; order is important.
	      (setq invisible-string (concat (match-string 3 visible-string)
					     invisible-string)
		    visible-string (concat (match-string 1 visible-string)
					   (match-string 2 visible-string))))
	    ;; `visible-string' may be shorter than the space allowed
	    ;; by `format-string'. If so we must insert some padding
	    ;; after `invisible-string'.
	    (setq padding (make-string (- (length formatted-sample)
					  (length visible-string)
					  (length "..."))
				       ? ))))

      ;; Use different faces to show matching and non-matching authors.
      (if (string-match "\\(.*\\)|\\(.*\\)" visible-string)
	  ;; The visible string contains both matching and
	  ;; non-matching authors.
	  (setq visible-string (notmuch-search-author-propertize visible-string)
		;; The invisible string must contain only non-matching
		;; authors, as the visible-string contains both.
		invisible-string (propertize invisible-string
					     'face 'notmuch-search-non-matching-authors))
	;; The visible string contains only matching authors.
	(setq visible-string (propertize visible-string
					 'face 'notmuch-search-matching-authors)
	      ;; The invisible string may contain both matching and
	      ;; non-matching authors.
	      invisible-string (notmuch-search-author-propertize invisible-string)))

      ;; If there is any invisible text, add it as a tooltip to the
      ;; visible text.
      (when (not (string= invisible-string ""))
	(setq visible-string (propertize visible-string 'help-echo (concat "..." invisible-string))))

      ;; Insert the visible and, if present, invisible author strings.
      (insert visible-string)
      (when (not (string= invisible-string ""))
	(let ((start (point))
	      overlay)
	  (insert invisible-string)
	  (setq overlay (make-overlay start (point)))
	  (overlay-put overlay 'invisible 'ellipsis)
	  (overlay-put overlay 'isearch-open-invisible #'delete-overlay)))
      (insert padding))))

(defun notmuch-search-insert-field (field format-string result)
  (cond
   ((string-equal field "date")
    (insert (propertize (format format-string (plist-get result :date_relative))
			'face 'notmuch-search-date)))
   ((string-equal field "count")
    (insert (propertize (format format-string
				(format "[%s/%s]" (plist-get result :matched)
					(plist-get result :total)))
			'face 'notmuch-search-count)))
   ((string-equal field "subject")
    (insert (propertize (format format-string
				(notmuch-sanitize (plist-get result :subject)))
			'face 'notmuch-search-subject)))

   ((string-equal field "authors")
    (notmuch-search-insert-authors
     format-string (notmuch-sanitize (plist-get result :authors))))

   ((string-equal field "tags")
    (let ((tags (plist-get result :tags))
	  (orig-tags (plist-get result :orig-tags)))
      (insert (format format-string (notmuch-tag-format-tags tags orig-tags)))))))

(defun notmuch-search-show-result (result pos)
  "Insert RESULT at POS."
  ;; Ignore excluded matches
  (unless (= (plist-get result :matched) 0)
    (save-excursion
      (goto-char pos)
      (dolist (spec notmuch-search-result-format)
	(notmuch-search-insert-field (car spec) (cdr spec) result))
      (insert "\n")
      (notmuch-search-color-line pos (point) (plist-get result :tags))
      (put-text-property pos (point) 'notmuch-search-result result))))

(defun notmuch-search-append-result (result)
  "Insert RESULT at the end of the buffer.

This is only called when a result is first inserted so it also
sets the :orig-tag property."
  (let ((new-result (plist-put result :orig-tags (plist-get result :tags)))
	(pos (point-max)))
    (notmuch-search-show-result new-result pos)
    (when (string= (plist-get result :thread) notmuch-search-target-thread)
      (setq notmuch-search-target-thread "found")
      (goto-char pos))))

(defun notmuch-search-process-filter (proc string)
  "Process and filter the output of \"notmuch search\""
  (let ((results-buf (process-buffer proc))
	(parse-buf (process-get proc 'parse-buf))
	(inhibit-read-only t)
	done)
    (when (buffer-live-p results-buf)
      (with-current-buffer parse-buf
	;; Insert new data
	(save-excursion
	  (goto-char (point-max))
	  (insert string))
	(notmuch-sexp-parse-partial-list 'notmuch-search-append-result
					 results-buf)))))

(defun notmuch-search-tag-all (tag-changes)
  "Add/remove tags from all messages in current search buffer.

See `notmuch-tag' for information on the format of TAG-CHANGES."
  (interactive
   (list (notmuch-read-tag-changes
	  (notmuch-search-get-tags-region (point-min) (point-max)) "Tag all")))
  (notmuch-search-tag tag-changes (point-min) (point-max) t))

(defun notmuch-search-buffer-title (query)
  "Returns the title for a buffer with notmuch search results."
  (let* ((saved-search
	  (let (longest
		(longest-length 0))
	    (loop for tuple in notmuch-saved-searches
		  if (let ((quoted-query (regexp-quote (notmuch-saved-search-get tuple :query))))
		       (and (string-match (concat "^" quoted-query) query)
			    (> (length (match-string 0 query))
			       longest-length)))
		  do (setq longest tuple))
	    longest))
	 (saved-search-name (notmuch-saved-search-get saved-search :name))
	 (saved-search-query (notmuch-saved-search-get saved-search :query)))
    (cond ((and saved-search (equal saved-search-query query))
	   ;; Query is the same as saved search (ignoring case)
	   (concat "*notmuch-saved-search-" saved-search-name "*"))
	  (saved-search
	   (concat "*notmuch-search-"
		   (replace-regexp-in-string (concat "^" (regexp-quote saved-search-query))
					     (concat "[ " saved-search-name " ]")
					     query)
		   "*"))
	  (t
	   (concat "*notmuch-search-" query "*"))
	  )))

(defun notmuch-read-query (prompt)
  "Read a notmuch-query from the minibuffer with completion.

PROMPT is the string to prompt with."
  (lexical-let*
      ((all-tags
        (mapcar (lambda (tag) (notmuch-escape-boolean-term tag))
                (process-lines notmuch-command "search" "--output=tags" "*")))
       (completions
	 (append (list "folder:" "path:" "thread:" "id:" "date:" "from:" "to:"
		       "subject:" "attachment:")
		 (mapcar (lambda (tag) (concat "tag:" tag)) all-tags)
		 (mapcar (lambda (tag) (concat "is:" tag)) all-tags)
		 (mapcar (lambda (mimetype) (concat "mimetype:" mimetype)) (mailcap-mime-types)))))
    (let ((keymap (copy-keymap minibuffer-local-map))
	  (current-query (case major-mode
			   (notmuch-search-mode (notmuch-search-get-query))
			   (notmuch-show-mode (notmuch-show-get-query))
			   (notmuch-tree-mode (notmuch-tree-get-query))))
	  (minibuffer-completion-table
	   (completion-table-dynamic
	    (lambda (string)
	      ;; generate a list of possible completions for the current input
	      (cond
	       ;; this ugly regexp is used to get the last word of the input
	       ;; possibly preceded by a '('
	       ((string-match "\\(^\\|.* (?\\)\\([^ ]*\\)$" string)
		(mapcar (lambda (compl)
			  (concat (match-string-no-properties 1 string) compl))
			(all-completions (match-string-no-properties 2 string)
					 completions)))
	       (t (list string)))))))
      ;; this was simpler than convincing completing-read to accept spaces:
      (define-key keymap (kbd "TAB") 'minibuffer-complete)
      (let ((history-delete-duplicates t))
	(read-from-minibuffer prompt nil keymap nil
			      'notmuch-search-history current-query nil)))))

(defun notmuch-search-get-query ()
  "Return the current query in this search buffer"
  notmuch-search-query-string)

(put 'notmuch-search 'notmuch-doc "Search for messages.")
;;;###autoload
(defun notmuch-search (&optional query oldest-first target-thread target-line no-display)
  "Display threads matching QUERY in a notmuch-search buffer.

If QUERY is nil, it is read interactively from the minibuffer.
Other optional parameters are used as follows:

  OLDEST-FIRST: A Boolean controlling the sort order of returned threads
  TARGET-THREAD: A thread ID (without the thread: prefix) that will be made
                 current if it appears in the search results.
  TARGET-LINE: The line number to move to if the target thread does not
               appear in the search results.
  NO-DISPLAY: Do not try to foreground the search results buffer. If it is
              already foregrounded i.e. displayed in a window, this has no
              effect, meaning the buffer will remain visible.

When called interactively, this will prompt for a query and use
the configured default sort order."
  (interactive
   (list
    ;; Prompt for a query
    nil
    ;; Use the default search order (if we're doing a search from a
    ;; search buffer, ignore any buffer-local overrides)
    (default-value 'notmuch-search-oldest-first)))

  (let* ((query (or query (notmuch-read-query "Notmuch search: ")))
	 (buffer (get-buffer-create (notmuch-search-buffer-title query))))
    (if no-display
	(set-buffer buffer)
      (switch-to-buffer buffer))
    (notmuch-search-mode)
    ;; Don't track undo information for this buffer
    (set 'buffer-undo-list t)
    (set 'notmuch-search-query-string query)
    (set 'notmuch-search-oldest-first oldest-first)
    (set 'notmuch-search-target-thread target-thread)
    (set 'notmuch-search-target-line target-line)
    (notmuch-tag-clear-cache)
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (if proc
	  (error "notmuch search process already running for query `%s'" query)
	)
      (erase-buffer)
      (goto-char (point-min))
      (save-excursion
	(let ((proc (notmuch-start-notmuch
		     "notmuch-search" buffer #'notmuch-search-process-sentinel
		     "search" "--format=sexp" "--format-version=4"
		     (if oldest-first
			 "--sort=oldest-first"
		       "--sort=newest-first")
		     query))
	      ;; Use a scratch buffer to accumulate partial output.
	      ;; This buffer will be killed by the sentinel, which
	      ;; should be called no matter how the process dies.
	      (parse-buf (generate-new-buffer " *notmuch search parse*")))
	  (process-put proc 'parse-buf parse-buf)
	  (set-process-filter proc 'notmuch-search-process-filter)
	  (set-process-query-on-exit-flag proc nil))))
    (run-hooks 'notmuch-search-hook)))

(defun notmuch-search-refresh-view ()
  "Refresh the current view.

Erases the current buffer and runs a new search with the same
query string as the current search. If the current thread is in
the new search results, then point will be placed on the same
thread. Otherwise, point will be moved to attempt to be in the
same relative position within the new buffer."
  (interactive)
  (let ((target-line (line-number-at-pos))
	(oldest-first notmuch-search-oldest-first)
	(target-thread (notmuch-search-find-thread-id 'bare))
	(query notmuch-search-query-string))
    ;; notmuch-search erases the current buffer.
    (notmuch-search query oldest-first target-thread target-line t)
    (goto-char (point-min))))

(defun notmuch-search-toggle-order ()
  "Toggle the current search order.

This command toggles the sort order for the current search. The
default sort order is defined by `notmuch-search-oldest-first'."
  (interactive)
  (set 'notmuch-search-oldest-first (not notmuch-search-oldest-first))
  (notmuch-search-refresh-view))

(defun notmuch-group-disjunctive-query-string (query-string)
  "Group query if it contains a complex expression.

Enclose QUERY-STRING in parentheses if it matches
`notmuch-search-disjunctive-regexp'."
  (if (string-match-p notmuch-search-disjunctive-regexp query-string)
      (concat "( " query-string " )")
    query-string))

(defun notmuch-search-filter (query)
  "Filter or LIMIT the current search results based on an additional query string.

Runs a new search matching only messages that match both the
current search results AND the additional query string provided."
  (interactive (list (notmuch-read-query "Filter search: ")))
  (let ((grouped-query (notmuch-group-disjunctive-query-string query))
	(grouped-original-query (notmuch-group-disjunctive-query-string
				 notmuch-search-query-string)))
    (notmuch-search (if (string= grouped-original-query "*")
			grouped-query
		      (concat grouped-original-query " and " grouped-query))
		    notmuch-search-oldest-first)))

(defun notmuch-search-filter-by-tag (tag)
  "Filter the current search results based on a single tag.

Runs a new search matching only messages that match both the
current search results AND that are tagged with the given tag."
  (interactive
   (list (notmuch-select-tag-with-completion "Filter by tag: ")))
  (notmuch-search (concat notmuch-search-query-string " and tag:" tag) notmuch-search-oldest-first))

;;;###autoload
(defun notmuch ()
  "Run notmuch and display saved searches, known tags, etc."
  (interactive)
  (notmuch-hello))

(defun notmuch-interesting-buffer (b)
  "Is the current buffer of interest to a notmuch user?"
  (with-current-buffer b
    (memq major-mode '(notmuch-show-mode
		       notmuch-search-mode
		       notmuch-tree-mode
		       notmuch-hello-mode
		       notmuch-message-mode))))

;;;###autoload
(defun notmuch-cycle-notmuch-buffers ()
  "Cycle through any existing notmuch buffers (search, show or hello).

If the current buffer is the only notmuch buffer, bury it. If no
notmuch buffers exist, run `notmuch'."
  (interactive)

  (let (start first)
    ;; If the current buffer is a notmuch buffer, remember it and then
    ;; bury it.
    (when (notmuch-interesting-buffer (current-buffer))
      (setq start (current-buffer))
      (bury-buffer))

    ;; Find the first notmuch buffer.
    (setq first (loop for buffer in (buffer-list)
		      if (notmuch-interesting-buffer buffer)
		      return buffer))

    (if first
	;; If the first one we found is any other than the starting
	;; buffer, switch to it.
	(unless (eq first start)
	  (switch-to-buffer first))
      (notmuch))))

;;;; Imenu Support

(defun notmuch-search-imenu-prev-index-position-function ()
  "Move point to previous message in notmuch-search buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (notmuch-search-previous-thread))

(defun notmuch-search-imenu-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (let ((subject (notmuch-search-find-subject))
	(author (notmuch-search-find-authors)))
    (format "%s (%s)" subject author)))

(setq mail-user-agent 'notmuch-user-agent)

(provide 'notmuch)

;; After provide to avoid loops if notmuch was require'd via notmuch-init-file.
(if init-file-user ; don't load init file if the -q option was used.
    (let ((init-file (locate-file notmuch-init-file '("/")
				  (get-load-suffixes))))
      (if init-file (load init-file nil t t))))

;;; notmuch.el ends here

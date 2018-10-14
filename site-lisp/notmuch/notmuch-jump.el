;;; notmuch-jump.el --- User-friendly shortcut keys
;;
;; Copyright © Austin Clements
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
;; Authors: Austin Clements <aclements@csail.mit.edu>
;;          David Edmondson <dme@dme.org>

;;; Code:

(eval-when-compile (require 'cl))

(require 'notmuch-lib)
(require 'notmuch-hello)

(eval-and-compile
  (unless (fboundp 'window-body-width)
    ;; Compatibility for Emacs pre-24
    (defalias 'window-body-width 'window-width)))

;;;###autoload
(defun notmuch-jump-search ()
  "Jump to a saved search by shortcut key.

This prompts for and performs a saved search using the shortcut
keys configured in the :key property of `notmuch-saved-searches'.
Typically these shortcuts are a single key long, so this is a
fast way to jump to a saved search from anywhere in Notmuch."
  (interactive)

  ;; Build the action map
  (let (action-map)
    (dolist (saved-search notmuch-saved-searches)
      (let* ((saved-search (notmuch-hello-saved-search-to-plist saved-search))
	     (key (plist-get saved-search :key)))
	(when key
	  (let ((name (plist-get saved-search :name))
		(query (plist-get saved-search :query))
		(oldest-first
		 (case (plist-get saved-search :sort-order)
		   (newest-first nil)
		   (oldest-first t)
		   (otherwise (default-value 'notmuch-search-oldest-first)))))
	    (push (list key name
			(if (eq (plist-get saved-search :search-type) 'tree)
			    `(lambda () (notmuch-tree ',query))
			  `(lambda () (notmuch-search ',query ',oldest-first))))
		  action-map)))))
    (setq action-map (nreverse action-map))

    (if action-map
	(notmuch-jump action-map "Search: ")
      (error "To use notmuch-jump, please customize shortcut keys in notmuch-saved-searches."))))

(defvar notmuch-jump--action nil)

(defun notmuch-jump (action-map prompt)
  "Interactively prompt for one of the keys in ACTION-MAP.

Displays a summary of all bindings in ACTION-MAP in the
minibuffer, reads a key from the minibuffer, and performs the
corresponding action.  The prompt can be canceled with C-g or
RET.  PROMPT must be a string to use for the prompt.  PROMPT
should include a space at the end.

ACTION-MAP must be a list of triples of the form
  (KEY LABEL ACTION)
where KEY is a key binding, LABEL is a string label to display in
the buffer, and ACTION is a nullary function to call.  LABEL may
be null, in which case the action will still be bound, but will
not appear in the pop-up buffer.
"

  (let* ((items (notmuch-jump--format-actions action-map))
	 ;; Format the table of bindings and the full prompt
	 (table
	  (with-temp-buffer
	    (notmuch-jump--insert-items (window-body-width) items)
	    (buffer-string)))
	 (full-prompt
	  (concat table "\n\n"
		  (propertize prompt 'face 'minibuffer-prompt)))
	 ;; By default, the minibuffer applies the minibuffer face to
	 ;; the entire prompt.  However, we want to clearly
	 ;; distinguish bindings (which we put in the prompt face
	 ;; ourselves) from their labels, so disable the minibuffer's
	 ;; own re-face-ing.
	 (minibuffer-prompt-properties
	  (notmuch-plist-delete
	   (copy-sequence minibuffer-prompt-properties)
	   'face))
	 ;; Build the keymap with our bindings
	 (minibuffer-map (notmuch-jump--make-keymap action-map prompt))
	 ;; The bindings save the the action in notmuch-jump--action
	 (notmuch-jump--action nil))
    ;; Read the action
    (read-from-minibuffer full-prompt nil minibuffer-map)

    ;; If we got an action, do it
    (when notmuch-jump--action
      (funcall notmuch-jump--action))))

(defun notmuch-jump--format-actions (action-map)
  "Format the actions in ACTION-MAP.

Returns a list of strings, one for each item with a label in
ACTION-MAP.  These strings can be inserted into a tabular
buffer."

  ;; Compute the maximum key description width
  (let ((key-width 1))
    (dolist (entry action-map)
      (setq key-width
	    (max key-width
		 (string-width (format-kbd-macro (first entry))))))
    ;; Format each action
    (mapcar (lambda (entry)
	      (let ((key (format-kbd-macro (first entry)))
		    (desc (second entry)))
		(concat
		 (propertize key 'face 'minibuffer-prompt)
		 (make-string (- key-width (length key)) ? )
		 " " desc)))
	    action-map)))

(defun notmuch-jump--insert-items (width items)
  "Make a table of ITEMS up to WIDTH wide in the current buffer."
  (let* ((nitems (length items))
	 (col-width (+ 3 (apply #'max (mapcar #'string-width items))))
	 (ncols (if (> (* col-width nitems) width)
		    (max 1 (/ width col-width))
		  ;; Items fit on one line.  Space them out
		  (setq col-width (/ width nitems))
		  (length items))))
    (while items
      (dotimes (col ncols)
	(when items
	  (let ((item (pop items)))
	    (insert item)
	    (when (and items (< col (- ncols 1)))
	      (insert (make-string (- col-width (string-width item)) ? ))))))
      (when items
	(insert "\n")))))

(defvar notmuch-jump-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    ;; Make this like a special-mode keymap, with no self-insert-command
    (suppress-keymap map)
    (define-key map (kbd "DEL") 'exit-minibuffer)
    map)
  "Base keymap for notmuch-jump's minibuffer keymap.")

(defun notmuch-jump--make-keymap (action-map prompt)
  "Translate ACTION-MAP into a minibuffer keymap."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map notmuch-jump-minibuffer-map)
    (dolist (action action-map)
      (if (= (length (first action)) 1)
	  (define-key map (first action)
	    `(lambda () (interactive)
	       (setq notmuch-jump--action ',(third action))
	       (exit-minibuffer)))))
    ;; By doing this in two passes (and checking if we already have a
    ;; binding) we avoid problems if the user specifies a binding which
    ;; is a prefix of another binding.
    (dolist (action action-map)
      (if (> (length (first action)) 1)
	  (let* ((key (elt (first action) 0))
		 (keystr (string key))
		 (new-prompt (concat prompt (format-kbd-macro keystr) " "))
		 (action-submap nil))
	    (unless (lookup-key map keystr)
	      (dolist (act action-map)
		(when (= key (elt (first act) 0))
		  (push (list (substring (first act) 1)
			      (second act)
			      (third act))
			action-submap)))
	      ;; We deal with backspace specially
	      (push (list (kbd "DEL")
			  "Backup"
			  (apply-partially #'notmuch-jump action-map prompt))
		    action-submap)
	      (setq action-submap (nreverse action-submap))
	      (define-key map keystr
		`(lambda () (interactive)
		   (setq notmuch-jump--action
			 ',(apply-partially #'notmuch-jump action-submap new-prompt))
		   (exit-minibuffer)))))))
    map))

;;

(provide 'notmuch-jump)

;;; notmuch-jump.el ends here

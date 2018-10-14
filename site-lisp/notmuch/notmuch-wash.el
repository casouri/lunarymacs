;;; notmuch-wash.el --- cleaning up message bodies
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

(require 'coolj)

(declare-function notmuch-show-insert-bodypart "notmuch-show" (msg part depth &optional hide))
(defvar notmuch-show-indent-messages-width)

;;

(defgroup notmuch-wash nil
  "Cleaning up messages for display."
  :group 'notmuch)

(defcustom notmuch-wash-signature-regexp "^\\(-- ?\\|_+\\)$"
  "Pattern to match a line that separates content from signature."
  :type 'regexp
  :group 'notmuch-wash)

(defcustom notmuch-wash-citation-regexp "\\(^[[:space:]]*>.*\n\\)+"
  "Pattern to match citation lines."
  :type 'regexp
  :group 'notmuch-wash)

(defcustom notmuch-wash-original-regexp "^\\(--+\s?[oO]riginal [mM]essage\s?--+\\)$"
  "Pattern to match a line that separates original message from
reply in top-posted message."
  :type 'regexp
  :group 'notmuch-wash)

(defcustom notmuch-wash-button-signature-hidden-format
  "[ %d-line signature. Click/Enter to show. ]"
  "String used to construct button text for hidden signatures.
Can use up to one integer format parameter, i.e. %d."
  :type 'string
  :group 'notmuch-wash)

(defcustom notmuch-wash-button-signature-visible-format
  "[ %d-line signature. Click/Enter to hide. ]"
  "String used to construct button text for visible signatures.
Can use up to one integer format parameter, i.e. %d."
  :type 'string
  :group 'notmuch-wash)

(defcustom notmuch-wash-button-citation-hidden-format
  "[ %d more citation lines. Click/Enter to show. ]"
  "String used to construct button text for hidden citations.
Can use up to one integer format parameter, i.e. %d."
  :type 'string
  :group 'notmuch-wash)

(defcustom notmuch-wash-button-citation-visible-format
  "[ %d more citation lines. Click/Enter to hide. ]"
  "String used to construct button text for visible citations.
Can use up to one integer format parameter, i.e. %d."
  :type 'string
  :group 'notmuch-wash)

(defcustom notmuch-wash-button-original-hidden-format
  "[ %d-line hidden original message. Click/Enter to show. ]"
  "String used to construct button text for hidden citations.
Can use up to one integer format parameter, i.e. %d."
  :type 'string
  :group 'notmuch-wash)

(defcustom notmuch-wash-button-original-visible-format
  "[ %d-line original message. Click/Enter to hide. ]"
  "String used to construct button text for visible citations.
Can use up to one integer format parameter, i.e. %d."
  :type 'string
  :group 'notmuch-wash)

(defcustom notmuch-wash-signature-lines-max 12
  "Maximum length of signature that will be hidden by default."
  :type 'integer
  :group 'notmuch-wash)

(defcustom notmuch-wash-citation-lines-prefix 3
  "Always show at least this many lines from the start of a citation.

If there is one more line than the sum of
`notmuch-wash-citation-lines-prefix' and
`notmuch-wash-citation-lines-suffix', show that, otherwise
collapse the remaining lines into a button."
  :type 'integer
  :group 'notmuch-wash)

(defcustom notmuch-wash-citation-lines-suffix 3
  "Always show at least this many lines from the end of a citation.

If there is one more line than the sum of
`notmuch-wash-citation-lines-prefix' and
`notmuch-wash-citation-lines-suffix', show that, otherwise
collapse the remaining lines into a button."
  :type 'integer
  :group 'notmuch-wash)

(defcustom notmuch-wash-wrap-lines-length nil
  "Wrap line after at most this many characters.

If this is nil, lines in messages will be wrapped to fit in the
current window. If this is a number, lines will be wrapped after
this many characters (ignoring indentation due to thread depth)
or at the window width (whichever one is lower)."
  :type '(choice (const :tag "window width" nil)
		 (integer :tag "number of characters"))
  :group 'notmuch-wash)

(defface notmuch-wash-toggle-button
  '((t (:inherit font-lock-comment-face)))
  "Face used for buttons toggling the visibility of washed away
message parts."
  :group 'notmuch-wash
  :group 'notmuch-faces)

(defface notmuch-wash-cited-text
  '((t (:inherit message-cited-text)))
  "Face used for cited text."
  :group 'notmuch-wash
  :group 'notmuch-faces)

(defun notmuch-wash-toggle-invisible-action (cite-button)
  ;; Toggle overlay visibility
  (let ((overlay (button-get cite-button 'overlay)))
    (overlay-put overlay 'invisible (not (overlay-get overlay 'invisible))))
  ;; Update button text
  (let* ((new-start (button-start cite-button))
	 (overlay (button-get cite-button 'overlay))
	 (button-label (notmuch-wash-button-label overlay))
	 (old-point (point))
	 (properties (text-properties-at (point)))
	 (inhibit-read-only t))
    (goto-char new-start)
    (insert button-label)
    (set-text-properties new-start (point) properties)
    (let ((old-end (button-end cite-button)))
      (move-overlay cite-button new-start (point))
      (delete-region (point) old-end))
    (goto-char (min old-point (1- (button-end cite-button))))))

(define-button-type 'notmuch-wash-button-invisibility-toggle-type
  'action 'notmuch-wash-toggle-invisible-action
  'follow-link t
  'face 'notmuch-wash-toggle-button
  :supertype 'notmuch-button-type)

(define-button-type 'notmuch-wash-button-citation-toggle-type
  'help-echo "mouse-1, RET: Show citation"
  :supertype 'notmuch-wash-button-invisibility-toggle-type)

(define-button-type 'notmuch-wash-button-signature-toggle-type
  'help-echo "mouse-1, RET: Show signature"
  :supertype 'notmuch-wash-button-invisibility-toggle-type)

(define-button-type 'notmuch-wash-button-original-toggle-type
  'help-echo "mouse-1, RET: Show original message"
  :supertype 'notmuch-wash-button-invisibility-toggle-type)

(defun notmuch-wash-region-isearch-show (overlay)
  (notmuch-wash-toggle-invisible-action
   (overlay-get overlay 'notmuch-wash-button)))

(defun notmuch-wash-button-label (overlay)
  (let* ((type (overlay-get overlay 'type))
	 (invis-spec (overlay-get overlay 'invisible))
	 (state (if (invisible-p invis-spec) "hidden" "visible"))
	 (label-format (symbol-value (intern-soft (concat "notmuch-wash-button-"
							  type "-" state "-format"))))
	 (lines-count (count-lines (overlay-start overlay) (overlay-end overlay))))
    (format label-format lines-count)))

(defun notmuch-wash-region-to-button (msg beg end type &optional prefix)
  "Auxiliary function to do the actual making of overlays and buttons

BEG and END are buffer locations. TYPE should a string, either
\"citation\" or \"signature\". Optional PREFIX is some arbitrary
text to insert before the button, probably for indentation.  Note
that PREFIX should not include a newline."

  ;; This uses some slightly tricky conversions between strings and
  ;; symbols because of the way the button code works. Note that
  ;; replacing intern-soft with make-symbol will cause this to fail,
  ;; since the newly created symbol has no plist.

  (let ((overlay (make-overlay beg end))
	(button-type (intern-soft (concat "notmuch-wash-button-"
					  type "-toggle-type"))))
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'isearch-open-invisible #'notmuch-wash-region-isearch-show)
    (overlay-put overlay 'type type)
    (goto-char (1+ end))
    (save-excursion
      (goto-char beg)
      (if prefix
	  (insert-before-markers prefix))
      (let ((button-beg (point)))
	(insert-before-markers (notmuch-wash-button-label overlay) "\n")
	(let ((button (make-button button-beg (1- (point))
				   'overlay overlay
				   :type button-type)))
	  (overlay-put overlay 'notmuch-wash-button button))))))

(defun notmuch-wash-excerpt-citations (msg depth)
  "Excerpt citations and up to one signature."
  (goto-char (point-min))
  (beginning-of-line)
  (if (and (< (point) (point-max))
	   (re-search-forward notmuch-wash-original-regexp nil t))
      (let* ((msg-start (match-beginning 0))
	     (msg-end (point-max))
	     (msg-lines (count-lines msg-start msg-end)))
	(notmuch-wash-region-to-button
	 msg msg-start msg-end "original")))
  (while (and (< (point) (point-max))
	      (re-search-forward notmuch-wash-citation-regexp nil t))
    (let* ((cite-start (match-beginning 0))
	   (cite-end (match-end 0))
	   (cite-lines (count-lines cite-start cite-end)))
      (overlay-put (make-overlay cite-start cite-end) 'face 'notmuch-wash-cited-text)
      (when (> cite-lines (+ notmuch-wash-citation-lines-prefix
			     notmuch-wash-citation-lines-suffix
			     1))
	(goto-char cite-start)
	(forward-line notmuch-wash-citation-lines-prefix)
	(let ((hidden-start (point-marker)))
	  (goto-char cite-end)
	  (forward-line (- notmuch-wash-citation-lines-suffix))
	  (notmuch-wash-region-to-button
	   msg hidden-start (point-marker)
	   "citation")))))
  (if (and (not (eobp))
	   (re-search-forward notmuch-wash-signature-regexp nil t))
      (let* ((sig-start (match-beginning 0))
	     (sig-end (match-end 0))
	     (sig-lines (count-lines sig-start (point-max))))
	(if (<= sig-lines notmuch-wash-signature-lines-max)
	    (let ((sig-start-marker (make-marker))
		  (sig-end-marker (make-marker)))
	      (set-marker sig-start-marker sig-start)
	      (set-marker sig-end-marker (point-max))
	      (overlay-put (make-overlay sig-start-marker sig-end-marker) 'face 'message-cited-text)
	      (notmuch-wash-region-to-button
	       msg sig-start-marker sig-end-marker
	       "signature"))))))

;;

(defun notmuch-wash-elide-blank-lines (msg depth)
  "Elide leading, trailing and successive blank lines."

  ;; Algorithm derived from `article-strip-multiple-blank-lines' in
  ;; `gnus-art.el'.

  ;; Make all blank lines empty.
  (goto-char (point-min))
  (while (re-search-forward "^[[:space:]\t]+$" nil t)
    (replace-match "" nil t))

  ;; Replace multiple empty lines with a single empty line.
  (goto-char (point-min))
  (while (re-search-forward "^\n\\(\n+\\)" nil t)
    (delete-region (match-beginning 1) (match-end 1)))

  ;; Remove a leading blank line.
  (goto-char (point-min))
  (if (looking-at "\n")
      (delete-region (match-beginning 0) (match-end 0)))

  ;; Remove a trailing blank line.
  (goto-char (point-max))
  (if (looking-at "\n")
      (delete-region (match-beginning 0) (match-end 0))))

;;

(defun notmuch-wash-tidy-citations (msg depth)
  "Improve the display of cited regions of a message.

Perform several transformations on the message body:

- Remove lines of repeated citation leaders with no other
  content,
- Remove citation leaders standing alone before a block of cited
  text,
- Remove citation trailers standing alone after a block of cited
  text."

  ;; Remove lines of repeated citation leaders with no other content.
  (goto-char (point-min))
  (while (re-search-forward "\\(^>[> ]*\n\\)\\{2,\\}" nil t)
    (replace-match "\\1"))

  ;; Remove citation leaders standing alone before a block of cited
  ;; text.
  (goto-char (point-min))
  (while (re-search-forward "\\(\n\\|^[^>].*\\)\n\\(^>[> ]*\n\\)" nil t)
    (replace-match "\\1\n"))

  ;; Remove citation trailers standing alone after a block of cited
  ;; text.
  (goto-char (point-min))
  (while (re-search-forward "\\(^>[> ]*\n\\)\\(^$\\|^[^>].*\\)" nil t)
    (replace-match "\\2")))

;;

(defun notmuch-wash-wrap-long-lines (msg depth)
  "Wrap long lines in the message.

If `notmuch-wash-wrap-lines-length' is a number, this will wrap
the message lines to the minimum of the width of the window or
its value. Otherwise, this function will wrap long lines in the
message at the window width. When doing so, citation leaders in
the wrapped text are maintained."

  (let* ((coolj-wrap-follows-window-size nil)
	 (indent (* depth notmuch-show-indent-messages-width))
	 (limit (if (numberp notmuch-wash-wrap-lines-length)
		    (min (+ notmuch-wash-wrap-lines-length indent)
			 (window-width))
		  (window-width)))
	 (fill-column (- limit
			 indent
			 ;; 2 to avoid poor interaction with
			 ;; `word-wrap'.
			 2)))
    (coolj-wrap-region (point-min) (point-max))))

;;

(require 'diff-mode)

(defvar diff-file-header-re) ; From `diff-mode.el'.

(defun notmuch-wash-subject-to-filename (subject &optional maxlen)
  "Convert a mail SUBJECT into a filename.

The resulting filename is similar to the names generated by \"git
format-patch\", without the leading patch sequence number
\"0001-\" and \".patch\" extension. Any leading \"[PREFIX]\"
style strings are removed prior to conversion.

Optional argument MAXLEN is the maximum length of the resulting
filename, before trimming any trailing . and - characters."
  (let* ((s (replace-regexp-in-string "^ *\\(\\[[^]]*\\] *\\)*" "" subject))
	 (s (replace-regexp-in-string "[^A-Za-z0-9._]+" "-" s))
	 (s (replace-regexp-in-string "\\.+" "." s))
	 (s (if maxlen (substring s 0 (min (length s) maxlen)) s))
	 (s (replace-regexp-in-string "[.-]*$" "" s)))
    s))

(defun notmuch-wash-subject-to-patch-sequence-number (subject)
  "Convert a patch mail SUBJECT into a patch sequence number.

Return the patch sequence number N from the last \"[PATCH N/M]\"
style prefix in SUBJECT, or nil if such a prefix can't be found."
  (when (string-match
	 "^ *\\(\\[[^]]*\\] *\\)*\\[[^]]*?\\([0-9]+\\)/[0-9]+[^]]*\\].*"
	 subject)
      (string-to-number (substring subject (match-beginning 2) (match-end 2)))))

(defun notmuch-wash-subject-to-patch-filename (subject)
  "Convert a patch mail SUBJECT into a filename.

The resulting filename is similar to the names generated by \"git
format-patch\". If the patch mail was generated and sent using
\"git format-patch/send-email\", this should re-create the
original filename the sender had."
  (format "%04d-%s.patch"
	  (or (notmuch-wash-subject-to-patch-sequence-number subject) 1)
	  (notmuch-wash-subject-to-filename subject 52)))

(defun notmuch-wash-convert-inline-patch-to-part (msg depth)
  "Convert an inline patch into a fake 'text/x-diff' attachment.

Given that this function guesses whether a buffer includes a
patch and then guesses the extent of the patch, there is scope
for error."

  (goto-char (point-min))
  (when (re-search-forward diff-file-header-re nil t)
    (beginning-of-line -1)
    (let ((patch-start (point))
	  (patch-end (point-max))
	  part)
      (goto-char patch-start)
      (if (or
	   ;; Patch ends with signature.
	   (re-search-forward notmuch-wash-signature-regexp nil t)
	   ;; Patch ends with bugtraq comment.
	   (re-search-forward "^\\*\\*\\* " nil t))
	  (setq patch-end (match-beginning 0)))
      (save-restriction
	(narrow-to-region patch-start patch-end)
	(setq part (plist-put part :content-type "inline patch"))
	(setq part (plist-put part :content (buffer-string)))
	(setq part (plist-put part :id -1))
	(setq part (plist-put part :filename
			      (notmuch-wash-subject-to-patch-filename
			       (plist-get
				(plist-get msg :headers) :Subject))))
	(delete-region (point-min) (point-max))
	(notmuch-show-insert-bodypart nil part depth)))))

;;

(provide 'notmuch-wash)

;;; notmuch-wash.el ends here

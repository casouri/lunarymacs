;;; notmuch-draft.el --- functions for postponing and editing drafts
;;
;; Copyright © Mark Walters
;; Copyright © David Bremner
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
;; Authors: Mark Walters <markwalters1009@gmail.com>
;;	    David Bremner <david@tethera.net>

;;; Code:

(require 'notmuch-maildir-fcc)
(require 'notmuch-tag)

(declare-function notmuch-show-get-message-id "notmuch-show" (&optional bare))
(declare-function notmuch-message-mode "notmuch-mua")

(defgroup notmuch-draft nil
  "Saving and editing drafts in Notmuch."
  :group 'notmuch)

(defcustom notmuch-draft-tags '("+draft")
  "List of tags changes to apply to a draft message when it is saved in the database.

Tags starting with \"+\" (or not starting with either \"+\" or
\"-\") in the list will be added, and tags starting with \"-\"
will be removed from the message being stored.

For example, if you wanted to give the message a \"draft\" tag
but not the (normally added by default) \"inbox\" tag, you would
set:
    (\"+draft\" \"-inbox\")"
  :type '(repeat string)
  :group 'notmuch-draft)

(defcustom notmuch-draft-folder "drafts"
  "Folder to save draft messages in.

This should be specified relative to the root of the notmuch
database. It will be created if necessary."
  :type 'string
  :group 'notmuch-draft)

(defcustom notmuch-draft-quoted-tags '()
  "Mml tags to quote.

This should be a list of mml tags to quote before saving. You do
not need to include \"secure\" as that is handled separately.

If you include \"part\" then attachments will not be saved with
the draft -- if not then they will be saved with the draft. The
former means the attachments may not still exist when you resume
the message, the latter means that the attachments as they were
when you postponed will be sent with the resumed message.

Note you may get strange results if you change this between
postponing and resuming a message."
  :type '(repeat string)
  :group 'notmuch-send)

(defcustom notmuch-draft-save-plaintext 'ask
  "Should notmuch save/postpone in plaintext messages that seem
  like they are intended to be sent encrypted
(i.e with an mml encryption tag in it)."
  :type '(radio
	  (const :tag "Never" nil)
	  (const :tag "Ask every time" ask)
	  (const :tag "Always" t))
  :group 'notmuch-draft
  :group 'notmuch-crypto)

(defvar notmuch-draft-encryption-tag-regex
  "<#\\(part encrypt\\|secure.*mode=.*encrypt>\\)"
  "Regular expression matching mml tags indicating encryption of part or message")

(defvar notmuch-draft-id nil
  "Message-id of the most recent saved draft of this message")
(make-variable-buffer-local 'notmuch-draft-id)

(defun notmuch-draft--mark-deleted ()
  "Tag the last saved draft deleted.

Used when a new version is saved, or the message is sent."
  (when notmuch-draft-id
    (notmuch-tag notmuch-draft-id '("+deleted"))))

(defun notmuch-draft-quote-some-mml ()
  "Quote the mml tags in `notmuch-draft-quoted-tags`."
  (save-excursion
    ;; First we deal with any secure tag separately.
    (message-goto-body)
    (when (looking-at "<#secure[^\n]*>\n")
      (let ((secure-tag (match-string 0)))
	(delete-region (match-beginning 0) (match-end 0))
	(message-add-header (concat "X-Notmuch-Emacs-Secure: " secure-tag))))
    ;; This is copied from mml-quote-region but only quotes the
    ;; specified tags.
    (when notmuch-draft-quoted-tags
      (let ((re (concat "<#!*/?\\("
			(mapconcat 'regexp-quote notmuch-draft-quoted-tags "\\|")
			"\\)")))
	(message-goto-body)
	(while (re-search-forward re nil t)
	  ;; Insert ! after the #.
	  (goto-char (+ (match-beginning 0) 2))
	  (insert "!"))))))

(defun notmuch-draft-unquote-some-mml ()
  "Unquote the mml tags in `notmuch-draft-quoted-tags`."
  (save-excursion
    (when notmuch-draft-quoted-tags
      (let ((re (concat "<#!+/?\\("
			(mapconcat 'regexp-quote notmuch-draft-quoted-tags "\\|")
			"\\)")))
	(message-goto-body)
	(while (re-search-forward re nil t)
	  ;; Remove one ! from after the #.
	  (goto-char (+ (match-beginning 0) 2))
	  (delete-char 1))))
    (let (secure-tag)
      (save-restriction
	(message-narrow-to-headers)
	(setq secure-tag (message-fetch-field "X-Notmuch-Emacs-Secure" 't))
	(message-remove-header "X-Notmuch-Emacs-Secure"))
      (message-goto-body)
      (when secure-tag
	(insert secure-tag "\n")))))

(defun notmuch-draft--has-encryption-tag ()
  "Returns t if there is an mml secure tag."
  (save-excursion
    (message-goto-body)
    (re-search-forward notmuch-draft-encryption-tag-regex nil 't)))

(defun notmuch-draft--query-encryption ()
  "Checks if we should save a message that should be encrypted.

`notmuch-draft-save-plaintext' controls the behaviour."
  (case notmuch-draft-save-plaintext
	((ask)
	 (unless (yes-or-no-p "(Customize `notmuch-draft-save-plaintext' to avoid this warning)
This message contains mml tags that suggest it is intended to be encrypted.
Really save and index an unencrypted copy? ")
	   (error "Save aborted")))
	((nil)
	 (error "Refusing to save draft with encryption tags (see `notmuch-draft-save-plaintext')"))
	((t)
	 (ignore))))

(defun notmuch-draft--make-message-id ()
  ;; message-make-message-id gives the id inside a "<" ">" pair,
  ;; but notmuch doesn't want that form, so remove them.
  (concat "draft-" (substring (message-make-message-id) 1 -1)))

(defun notmuch-draft-save ()
  "Save the current draft message in the notmuch database.

This saves the current message in the database with tags
`notmuch-draft-tags` (in addition to any default tags
applied to newly inserted messages)."
  (interactive)
  (when (notmuch-draft--has-encryption-tag)
    (notmuch-draft--query-encryption))
  (let ((id (notmuch-draft--make-message-id)))
    (with-temporary-notmuch-message-buffer
     ;; We insert a Date header and a Message-ID header, the former
     ;; so that it is easier to search for the message, and the
     ;; latter so we have a way of accessing the saved message (for
     ;; example to delete it at a later time). We check that the
     ;; user has these in `message-deletable-headers` (the default)
     ;; as otherwise they are doing something strange and we
     ;; shouldn't interfere. Note, since we are doing this in a new
     ;; buffer we don't change the version in the compose buffer.
     (cond
      ((member 'Message-ID message-deletable-headers)
       (message-remove-header "Message-ID")
       (message-add-header (concat "Message-ID: <" id ">")))
      (t
       (message "You have customized emacs so Message-ID is not a deletable header, so not changing it")
       (setq id nil)))
     (cond
      ((member 'Date message-deletable-headers)
       (message-remove-header "Date")
       (message-add-header (concat "Date: " (message-make-date))))
      (t
       (message "You have customized emacs so Date is not a deletable header, so not changing it")))
     (message-add-header "X-Notmuch-Emacs-Draft: True")
     (notmuch-draft-quote-some-mml)
     (notmuch-maildir-setup-message-for-saving)
     (notmuch-maildir-notmuch-insert-current-buffer
      notmuch-draft-folder 't notmuch-draft-tags))
    ;; We are now back in the original compose buffer. Note the
    ;; function notmuch-call-notmuch-process (called by
    ;; notmuch-maildir-notmuch-insert-current-buffer) signals an error
    ;; on failure, so to get to this point it must have
    ;; succeeded. Also, notmuch-draft-id is still the id of the
    ;; previous draft, so it is safe to mark it deleted.
    (notmuch-draft--mark-deleted)
    (setq notmuch-draft-id (concat "id:" id))
    (set-buffer-modified-p nil)))

(defun notmuch-draft-postpone ()
  "Save the draft message in the notmuch database and exit buffer."
  (interactive)
  (notmuch-draft-save)
  (kill-buffer))

(defun notmuch-draft-resume (id)
  "Resume editing of message with id ID."
  (let* ((tags (process-lines notmuch-command "search" "--output=tags"
			      "--exclude=false" id))
	 (draft (equal tags (notmuch-update-tags tags notmuch-draft-tags))))
    (when (or draft
	      (yes-or-no-p "Message does not appear to be a draft: really resume? "))
      (switch-to-buffer (get-buffer-create (concat "*notmuch-draft-" id "*")))
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((coding-system-for-read 'no-conversion))
	(call-process notmuch-command nil t nil "show" "--format=raw" id))
      (mime-to-mml)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
	(replace-match mail-header-separator t t))
      ;; Remove the Date and Message-ID headers (unless the user has
      ;; explicitly customized emacs to tell us not to) as they will
      ;; be replaced when the message is sent.
      (save-restriction
	(message-narrow-to-headers)
	(when (member 'Message-ID message-deletable-headers)
	  (message-remove-header "Message-ID"))
	(when (member 'Date message-deletable-headers)
	  (message-remove-header "Date"))
	;; The X-Notmuch-Emacs-Draft header is a more reliable
	;; indication of whether the message really is a draft.
	(setq draft (> (message-remove-header "X-Notmuch-Emacs-Draft") 0)))
      ;; If the message is not a draft we should not unquote any mml.
      (when draft
	(notmuch-draft-unquote-some-mml))
      (notmuch-message-mode)
      (message-goto-body)
      (set-buffer-modified-p nil)
      ;; If the resumed message was a draft then set the draft
      ;; message-id so that we can delete the current saved draft if the
      ;; message is resaved or sent.
      (setq notmuch-draft-id (when draft id)))))


(add-hook 'message-send-hook 'notmuch-draft--mark-deleted)


(provide 'notmuch-draft)

;;; notmuch-draft.el ends here

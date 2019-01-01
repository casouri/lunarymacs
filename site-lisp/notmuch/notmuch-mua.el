;;; notmuch-mua.el --- emacs style mail-user-agent
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
(require 'mm-view)
(require 'format-spec)

(require 'notmuch-lib)
(require 'notmuch-address)
(require 'notmuch-draft)

(eval-when-compile (require 'cl))

(declare-function notmuch-show-insert-body "notmuch-show" (msg body depth))
(declare-function notmuch-fcc-header-setup "notmuch-maildir-fcc" ())
(declare-function notmuch-maildir-message-do-fcc "notmuch-maildir-fcc" ())
(declare-function notmuch-draft-postpone "notmuch-draft" ())
(declare-function notmuch-draft-save "notmuch-draft" ())

;;

(defcustom notmuch-mua-send-hook '(notmuch-mua-message-send-hook)
  "Hook run before sending messages."
  :type 'hook
  :group 'notmuch-send
  :group 'notmuch-hooks)

(defcustom notmuch-mua-compose-in 'current-window
  (concat
   "Where to create the mail buffer used to compose a new message.
Possible values are `current-window' (default), `new-window' and
`new-frame'. If set to `current-window', the mail buffer will be
displayed in the current window, so the old buffer will be
restored when the mail buffer is killed. If set to `new-window'
or `new-frame', the mail buffer will be displayed in a new
window/frame that will be destroyed when the buffer is killed.
You may want to customize `message-kill-buffer-on-exit'
accordingly."
   (when (< emacs-major-version 24)
	   " Due to a known bug in Emacs 23, you should not set
this to `new-window' if `message-kill-buffer-on-exit' is
disabled: this would result in an incorrect behavior."))
  :group 'notmuch-send
  :type '(choice (const :tag "Compose in the current window" current-window)
		 (const :tag "Compose mail in a new window"  new-window)
		 (const :tag "Compose mail in a new frame"   new-frame)))

(defcustom notmuch-mua-user-agent-function nil
  "Function used to generate a `User-Agent:' string. If this is
`nil' then no `User-Agent:' will be generated."
  :type '(choice (const :tag "No user agent string" nil)
		 (const :tag "Full" notmuch-mua-user-agent-full)
		 (const :tag "Notmuch" notmuch-mua-user-agent-notmuch)
		 (const :tag "Emacs" notmuch-mua-user-agent-emacs)
		 (function :tag "Custom user agent function"
			   :value notmuch-mua-user-agent-full))
  :group 'notmuch-send)

(defcustom notmuch-mua-hidden-headers nil
  "Headers that are added to the `message-mode' hidden headers
list."
  :type '(repeat string)
  :group 'notmuch-send)

(defgroup notmuch-reply nil
  "Replying to messages in notmuch"
  :group 'notmuch)

(defcustom notmuch-mua-cite-function 'message-cite-original
  "*Function for citing an original message.
Predefined functions include `message-cite-original' and
`message-cite-original-without-signature'.
Note that these functions use `mail-citation-hook' if that is non-nil."
  :type '(radio (function-item message-cite-original)
		(function-item message-cite-original-without-signature)
		(function-item sc-cite-original)
		(function :tag "Other"))
  :link '(custom-manual "(message)Insertion Variables")
  :group 'notmuch-reply)

(defcustom notmuch-mua-reply-insert-header-p-function
  'notmuch-show-reply-insert-header-p-never
  "Function to decide which parts get a header when replying.

This function specifies which parts of a mime message with
multiple parts get a header."
  :type '(radio (const :tag "No part headers"
		               notmuch-show-reply-insert-header-p-never)
		(const :tag "All except multipart/* and hidden parts"
		               notmuch-show-reply-insert-header-p-trimmed)
		(const :tag "Only for included text parts"
			       notmuch-show-reply-insert-header-p-minimal)
		(const :tag "Exactly as in show view"
			       notmuch-show-insert-header-p)
		(function :tag "Other"))
  :group 'notmuch-reply)

;;

(defun notmuch-mua-get-switch-function ()
  "Get a switch function according to `notmuch-mua-compose-in'."
  (cond ((eq notmuch-mua-compose-in 'current-window)
	 'switch-to-buffer)
	((eq notmuch-mua-compose-in 'new-window)
	 'switch-to-buffer-other-window)
	((eq notmuch-mua-compose-in 'new-frame)
	 'switch-to-buffer-other-frame)
	(t (error "Invalid value for `notmuch-mua-compose-in'"))))

(defun notmuch-mua-maybe-set-window-dedicated ()
  "Set the selected window as dedicated according to
`notmuch-mua-compose-in'."
  (when (or (eq notmuch-mua-compose-in 'new-frame)
	    (eq notmuch-mua-compose-in 'new-window))
    (set-window-dedicated-p (selected-window) t)))

(defun notmuch-mua-user-agent-full ()
  "Generate a `User-Agent:' string suitable for notmuch."
  (concat (notmuch-mua-user-agent-notmuch)
	  " "
	  (notmuch-mua-user-agent-emacs)))

(defun notmuch-mua-user-agent-notmuch ()
  "Generate a `User-Agent:' string suitable for notmuch."
  (let ((notmuch-version (if (string= notmuch-emacs-version "unknown")
			     (notmuch-cli-version)
			   notmuch-emacs-version)))
    (concat "Notmuch/" notmuch-version " (https://notmuchmail.org)")))

(defun notmuch-mua-user-agent-emacs ()
  "Generate a `User-Agent:' string suitable for notmuch."
  (concat "Emacs/" emacs-version " (" system-configuration ")"))

(defun notmuch-mua-add-more-hidden-headers ()
  "Add some headers to the list that are hidden by default."
  (mapc (lambda (header)
	  (when (not (member header message-hidden-headers))
	    (push header message-hidden-headers)))
	notmuch-mua-hidden-headers))

(defun notmuch-mua-reply-crypto (parts)
  "Add mml sign-encrypt flag if any part of original message is encrypted."
  (loop for part in parts
	if (notmuch-match-content-type (plist-get part :content-type) "multipart/encrypted")
	  do (mml-secure-message-sign-encrypt)
	else if (notmuch-match-content-type (plist-get part :content-type) "multipart/*")
	  do (notmuch-mua-reply-crypto (plist-get part :content))))

;; There is a bug in emacs 23's message.el that results in a newline
;; not being inserted after the References header, so the next header
;; is concatenated to the end of it. This function fixes the problem,
;; while guarding against the possibility that some current or future
;; version of emacs has the bug fixed.
(defun notmuch-mua-insert-references (original-func header references)
  (funcall original-func header references)
  (unless (bolp) (insert "\n")))

(defun notmuch-mua-reply (query-string &optional sender reply-all)
  (let ((args '("reply" "--format=sexp" "--format-version=4"))
	(process-crypto notmuch-show-process-crypto)
	reply
	original)
    (when process-crypto
      (setq args (append args '("--decrypt=true"))))

    (if reply-all
	(setq args (append args '("--reply-to=all")))
      (setq args (append args '("--reply-to=sender"))))
    (setq args (append args (list query-string)))

    ;; Get the reply object as SEXP, and parse it into an elisp object.
    (setq reply (apply #'notmuch-call-notmuch-sexp args))

    ;; Extract the original message to simplify the following code.
    (setq original (plist-get reply :original))

    ;; Extract the headers of both the reply and the original message.
    (let* ((original-headers (plist-get original :headers))
	   (reply-headers (plist-get reply :reply-headers)))

      ;; If sender is non-nil, set the From: header to its value.
      (when sender
	(plist-put reply-headers :From sender))
      (let
	  ;; Overlay the composition window on that being used to read
	  ;; the original message.
	  ((same-window-regexps '("\\*mail .*")))

	;; We modify message-header-format-alist to get around a bug in message.el.
	;; See the comment above on notmuch-mua-insert-references.
	(let ((message-header-format-alist
	       (loop for pair in message-header-format-alist
		     if (eq (car pair) 'References)
		     collect (cons 'References
				   (apply-partially
				    'notmuch-mua-insert-references
				    (cdr pair)))
		     else
		     collect pair)))
	  (notmuch-mua-mail (plist-get reply-headers :To)
			    (notmuch-sanitize (plist-get reply-headers :Subject))
			    (notmuch-headers-plist-to-alist reply-headers)
			    nil (notmuch-mua-get-switch-function))))

      ;; Insert the message body - but put it in front of the signature
      ;; if one is present, and after any other content
      ;; message*setup-hooks may have added to the message body already.
      (save-restriction
	(message-goto-body)
	(narrow-to-region (point) (point-max))
	(goto-char (point-max))
	(if (re-search-backward message-signature-separator nil t)
	    (if message-signature-insert-empty-line
		(forward-line -1))
	  (goto-char (point-max))))

      (let ((from (plist-get original-headers :From))
	    (date (plist-get original-headers :Date))
	    (start (point)))

	;; notmuch-mua-cite-function constructs a citation line based
	;; on the From and Date headers of the original message, which
	;; are assumed to be in the buffer.
	(insert "From: " from "\n")
	(insert "Date: " date "\n\n")

	(insert (with-temp-buffer
		  (let
		      ;; Don't attempt to clean up messages, excerpt
		      ;; citations, etc. in the original message before
		      ;; quoting.
		      ((notmuch-show-insert-text/plain-hook nil)
		       ;; Don't omit long parts.
		       (notmuch-show-max-text-part-size 0)
		       ;; Insert headers for parts as appropriate for replying.
		       (notmuch-show-insert-header-p-function notmuch-mua-reply-insert-header-p-function)
		       ;; Ensure that any encrypted parts are
		       ;; decrypted during the generation of the reply
		       ;; text.
		       (notmuch-show-process-crypto process-crypto)
		       ;; Don't indent multipart sub-parts.
		       (notmuch-show-indent-multipart nil))
		    ;; We don't want sigstatus buttons (an information leak and usually wrong anyway).
		    (letf (((symbol-function 'notmuch-crypto-insert-sigstatus-button) #'ignore)
			   ((symbol-function 'notmuch-crypto-insert-encstatus-button) #'ignore))
			  (notmuch-show-insert-body original (plist-get original :body) 0)
			  (buffer-substring-no-properties (point-min) (point-max))))))

	(set-mark (point))
	(goto-char start)
	;; Quote the original message according to the user's configured style.
	(funcall notmuch-mua-cite-function)))

    ;; Crypto processing based crypto content of the original message
    (when process-crypto
      (notmuch-mua-reply-crypto (plist-get original :body))))

  ;; Push mark right before signature, if any.
  (message-goto-signature)
  (unless (eobp)
    (end-of-line -1))
  (push-mark)

  (message-goto-body)
  (set-buffer-modified-p nil))

(define-derived-mode notmuch-message-mode message-mode "Message[Notmuch]"
  "Notmuch message composition mode. Mostly like `message-mode'"
  (notmuch-address-setup))

(put 'notmuch-message-mode 'flyspell-mode-predicate 'mail-mode-flyspell-verify)

(define-key notmuch-message-mode-map (kbd "C-c C-c") #'notmuch-mua-send-and-exit)
(define-key notmuch-message-mode-map (kbd "C-c C-s") #'notmuch-mua-send)
(define-key notmuch-message-mode-map (kbd "C-c C-p") #'notmuch-draft-postpone)
(define-key notmuch-message-mode-map (kbd "C-x C-s") #'notmuch-draft-save)

(defun notmuch-mua-pop-to-buffer (name switch-function)
  "Pop to buffer NAME, and warn if it already exists and is
modified. This function is notmuch addaptation of
`message-pop-to-buffer'."
  (let ((buffer (get-buffer name)))
    (if (and buffer
	     (buffer-name buffer))
	(let ((window (get-buffer-window buffer 0)))
	  (if window
	      ;; Raise the frame already displaying the message buffer.
	      (progn
		(select-frame-set-input-focus (window-frame window))
		(select-window window))
	    (funcall switch-function buffer)
	    (set-buffer buffer))
	  (when (and (buffer-modified-p)
		     (not (prog1
			      (y-or-n-p
			       "Message already being composed; erase? ")
			    (message nil))))
	    (error "Message being composed")))
      (funcall switch-function name)
      (set-buffer name))
    (erase-buffer)
    (notmuch-message-mode)))

(defun notmuch-mua-mail (&optional to subject other-headers continue
				   switch-function yank-action send-actions
				   return-action &rest ignored)
  "Invoke the notmuch mail composition window."
  (interactive)

  (when notmuch-mua-user-agent-function
    (let ((user-agent (funcall notmuch-mua-user-agent-function)))
      (when (not (string= "" user-agent))
	(push (cons 'User-Agent user-agent) other-headers))))

  (unless (assq 'From other-headers)
    (push (cons 'From (message-make-from
		       (notmuch-user-name) (notmuch-user-primary-email))) other-headers))

  (notmuch-mua-pop-to-buffer (message-buffer-name "mail" to)
			     (or switch-function (notmuch-mua-get-switch-function)))
  (let ((headers
	 (append
	  ;; The following is copied from `message-mail'
	  `((To . ,(or to "")) (Subject . ,(or subject "")))
	  ;; C-h f compose-mail says that headers should be specified as
	  ;; (string . value); however all the rest of message expects
	  ;; headers to be symbols, not strings (eg message-header-format-alist).
	  ;; https://lists.gnu.org/archive/html/emacs-devel/2011-01/msg00337.html
	  ;; We need to convert any string input, eg from rmail-start-mail.
	  (dolist (h other-headers other-headers)
	    (if (stringp (car h)) (setcar h (intern (capitalize (car h))))))))
	(args (list yank-action send-actions))
	;; Cause `message-setup-1' to do things relevant for mail,
	;; such as observe `message-default-mail-headers'.
	(message-this-is-mail t))
    ;; message-setup-1 in Emacs 23 does not accept return-action
    ;; argument. Pass it only if it is supplied by the caller. This
    ;; will never be the case when we're called by `compose-mail' in
    ;; Emacs 23.
    (when return-action (nconc args '(return-action)))
    (apply 'message-setup-1 headers args))
  (notmuch-fcc-header-setup)
  (message-sort-headers)
  (message-hide-headers)
  (set-buffer-modified-p nil)
  (notmuch-mua-maybe-set-window-dedicated)

  (message-goto-to))

(defcustom notmuch-identities nil
  "Identities that can be used as the From: address when composing a new message.

If this variable is left unset, then a list will be constructed from the
name and addresses configured in the notmuch configuration file."
  :type '(repeat string)
  :group 'notmuch-send)

(defcustom notmuch-always-prompt-for-sender nil
  "Always prompt for the From: address when composing or forwarding a message.

This is not taken into account when replying to a message, because in that case
the From: header is already filled in by notmuch."
  :type 'boolean
  :group 'notmuch-send)

(defvar notmuch-mua-sender-history nil)

;; Workaround: Running `ido-completing-read' in emacs 23.1, 23.2 and 23.3
;; without some explicit initialization fill freeze the operation.
;; Hence, we advice `ido-completing-read' to ensure required initialization
;; is done.
(if (and (= emacs-major-version 23) (< emacs-minor-version 4))
    (defadvice ido-completing-read (before notmuch-ido-mode-init activate)
      (ido-init-completion-maps)
      (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
      (add-hook 'choose-completion-string-functions
		'ido-choose-completion-string)
      (ad-disable-advice 'ido-completing-read 'before 'notmuch-ido-mode-init)
      (ad-activate 'ido-completing-read)))

(defun notmuch-mua-prompt-for-sender ()
  "Prompt for a sender from the user's configured identities."
  (if notmuch-identities
      (ido-completing-read "Send mail from: " notmuch-identities
			   nil nil nil 'notmuch-mua-sender-history
			   (car notmuch-identities))
    (let* ((name (notmuch-user-name))
	   (addrs (cons (notmuch-user-primary-email)
			(notmuch-user-other-email)))
	   (address
	    (ido-completing-read (concat "Sender address for " name ": ") addrs
				 nil nil nil 'notmuch-mua-sender-history
				 (car addrs))))
      (message-make-from name address))))

(put 'notmuch-mua-new-mail 'notmuch-prefix-doc "... and prompt for sender")
(defun notmuch-mua-new-mail (&optional prompt-for-sender)
  "Compose new mail.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first."
  (interactive "P")
  (let ((other-headers
	 (when (or prompt-for-sender notmuch-always-prompt-for-sender)
	   (list (cons 'From (notmuch-mua-prompt-for-sender))))))
    (notmuch-mua-mail nil nil other-headers nil (notmuch-mua-get-switch-function))))

(defun notmuch-mua-new-forward-messages (messages &optional prompt-for-sender)
  "Compose a new message forwarding MESSAGES.

If PROMPT-FOR-SENDER is non-nil, the user will be prompteed for
the From: address."
  (let* ((other-headers
	  (when (or prompt-for-sender notmuch-always-prompt-for-sender)
	    (list (cons 'From (notmuch-mua-prompt-for-sender)))))
	 forward-subject) ;; Comes from the first message and is
			  ;; applied later.

    ;; Generate the template for the outgoing message.
    (notmuch-mua-mail nil "" other-headers nil (notmuch-mua-get-switch-function))

    (save-excursion
      ;; Insert all of the forwarded messages.
      (mapc (lambda (id)
	      (let ((temp-buffer (get-buffer-create
				  (concat "*notmuch-fwd-raw-" id "*"))))
		;; Get the raw version of this message in the buffer.
		(with-current-buffer temp-buffer
		  (erase-buffer)
		  (let ((coding-system-for-read 'no-conversion))
		    (call-process notmuch-command nil t nil "show" "--format=raw" id))
		  ;; Because we process the messages in reverse order,
		  ;; always generate a forwarded subject, then use the
		  ;; last (i.e. first) one.
		  (setq forward-subject (message-make-forward-subject)))
		;; Make a copy ready to be forwarded in the
		;; composition buffer.
		(message-forward-make-body temp-buffer)
		;; Kill the temporary buffer.
		(kill-buffer temp-buffer)))
	    ;; `message-forward-make-body' always puts the message at
	    ;; the top, so do them in reverse order.
	    (reverse messages))

      ;; Add in the appropriate subject.
      (save-restriction
	(message-narrow-to-headers)
	(message-remove-header "Subject")
	(message-add-header (concat "Subject: " forward-subject)))

      ;; `message-forward-make-body' shows the User-agent header.  Hide
      ;; it again.
      (message-hide-headers)
      (set-buffer-modified-p nil))))

(defun notmuch-mua-new-reply (query-string &optional prompt-for-sender reply-all)
  "Compose a reply to the message identified by QUERY-STRING.

If PROMPT-FOR-SENDER is non-nil, the user will be prompted for
the From: address first.  If REPLY-ALL is non-nil, the message
will be addressed to all recipients of the source message."

;; In current emacs (24.3) select-active-regions is set to t by
;; default. The reply insertion code sets the region to the quoted
;; message to make it easy to delete (kill-region or C-w). These two
;; things combine to put the quoted message in the primary selection.
;;
;; This is not what the user wanted and is a privacy risk (accidental
;; pasting of the quoted message). We can avoid some of the problems
;; by let-binding select-active-regions to nil. This fixes if the
;; primary selection was previously in a non-emacs window but not if
;; it was in an emacs window. To avoid the problem in the latter case
;; we deactivate mark.

  (let ((sender
	 (when prompt-for-sender
	   (notmuch-mua-prompt-for-sender)))
	(select-active-regions nil))
    (notmuch-mua-reply query-string sender reply-all)
    (deactivate-mark)))

(defun notmuch-mua-check-no-misplaced-secure-tag ()
  "Query user if there is a misplaced secure mml tag.

Emacs message-send will (probably) ignore a secure mml tag unless
it is at the start of the body. Returns t if there is no such
tag, or the user confirms they mean it."
  (save-excursion
    (let ((body-start (progn (message-goto-body) (point))))
      (goto-char (point-max))
      (or
       ;; We are always fine if there is no secure tag.
       (not (search-backward "<#secure" nil 't))
       ;; There is a secure tag, so it must be at the start of the
       ;; body, with no secure tag earlier (i.e., in the headers).
       (and (= (point) body-start)
	    (not (search-backward "<#secure" nil 't)))
       ;; The user confirms they means it.
       (yes-or-no-p "\
There is a <#secure> tag not at the start of the body. It is
likely that the message will be sent unsigned and unencrypted.
Really send? ")))))

(defun notmuch-mua-check-secure-tag-has-newline ()
  "Query if the secure mml tag has a newline following it.

Emacs message-send will (probably) ignore a correctly placed
secure mml tag unless it is followed by a newline. Returns t if
any secure tag is followed by a newline, or the user confirms
they mean it."
  (save-excursion
    (message-goto-body)
    (or
     ;; There is no (correctly placed) secure tag.
     (not (looking-at "<#secure"))
     ;; The secure tag is followed by a newline.
     (looking-at "<#secure[^\n>]*>\n")
     ;; The user confirms they means it.
     (yes-or-no-p "\
The <#secure> tag at the start of the body is not followed by a
newline. It is likely that the message will be sent unsigned and
unencrypted.  Really send? "))))

(defun notmuch-mua-send-common (arg &optional exit)
  (interactive "P")
  (run-hooks 'notmuch-mua-send-hook)
  (when (and (notmuch-mua-check-no-misplaced-secure-tag)
	     (notmuch-mua-check-secure-tag-has-newline))
    (letf (((symbol-function 'message-do-fcc) #'notmuch-maildir-message-do-fcc))
	  (if exit
	      (message-send-and-exit arg)
	    (message-send arg)))))

(defun notmuch-mua-send-and-exit (&optional arg)
  (interactive "P")
  (notmuch-mua-send-common arg 't))

(defun notmuch-mua-send (&optional arg)
  (interactive "P")
  (notmuch-mua-send-common arg))

(defun notmuch-mua-kill-buffer ()
  (interactive)
  (message-kill-buffer))

(defun notmuch-mua-message-send-hook ()
  "The default function used for `notmuch-mua-send-hook', this
simply runs the corresponding `message-mode' hook functions."
  (run-hooks 'message-send-hook))

;;

(define-mail-user-agent 'notmuch-user-agent
  'notmuch-mua-mail 'notmuch-mua-send-and-exit
  'notmuch-mua-kill-buffer 'notmuch-mua-send-hook)

;; Add some more headers to the list that `message-mode' hides when
;; composing a message.
(notmuch-mua-add-more-hidden-headers)

;;

(provide 'notmuch-mua)

;;; notmuch-mua.el ends here

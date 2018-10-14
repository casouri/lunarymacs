;;; notmuch-crypto.el --- functions for handling display of cryptographic metadata.
;;
;; Copyright © Jameson Rollins
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
;; Authors: Jameson Rollins <jrollins@finestructure.net>

;;; Code:

(require 'epg)
(require 'notmuch-lib)

(defcustom notmuch-crypto-process-mime t
  "Should cryptographic MIME parts be processed?

If this variable is non-nil signatures in multipart/signed
messages will be verified and multipart/encrypted parts will be
decrypted.  The result of the crypto operation will be displayed
in a specially colored header button at the top of the processed
part.  Signed parts will have variously colored headers depending
on the success or failure of the verification process and on the
validity of user ID of the signer.

The effect of setting this variable can be seen temporarily by
providing a prefix when viewing a signed or encrypted message, or
by providing a prefix when reloading the message in notmuch-show
mode."
  :type 'boolean
  :package-version '(notmuch . "0.25")
  :group 'notmuch-crypto)

(defface notmuch-crypto-part-header
  '((((class color)
      (background dark))
     (:foreground "LightBlue1"))
    (((class color)
      (background light))
     (:foreground "blue")))
  "Face used for crypto parts headers."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-signature-good
  '((t (:background "green" :foreground "black")))
  "Face used for good signatures."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-signature-good-key
  '((t (:background "orange" :foreground "black")))
  "Face used for good signatures."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-signature-bad
  '((t (:background "red" :foreground "black")))
  "Face used for bad signatures."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-signature-unknown
  '((t (:background "red" :foreground "black")))
  "Face used for signatures of unknown status."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-decryption
  '((t (:background "purple" :foreground "black")))
  "Face used for encryption/decryption status messages."
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(define-button-type 'notmuch-crypto-status-button-type
  'action (lambda (button) (message (button-get button 'help-echo)))
  'follow-link t
  'help-echo "Set notmuch-crypto-process-mime to process cryptographic mime parts."
  :supertype 'notmuch-button-type)

(defun notmuch-crypto-insert-sigstatus-button (sigstatus from)
  (let* ((status (plist-get sigstatus :status))
	 (help-msg nil)
	 (label "Signature not processed")
	 (face 'notmuch-crypto-signature-unknown)
	 (button-action (lambda (button) (message (button-get button 'help-echo)))))
    (cond
     ((string= status "good")
      (let ((fingerprint (concat "0x" (plist-get sigstatus :fingerprint))))
	;; if userid present, userid has full or greater validity
	(if (plist-member sigstatus :userid)
	    (let ((userid (plist-get sigstatus :userid)))
	      (setq label (concat "Good signature by: " userid))
	      (setq face 'notmuch-crypto-signature-good))
	  (progn
	    (setq label (concat "Good signature by key: " fingerprint))
	    (setq face 'notmuch-crypto-signature-good-key)))
	(setq button-action 'notmuch-crypto-sigstatus-good-callback)
	(setq help-msg (concat "Click to list key ID 0x" fingerprint "."))))
     ((string= status "error")
      (let ((keyid (concat "0x" (plist-get sigstatus :keyid))))
	(setq label (concat "Unknown key ID " keyid " or unsupported algorithm"))
	(setq button-action 'notmuch-crypto-sigstatus-error-callback)
	(setq help-msg (concat "Click to retrieve key ID " keyid " from keyserver and redisplay."))))
     ((string= status "bad")
      (let ((keyid (concat "0x" (plist-get sigstatus :keyid))))
	(setq label (concat "Bad signature (claimed key ID " keyid ")"))
	(setq face 'notmuch-crypto-signature-bad)))
     (t
      (setq label (concat "Unknown signature status"
			  (if status (concat ": " status))))))
    (insert-button
     (concat "[ " label " ]")
     :type 'notmuch-crypto-status-button-type
     'help-echo help-msg
     'face face
     'mouse-face face
     'action button-action
     :notmuch-sigstatus sigstatus
     :notmuch-from from)
    (insert "\n")))

(declare-function notmuch-show-refresh-view "notmuch-show" (&optional reset-state))

(defun notmuch-crypto-sigstatus-good-callback (button)
  (let* ((sigstatus (button-get button :notmuch-sigstatus))
	 (fingerprint (concat "0x" (plist-get sigstatus :fingerprint)))
	 (buffer (get-buffer-create "*notmuch-crypto-gpg-out*"))
	 (window (display-buffer buffer t nil)))
    (with-selected-window window
      (with-current-buffer buffer
	(goto-char (point-max))
	(call-process epg-gpg-program nil t t "--list-keys" fingerprint))
      (recenter -1))))

(defun notmuch-crypto-sigstatus-error-callback (button)
  (let* ((sigstatus (button-get button :notmuch-sigstatus))
	 (keyid (concat "0x" (plist-get sigstatus :keyid)))
	 (buffer (get-buffer-create "*notmuch-crypto-gpg-out*"))
	 (window (display-buffer buffer t nil)))
    (with-selected-window window
      (with-current-buffer buffer
	(goto-char (point-max))
	(call-process epg-gpg-program nil t t "--recv-keys" keyid)
	(insert "\n")
	(call-process epg-gpg-program nil t t "--list-keys" keyid))
      (recenter -1))
    (notmuch-show-refresh-view)))

(defun notmuch-crypto-insert-encstatus-button (encstatus)
  (let* ((status (plist-get encstatus :status))
	 (help-msg nil)
	 (label "Decryption not attempted")
	 (face 'notmuch-crypto-decryption))
    (cond
     ((string= status "good")
      (setq label "Decryption successful"))
     ((string= status "bad")
      (setq label "Decryption error"))
     (t
      (setq label (concat "Unknown encryption status"
			  (if status (concat ": " status))))))
    (insert-button
     (concat "[ " label " ]")
     :type 'notmuch-crypto-status-button-type
     'help-echo help-msg
     'face face
     'mouse-face face)
    (insert "\n")))

;;

(provide 'notmuch-crypto)

;;; notmuch-crypto.el ends here

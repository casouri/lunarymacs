;;; notmuch-print.el --- printing messages from notmuch.
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

(require 'notmuch-lib)

(declare-function notmuch-show-get-prop "notmuch-show" (prop &optional props))

(defcustom notmuch-print-mechanism 'notmuch-print-lpr
  "How should printing be done?"
  :group 'notmuch-show
  :type '(choice
	  (function :tag "Use lpr" notmuch-print-lpr)
	  (function :tag "Use ps-print" notmuch-print-ps-print)
	  (function :tag "Use ps-print then evince" notmuch-print-ps-print/evince)
	  (function :tag "Use muttprint" notmuch-print-muttprint)
	  (function :tag "Use muttprint then evince" notmuch-print-muttprint/evince)
	  (function :tag "Using a custom function")))

;; Utility functions:

(defun notmuch-print-run-evince (file)
  "View FILE using 'evince'."
  (start-process "evince" nil "evince" file))

(defun notmuch-print-run-muttprint (&optional output)
  "Pass the contents of the current buffer to 'muttprint'.

Optional OUTPUT allows passing a list of flags to muttprint."
  (apply #'call-process-region (point-min) (point-max)
	 ;; Reads from stdin.
	 "muttprint"
	 nil nil nil
	 ;; Show the tags.
	 "--printed-headers" "Date_To_From_CC_Newsgroups_*Subject*_/Tags/"
	 output))

;; User-visible functions:

(defun notmuch-print-lpr (msg)
  "Print a message buffer using lpr."
  (lpr-buffer))

(defun notmuch-print-ps-print (msg)
  "Print a message buffer using the ps-print package."
  (let ((subject (notmuch-prettify-subject
		  (plist-get (notmuch-show-get-prop :headers msg) :Subject))))
    (rename-buffer subject t)
    (ps-print-buffer)))

(defun notmuch-print-ps-print/evince (msg)
  "Preview a message buffer using ps-print and evince."
  (let ((ps-file (make-temp-file "notmuch"))
	(subject (notmuch-prettify-subject
		  (plist-get (notmuch-show-get-prop :headers msg) :Subject))))
    (rename-buffer subject t)
    (ps-print-buffer ps-file)
    (notmuch-print-run-evince ps-file)))

(defun notmuch-print-muttprint (msg)
  "Print a message using muttprint."
  (notmuch-print-run-muttprint))

(defun notmuch-print-muttprint/evince (msg)
  "Preview a message buffer using muttprint and evince."
  (let ((ps-file (make-temp-file "notmuch")))
    (notmuch-print-run-muttprint (list "--printer" (concat "TO_FILE:" ps-file)))
    (notmuch-print-run-evince ps-file)))

(defun notmuch-print-message (msg)
  "Print a message using the user-selected mechanism."
  (set-buffer-modified-p nil)
  (funcall notmuch-print-mechanism msg))

(provide 'notmuch-print)

;;; notmuch-print.el ends here

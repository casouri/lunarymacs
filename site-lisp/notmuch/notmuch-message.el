;;; notmuch-message.el --- message-mode functions specific to notmuch
;;
;; Copyright © Jesse Rosenthal
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
;; Authors: Jesse Rosenthal <jrosenthal@jhu.edu>

;;; Code:

(require 'message)
(require 'notmuch-tag)
(require 'notmuch-mua)

(defcustom notmuch-message-replied-tags '("+replied")
  "List of tag changes to apply to a message when it has been replied to.

Tags starting with \"+\" (or not starting with either \"+\" or
\"-\") in the list will be added, and tags starting with \"-\"
will be removed from the message being replied to.

For example, if you wanted to add a \"replied\" tag and remove
the \"inbox\" and \"todo\" tags, you would set:
    (\"+replied\" \"-inbox\" \"-todo\"\)"
  :type '(repeat string)
  :group 'notmuch-send)

(defun notmuch-message-mark-replied ()
  ;; get the in-reply-to header and parse it for the message id.
  (let ((rep (mail-header-parse-addresses (message-field-value "In-Reply-To"))))
    (when (and notmuch-message-replied-tags rep)
      (notmuch-tag (notmuch-id-to-query (car (car rep)))
	       (notmuch-tag-change-list notmuch-message-replied-tags)))))

(add-hook 'message-send-hook 'notmuch-message-mark-replied)

(provide 'notmuch-message)

;;; notmuch-message.el ends here

;; make-deps.el --- compute make dependencies for Elisp sources
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

;;; Code:

(defun batch-make-deps ()
  "Invoke `make-deps' for each file on the command line."

  (setq debug-on-error t)
  (dolist (file command-line-args-left)
    (let ((default-directory command-line-default-directory))
      (find-file-literally file))
    (make-deps command-line-default-directory))
  (kill-emacs))

(defun make-deps (&optional dir)
  "Print make dependencies for the current buffer.

This prints make dependencies to `standard-output' based on the
top-level `require' expressions in the current buffer.  Paths in
rules will be given relative to DIR, or `default-directory'."

  (setq dir (or dir default-directory))
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
	(while t
	  (let ((form (read (current-buffer))))
	    ;; Is it a (require 'x) form?
	    (when (and (listp form) (= (length form) 2)
		       (eq (car form) 'require)
		       (listp (cadr form)) (= (length (cadr form)) 2)
		       (eq (car (cadr form)) 'quote)
		       (symbolp (cadr (cadr form))))
	      ;; Find the required library
	      (let* ((name (cadr (cadr form)))
		     (fname (locate-library (symbol-name name))))
		;; Is this file and the library in the same directory?
		;; If not, assume it's a system library and don't
		;; bother depending on it.
		(when (and fname
			   (string= (file-name-directory (buffer-file-name))
				    (file-name-directory fname)))
		  ;; Print the dependency
		  (princ (format "%s.elc: %s.elc\n"
				 (file-name-sans-extension
				  (file-relative-name (buffer-file-name) dir))
				 (file-name-sans-extension
				  (file-relative-name fname dir)))))))))
      (end-of-file nil))))

;;; make-deps.el ends here

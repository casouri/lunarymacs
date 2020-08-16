;;; recentf-ext.el --- Recentf extensions

;; $Id: recentf-ext.el,v 1.4 2013/01/30 21:47:11 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; Keywords: convenience, files
;; Package-Version: 20170926.35
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/recentf-ext.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Extension of `recentf' package.
;;
;; * `dired' buffers can be handled.
;; * Switching to file buffer considers it as most recent file.

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put recentf-ext.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'recentf-ext)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET recentf-ext RET
;;


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x recentf-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of recentf-ext.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "recentf-ext.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x recentf-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; - Add directory tracking - Yuan, 2020/7/22
;; - remove (eval-when-compile (require 'cl)), turns out it’s not used - Yuan, 2020/1/17
;; - remove (recentf-mode 1) - Yuan, 2020/1/17
;; $Log: recentf-ext.el,v $
;; Revision 1.4  2013/01/30 21:47:11  rubikitch
;; fix header
;;
;; Revision 1.3  2010/05/04 09:06:55  rubikitch
;; Added bug report command
;;
;; Revision 1.2  2010/01/14 21:52:16  rubikitch
;; `recentf-add-dired-directory': Fix an error when `dired-directory' is not a directory name.
;;
;; Revision 1.1  2009/12/24 11:53:03  rubikitch
;; Initial revision
;;

;;; Code:

(defvar recentf-ext-version "$Id: recentf-ext.el,v 1.4 2013/01/30 21:47:11 rubikitch Exp $")
(defgroup recentf-ext nil
  "recentf-ext"
  :group 'emacs)
(require 'recentf)

;;; [2020/08/16] (@ "Add convenient functions")
(defun recentf-add-direcoty-files (directory)
  "Add files in DIRECTORY to recentf list."
  (interactive "DDirectory: ")
  (dolist (path (directory-files-recursively
                 directory (rx (seq bol (not "."))) t t))
    (recentf-add-file path)
    (recentf-save-list)))

;;; [2020/07/22] (@ "Also add file’s directory")
(defun recentf-push-buffer-directory ()
  (when default-directory
    (recentf-add-file default-directory))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)
(add-hook 'find-file-hook #'recentf-push-buffer-directory)
(add-hook 'write-file-functions #'recentf-push-buffer-directory)

;;; [2009/03/01] (@* "`recentf' as most recently USED files")
(defun recentf-push-buffers-in-frame ()
  (walk-windows
   (lambda (win)
     (let ((bfn (buffer-local-value 'buffer-file-name (window-buffer win))))
       (and bfn (recentf-add-file bfn))))))
(add-to-list 'window-configuration-change-hook 'recentf-push-buffers-in-frame)

;;; [2009/12/24] (@* "`recentf' directory")
(defun recentf-add-dired-directory ()
  (when (and (stringp dired-directory)
             (equal "" (file-name-nondirectory dired-directory)))
    (recentf-add-file dired-directory)))
(add-hook 'dired-mode-hook 'recentf-add-dired-directory)

;;;; Bug report
(defvar recentf-ext-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar recentf-ext-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of recentf-ext.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"recentf-ext.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun recentf-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   recentf-maintainer-mail-address
   "recentf-ext.el"
   (apropos-internal "^recentf-" 'boundp)
   nil nil
   recentf-bug-report-salutation))

(provide 'recentf-ext)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "recentf-ext.el")
;;; recentf-ext.el ends here

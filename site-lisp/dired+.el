;;; dired+.el --- Dired goodies      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package provides some convenient commands:
;;
;; - dired-toggle-mark-click: bind this to <s-mouse-1>, and you can
;;   s-click a file and toggle its mark.
;; - dired-copy: copy marked files to clipboard.
;; - dired-paste: paste files in the clipboard to the current directory.
;; - dired-move: move files in the clipboard to the current directory.
;;
;; Copy/paste/move also supports tramp.

;;; Code:
;;

(defun dired-marked-p ()
  "Return t if the file at point is marked, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (eq (char-after) ?*)))

(defun dired-toggle-mark ()
  "Toggle mark of file at point."
  (interactive)
  (if (dired-marked-p)
      (dired-unmark 1)
    (dired-mark 1)))

(defun dired-toggle-mark-click (event)
  "Toggle mark of a file by mouse click.
EVENT is the mouse click event."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (dired-toggle-mark))

(defvar dired-clipboard nil
  "A list of files “copied” in dired mode.")

(defun dired-copy ()
  "Copy marked files in dired."
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (user-error "Not in dired mode")
    (if-let ((files (dired-get-marked-files)))
        (progn (setq dired-clipboard files)
               (dired-unmark-all-marks))
      (user-error "Nothing selected"))))

(defun dired-paste-1 (file-list new-directory)
  "Copy files in FILE-LIST to NEW-DIRECTORY."
  (dolist (file file-list)
    (if (file-directory-p file)
        (copy-directory file new-directory)
      (copy-file file new-directory 1 nil t t))))

(defun dired-paste ()
  "Paste copied files into this dired buffer."
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (user-error "Not in dired mode")
    (if (not dired-clipboard)
        (user-error "Nothing copied")
      (dired-paste-1 dired-clipboard default-directory))))

(defun dired-move ()
  "Move copied files into this dired buffer."
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (user-error "Not in dired mode")
    (if (not dired-clipboard)
        (user-error "Nothing copied")
      (when (y-or-n-p "Move files to this directory? ")
        ;; Separate copying and trashing in case trashing goes
        ;; wrong.
        (dired-paste-1 dired-clipboard default-directory)
        (dolist (file dired-clipboard)
          (move-file-to-trash file))))))

(provide 'dired+)

;;; dired+.el ends here

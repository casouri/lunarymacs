;;; mcdired.el --- Multi-column Dired      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'cl-lib)

(defvar mcdired-column-width 25
  "Width of a column in characters.")

(defvar mcdired--placeholder-buffer "*mcdired placeholder*"
  "Placeholder buffer for mcdired.")

(defvar-local mcdired--column-config nil
  "We store the column configuration in the first column.
This variable records how many columns are there, and directories
they display.")

;;; Helpers

(defun mcdired--window-children (&optional window)
  "Return a list of windows that are children of WINDOW.
WINDOW defaults to the selected one. Return nil if none."
  (let ((win (window-child (or window (selected-window))))
        children)
    (while (and win (window-live-p win))
      (push win children)
      (setq win (window-next-sibling win)))
    (reverse children)))

(defun mcdired--window-siblings (&optional window)
  "Return a list of windows that are mcdired siblings to WINDOW.
WINDOW defaults to the selected one. Assumes WINDOW is a mcdired
window."
  (let ((id (window-parameter nil 'mcdired)))
    (if (not id)
        (error "Cannot find mcdired id")
      (cl-remove-if-not
       (lambda (win) (eq id (window-parameter win 'mcdired)))
       (mcdired--window-children (window-parent window))))))

(defun mcdired--first-column-p (&optional window)
  "Return t if WINDOW is the first column."
  (eq (or window (selected-window))
      (car (mcdired--window-siblings window))))

(defun mcdired--last-column-p (&optional window)
  "Return t if WINDOW is the last column."
  (eq (or window (selected-window))
      (car (last (mcdired--window-siblings window)))))

(defun mcdired--record-column-config ()
  "Record column configuration.
Assumes we are in a good column configuration and one of the
column is selected."
  (let* ((column-list (mcdired--window-siblings))
         (master (car column-list)))
    (with-current-buffer (window-buffer master)
      (setq mcdired--column-config
            (mapcar #'window-buffer column-list)))))

(defun mcdired--master-column-p (&optional window)
  "Return t if WINDOW is the master column."
  ;; Only the master column has this variable set.
  (if (buffer-local-value 'mcdired--column-config
                          (window-buffer window))
      t nil))

(defun mcdired ()
  (interactive)
  (save-excursion
    (let* ((window-width (window-width))
           (column-number (/ window-width mcdired-column-width))
           (column-width (/ window-width column-number))
           (id (gensym))
           (current-window (selected-window)))
      ;; Split windows
      (set-window-parameter current-window 'mcdired id)
      (dotimes (_ (1- column-number))
        (setq current-window
              (display-buffer-in-atom-window
               (get-buffer-create mcdired--placeholder-buffer)
               `((side . right)
                 (window . ,current-window)
                 (window-width . ,(- (window-width) column-width)))))
        ;; Make all columns except for the first one dedicated.
        (set-window-dedicated-p current-window t)
        (set-window-parameter current-window 'mcdired id)))
    (mcdired--record-column-config)))

(defun mcdired-open ()
  "Open the directory at point in a new column."
  (interactive)
  (let ((file (dired-file-name-at-point)))
    (if (file-directory-p file)
        (if (mcdired--last-column-p)
            (progn (mcdired--shift-down)
                   (find-file file))
          (select-window (window-next-sibling))
          (find-file file))
      (find-file-other-window file))))

(defun mcdired--shift-down (&optional window)
  "Shift everything down one directory.
The last column displays the placeholder buffer. WINDOW is a
column. Don’t use this function when the last column is already
displaying the placeholder."
  (let ((column-list (mcdired--window-siblings window)))
    (dolist (column column-list)
      (if-let ((next-column (window-next-sibling column)))
          (set-window-buffer nil (window-buffer next-column))
        (set-window-buffer
         nil (get-buffer-create mcdired--placeholder-buffer))))))

(defun mcdired--shift-up (&optional window)
  "Shift everything up one directory.
The first column displays the parent of the directory before
shift. WINDOW is a column. Don’t use this function when the first
column displays the root directory."
  (let ((column-list (mcdired--window-siblings window)))
    (dolist (column column-list)
      (with-selected-window column
        (let ((parent-dir
               (file-name-directory
                (directory-file-name
                 (expand-file-name default-directory)))))
          (find-file parent-dir))))))

(provide 'mcdired)



;;; mcdired.el ends here

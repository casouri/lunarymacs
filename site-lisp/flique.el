;;; flique.el --- File cliques  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package complements bklink.el. With this package, you can
;; easily group related note files into cliques and go back and
;; forth within the clique.
;;
;; To start, create a file "index.flique" under the directory where
;; all the notes are. Each line is a filename. Consecutive lines define
;; a clique. Separate cliques by empty lines. For example,
;;
;; file1.txt
;; file2.txt
;;
;; file3.txt
;; file4.txt
;;
;; defines two cliques, file1 and 2, and file3 and 4.
;;
;; Then, you can go back and forth by
;;
;;  - ‘flique-forward’
;;  - ‘flique-backward’
;;
;; If a line in the index file contains a file that doesn’t exist,
;; flique just ignores that.

;;; Code:

(defun flique--index ()
  "Return the index buffer."
  (with-current-buffer (get-buffer-create " *flique index*")
    (when (file-exists-p "index.flique")
      (insert-file-contents "index.flique" nil nil nil t))
    (current-buffer)))

(defun flique-append-to-index (file)
  "Add FILE to the end of the index file.
Only add if FILE is not already in the index file."
  (with-current-buffer (flique--index)
    (goto-char (point-min))
    ;; Unless already exists...
    (unless (re-search-forward
             (concat "^" (regexp-quote
                          (file-name-nondirectory file))
                     "$")
             nil t)
      ;; ...append to the end.
      (goto-char (point-max))
      (unless (looking-back "\n" 1)
        (insert "\n"))
      (insert (file-name-nondirectory file))
      ;; If we use ‘write-file’, this buffer is associated with
      ;; index.flique. (Changes from *flique index* to index.flique.)
      (write-region (point-min) (point-max) "index.flique"))))

(defun flique--next-file (filename step)
  "Return the next file of FILENAME. FILENAME cannot be a path.
STEP can be either 1 or -1, 1 for going forward, -1 for going
backward."
  (with-current-buffer (flique--index)
    (goto-char (point-min))
    (when (and (re-search-forward
                (concat "^" (regexp-quote filename) "$") nil t)
               (eq 0 (forward-line step)))
      (let ((next (buffer-substring
                   (line-beginning-position)
                   (line-end-position))))
        (unless (equal next "")
          next)))))

(defun flique-forward ()
  "Go forward in the clique."
  (interactive)
  (let ((next (flique--next-file
               (file-name-nondirectory (buffer-file-name))
               1)))
    (when (and next (file-exists-p next))
      (find-file next)
      (flique-show-navigation))))

(defun flique-backward ()
  "Go backward in the clique."
  (interactive)
  (let ((next (flique--next-file
               (file-name-nondirectory (buffer-file-name))
               -1)))
    (when (and next (file-exists-p next))
      (find-file next)
      (flique-show-navigation))))

(defun flique-show-navigation ()
  "Display a navigation bar in header-line."
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (prev (flique--next-file filename -1))
         (next (flique--next-file filename 1)))
    (when (or prev next)
      (setq header-line-format
            (propertize
             (concat
              (if (and prev (file-exists-p prev))
                  (concat "← " (file-name-base prev))
                ":-O")
              (propertize "\t" 'display `(space :width ,tab-width))
              (if (and next (file-exists-p next))
                  (concat (file-name-base next) " →")
                ":-D")
              (propertize " " 'display '(raise 0.3))
              (propertize " " 'display '(raise -0.2)))
             'face '(:height 130))))))

(defun flique-list ()
  "Show the current file in the flique index file."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (find-file "index.flique")
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote filename) "$") nil t)
    (beginning-of-line)))

(define-derived-mode flique-mode fundamental-mode
  "Flique"
  "Editing flique index files."
  (setq outline-regexp (rx "*"))
  (outline-minor-mode))

(add-to-list 'auto-mode-alist '("\\.flique\\'" . flique-mode))

(provide 'flique)

;;; flique.el ends here

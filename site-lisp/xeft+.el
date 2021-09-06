;;; xeft+.el --- Xeft enhancement for zettelkasten  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Some utility for using Xeft as a zettelkasten tool. This is
;; entirely use to collect some related notes in a linear order, so I
;; can move backward and forward between them. The nesting is purely
;; for inserting new notes in between any two. Child/parent
;; relationship don’t bear any meaning. Connecting and structuring
;; notes is done by bklink.

;;; Code:

(defun xeft+-new-file (_)
  "Return a new filename."
  (let ((count
         (length
          (cl-remove-if-not
           (lambda (path)
             (let ((name (file-name-base path)))
               (string-match-p
                (rx "x" (+ digit) (? ".txt")) name)))
           (xeft--file-list)))))
    (format "x%d.txt" (1+ count))))

(defun xeft+-lex-step (step n &optional idx)
  "Increment N by 1 lexical graphically. IDX is for internal use.
STEP’s positivist determines step direction. Return an empty
string if reaches “zero”."
  (let* ((lst (mapcar #'identity n))
         (idx (or idx (1- (length lst))))
         max min carry)
    (if (string-match-p "^[[:digit:]]+$" n)
        (setq max ?9 min ?0 carry ?1)
      (setq max ?z min ?a carry ?a))
    (let ((digit (nth idx lst)))
      (if (> step 0)
          (if (< digit max)
              (progn (setf (nth idx lst) (1+ digit))
                     (concat lst))
            (setf (nth idx lst) min)
            (if (eq idx 0)
                (concat (cons carry lst))
              (xeft+-lex-step step (concat lst) (1- idx))))
        (if (> digit min)
            (progn (setf (nth idx lst) (1- digit))
                   (string-trim-left (concat lst) "0"))
          (setf (nth idx lst) max)
          (if (eq idx 0)
              ""
            (xeft+-lex-step step (concat lst) (1- idx))))))))

(defun xeft+-lex-forward (n)
  "Return the next value for N, e.g., a11 -> a12."
  (if (or (string-match (rx (seq (+ digit) eol)) n)
          (string-match (rx (seq (+ (any "a-z")) eol)) n))
      (concat (substring n 0 (match-beginning 0))
              (xeft+-lex-step 1 (match-string 0 n)))))

(defun xeft+-lex-back (n)
  "Return the previous value for N, e.g., 11c -> 11b."
  (if (or (string-match (rx (seq (+ digit) eol)) n)
          (string-match (rx (seq (+ (any "a-z")) eol)) n))
      (concat (substring n 0 (match-beginning 0))
              (xeft+-lex-step -1 (match-string 0 n)))))

(defun xeft+-lex-down (n)
  "Return the value to insert under N, e.g., 11 -> 11a."
  (if (string-match (rx (seq (+ digit) eol)) n)
      (concat n "a")
    (concat n "1")))

(defun xeft+-lex-up (n)
  "Return the value above N, e.g., 1a -> 1."
  (if (or (string-match (rx (seq (+ digit) eol)) n)
          (string-match (rx (seq (+ (any "a-z")) eol)) n))
      (replace-match "" nil nil n)))

(defun xeft+-nagivate (filename)
  "Return (PREV NEXT DOWN).
PREV is the previous file of FILENAME in the series,
NEXT and DOWN are the possible names for the next file.
Return nil if FILENAME is not alphanumeric.

FILENAME can be a path, this function only takes the basename."
  (let* ((basename (file-name-base filename))
         (ext (file-name-extension filename))
         (stem (and (string-match
                     (rx bol "x" (group (+ (any "0-9a-z")))
                         "." (+ anychar) eol)
                     basename)
                    (match-string 1 basename))))
    (when stem
      (list
       ;; PREV.
       (format "x%s%s"
               (xeft+-lex-back stem)
               (if ext (concat "." ext) ""))
       ;; NEXT.
       (format "x%s%s"
               (xeft+-lex-forward stem)
               (if ext (concat "." ext) ""))
       ;; DOWN.
       (format "x%s%s"
               (xeft+-lex-down stem)
               (if ext (concat "." ext) ""))
       ;; UP.
       (format "x%s%s"
               (xeft+-lex-up stem)
               (if ext (concat "." ext) ""))))))

(defun xeft+-find-next (filename &optional next-only)
  "Find the next file of FILENAME.
The next file can be the one after this file, or below it, or the
one after the one above it.

If NEXT-ONLY non-nil, don’t consider to one below this file."
  (pcase-let ((`(,_ ,next ,down ,up) (xeft+-nagivate filename)))
    (cond ((and (not next-only) (file-exists-p down)) down)
          ((file-exists-p next) next)
          (t (xeft+-find-next up t)))))

(defun xeft+-jump-forward ()
  "Go to the next file, if exists."
  (interactive)
  (if-let ((next (xeft+-find-next (buffer-file-name))))
      (progn
        (find-file next)
        (run-hooks 'xeft-find-file-hook)
        (message "Jump forward"))
    (message "Cannot find the next file")))

(defun xeft+-jump-backward ()
  "Go to the previous file, if exists."
  (interactive)
  (pcase-let ((`(,prev ,_ ,_ ,_) (xeft+-nagivate (buffer-file-name))))
    (if (or (not prev) (not (file-exists-p prev)))
        (message "Cannot find the previous file")
      (find-file prev)
      (run-hooks 'xeft-find-file-hook)
      (message "Jump backward"))))

(defun xeft+-show-nagivation ()
  "Show prev/next file in header line."
  (pcase-let* ((`(,prev ,next ,down) (xeft+-nagivate (buffer-file-name)))
               (title-list
                (cl-loop
                 for file in (list prev next down)
                 collect (when (and file (file-exists-p file))
                           (with-temp-buffer
                             (insert-file-contents file)
                             (goto-char (point-min))
                             (buffer-substring
                              (point) (line-end-position)))))))
    (when (or prev next down)
      (setq header-line-format
            (propertize
             (concat
              "← "
              (or (car title-list) "BEG")
              (propertize "\t" 'display `(space :width ,tab-width))
              (or (nth 1 title-list) (nth 2 title-list) "END")
              " →"
              (propertize " " 'display '(raise 0.3))
              (propertize " " 'display '(raise -0.2)))
             'face '(:height 130))))))

(defun xeft+-collect-kin (file &optional sibling-only)
  "Collect FILE’s kin.
A file kin includes its sibling and children in the numbering tree.
If SIBLING-ONLY non-nil, do not collect children."
  (pcase-let ((`(,_ ,next ,down) (xeft+-nagivate file))
              (filename (file-name-nondirectory file)))
    (if (file-exists-p file)
        (append (list filename)
                (if next (xeft+-collect-kin next sibling-only))
                (if (and (not sibling-only) down)
                    (xeft+-collect-kin down))))))

(provide 'xeft+)

;;; xeft+.el ends here

;;; expreg.el --- Simple expand region  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This is just like expand-region, but simpler and easier to debug.
;; Bind ‘expreg-expand’ and ‘expreg-contract’ and start using it.

;;; Developer
;;
;; It works roughly as follows: ‘expreg-expand’ collects a list of
;; possible expansions on startup with functions in
;; ‘expreg-functions’. Then it sorts them by each region’s size. It
;; also removes duplicates, etc. Then this list is stored in
;; ‘expreg--next-regions’.
;;
;; To expand, we pop a region from ‘expreg--next-regions’, set point
;; and mark accordingly, and push this region to
;; ‘expreg--prev-regions’. So the head of ‘expreg--prev-regions’
;; should always equal the current region.
;;
;; ‘expreg-contract’ does just the opposite: it pops a region from
;; ‘expreg--prev-regions’, push it to ‘expreg--next-regions’, and set
;; the current region to the head of ‘expreg--prev-regions’.
;;
;; For better debugability, each region is of the form
;;
;;     (FN . (BEG . END))
;;
;; where FN is the function produced this region. So accessing BEG is
;; ‘cadr’, accessing END is ‘cddr’.
;;
;; Credit: I stole a lot of ideas on how to expand lists and strings
;; from ‘expand-region’ :-)

;;; Code:

(require 'subword)
(require 'treesit)
(require 'cl-lib)

;;; Cutom options and variables

(defvar-local expreg-functions
    '( expreg--word expreg--list expreg--string expreg--treesit
       expreg--comment expreg--paragraph)
  "A list of expansion functions.
Each function is called with no arguments and should return a
list of (BEG . END). The list don’t have to be sorted, and can
have duplicates. It’s also fine to include invalid regions, such
as ones where BEG equals END, etc, these will be filtered out by
‘expreg-expand’.")

;;; Helper functions

(defun expreg--sort-regions (regions)
  "Sort REGIONS by their span."
  (cl-sort regions (lambda (a b)
                     (< (- (cddr a) (cadr a))
                        (- (cddr b) (cadr b))))))

(defvar expreg--validation-white-list '(list-at-point)
  "Regions produced by functions in this list skips filtering.")

(defun expreg--valid-p (region orig)
  "Return non-nil if REGION = (BEG . END) valid regarding ORIG.
ORIG is the current position."
  (let ((producer (car region))
        (beg (cadr region))
        (end (cddr region)))
    (or (memq producer expreg--validation-white-list)
        (and (<= beg orig end)
             (< beg end)
             ;; We don’t filter out regions that’s only one character
             ;; long, because there are useful regions of that size.
             ;; Consider ‘c-ts-mode--looking-at-star’, the "c" is one
             ;; character long but we don’t want to skip it: my muscle
             ;; remembers to hit C-= twice to mark a symbol, skipping "c"
             ;; messes that up.
             ;; (< 1 (- end beg) 8000)

             ;; If the region is only one character long, and the
             ;; character is stuff like bracket, escape char, quote, etc,
             ;; filter it out. This is usually returned by
             ;; ‘expreg--treesit’.
             (not (and (eq (- end beg) 1)
                       (not (memq (char-syntax (char-after beg))
                                  '(?- ?w ?_)))))))))

;;; Expand/contract

(defvar-local expreg--verbose nil
  "If t, print debugging information.")

(defvar-local expreg--next-regions nil
  "The regions we are going to expand to.
This should be a list of (BEG . END).")

(defvar-local expreg--prev-regions nil
  "The regions we’ve expanded past.
This should be a list of (BEG . END).")

(defun expreg-expand ()
  "Expand region."
  (interactive)
  ;; Checking for last-command isn’t strictly necessary, but nice to
  ;; have.
  (when (not (and (region-active-p)
                  (eq (region-beginning)
                      (cadr (car expreg--prev-regions)))
                  (eq (region-end)
                      (cddr (car expreg--prev-regions)))))
    (setq-local expreg--next-regions nil)
    (setq-local expreg--prev-regions nil))
  (when (and (null expreg--next-regions)
             (null expreg--prev-regions))
    (let* ((orig (point))
           (regions (mapcan #'funcall expreg-functions))
           (regions (cl-remove-if-not
                     (lambda (region)
                       (expreg--valid-p region orig))
                     regions))
           (regions (expreg--sort-regions regions))
           (regions (cl-remove-duplicates regions :test #'equal)))
      (setq-local expreg--next-regions regions)))
  ;; Go past all the regions that are smaller than the current region,
  ;; if region is active.
  (when (region-active-p)
    (while (and expreg--next-regions
                (let ((beg (cadr (car expreg--next-regions)))
                      (end (cddr (car expreg--next-regions))))
                  (and (<= (region-beginning) beg)
                       (<= end (region-end)))))
      ;; Pop from next-regions, push into prev-regions.
      (push (pop expreg--next-regions)
            expreg--prev-regions)))
  (when expreg--next-regions
    (let ((region (pop expreg--next-regions)))
      (set-mark (cddr region))
      (goto-char (cadr region))
      (push region expreg--prev-regions)))
  (when expreg--verbose
    (message "blame: %s\nnext: %S\nprev: %S"
             (caar expreg--prev-regions)
             expreg--next-regions expreg--prev-regions)))

(defun expreg-contract ()
  "Contract region."
  (interactive)
  (when (and (region-active-p)
             (> (length expreg--prev-regions) 1))
    (push (pop expreg--prev-regions) expreg--next-regions)
    (set-mark (cddr (car expreg--prev-regions)))
    (goto-char (cadr (car expreg--prev-regions))))
  (when expreg--verbose
    (message "next: %S\nprev: %S"
             expreg--next-regions expreg--prev-regions)))

;;; Expansion functions

(defun expreg--word ()
  "Return a list of regions within the word at point."
  ;; - subwords in camel-case.
  ;; - subwords by “-” or “_”.
  ;; - symbol-at-point
  ;; - within whitespace & paren/quote (but can contain punctuation)
  ;;   (“10–20”, “1.2”, “1,2”, etc). (This is technically not always
  ;;   within a word anymore...)
  (save-excursion
    (let ((orig (point))
          result
          beg end)
      ;; (1) subwords in camel-case.
      (subword-forward)
      (setq end (point))
      (subword-backward)
      (setq beg (point))
      (skip-syntax-forward "w")
      ;; Make sure we stay in the word boundary.
      ;; ‘subword-backward/forward’ could go through parenthesis, etc.
      (when (eq (point) end)
        (push `(word . ,(cons beg end)) result))

      ;; (2) subwords by “-” or “_”.
      (goto-char orig)
      (skip-syntax-forward "w")
      (setq end (point))
      (skip-syntax-backward "w")
      (setq beg (point))
      (push `(word . ,(cons beg end)) result)

      ;; (3) symbol-at-point
      (goto-char orig)
      (skip-syntax-forward "w_")
      (setq end (point))
      (skip-syntax-backward "w_")
      (setq beg (point))
      (push `(word . ,(cons beg end)) result)

      ;; (4) within whitespace & paren. (Allow word constituents, symbol
      ;; constituents, punctuation.)
      (goto-char orig)
      (skip-syntax-forward "w_.")
      (setq end (point))
      (skip-syntax-backward "w_.")
      (setq beg (point))
      (push `(word . ,(cons beg end)) result)
      (goto-char orig)
      ;; Return!
      result)))

(defun expreg--treesit ()
  "Return a list of regions according to tree-sitter."
  (when (treesit-parser-list)
    (let ((node (treesit-node-at
                 (point) (treesit-language-at (point))))
          (root (treesit-buffer-root-node
                 (treesit-language-at (point))))
          result)
      (while node
        (let ((beg (treesit-node-start node))
              (end (treesit-node-end node)))
          (when (not (treesit-node-eq node root))
            (push `(treesit . ,(cons beg end)) result)))
        (setq node (treesit-node-parent node)))
      result)))

(defun expreg--inside-list ()
  "Return a list of one region marking inside the list, or nil.
Assumes point not in string."
  (condition-case nil
      (progn
        ;; Inside a string? Move out of it first.
        (when (nth 3 (syntax-ppss))
          (goto-char (nth 8 (syntax-ppss))))
        (when (> (car (syntax-ppss)) 0)
          (let (beg)
            (save-excursion
              (goto-char (nth 1 (syntax-ppss)))
              (save-excursion
                (forward-char)
                (skip-syntax-forward "-")
                (setq beg (point)))
              (forward-list)
              (backward-char)
              (skip-syntax-backward "-")
              (list `(inside-list . ,(cons beg (point))))))))
    (scan-error nil)))

(defun expreg--list-at-point ()
  "Return a list of one region marking the list at point, or nil.
Point should be at the beginning or end of a list."
  (unless (nth 3 (syntax-ppss))
    (save-excursion
      ;; Even if point is not at the beginning of a list, but before a
      ;; list (with only spaces between), we want to return a region
      ;; covering that list after point, for convenience. But because
      ;; this region will not cover point, it will not pass the
      ;; filtering, so this function needs to be added to
      ;; ‘expreg--validation-white-list’.
      (when (and (looking-at (rx (syntax whitespace)))
                 (not (looking-back ")" 1)))
        (skip-syntax-forward "-"))
      ;; If at the end of a list and not the beginning of another one,
      ;; move to the beginning of the list.
      ;; Corresponding char for each int: 40=(, 39=', 41=).
      (when (and (eq (char-syntax (or (char-before) ?x)) 41)
                 (not (memq (char-syntax (or (char-after) ?x)) '(39 40))))
        (ignore-errors (backward-list 1)))
      (when (memq (char-syntax (or (char-after) ?x)) '(39 40))
        (save-excursion
          (condition-case nil
              (let ((beg (if (eq (char-syntax (or (char-before) ?x)) 39)
                             (1- (point))
                           (point))))
                (forward-list)
                (list `(list-at-point . ,(cons beg (point)))))
            (scan-error nil)))))))

(defun expreg--outside-list ()
  "Return a list of one region marking outside the list, or nil.
If find something, leave point at the beginning of the list."
  (let (beg end)
    (condition-case nil
        (when (> (car (syntax-ppss)) 0)
          (save-excursion
            ;; If point inside a list but not at the beginning of one,
            ;; move to the beginning of enclosing list.
            (when (> (car (syntax-ppss)) 0)
              (goto-char (nth 1 (syntax-ppss))))
            (setq beg (point))
            (forward-list)
            (setq end (point)))
          (when (and beg end)
            (goto-char beg)
            (list `(outside-list . ,(cons beg end)))))
      (scan-error nil))))

(defun expreg--string ()
  "Return regions marking the inside and outside of the string."
  (let ( outside-beg outside-end
         inside-beg inside-end)
    (condition-case nil
        (save-excursion
          (if (nth 3 (syntax-ppss))
              ;; Inside a string? Move to beginning.
              (goto-char (nth 8 (syntax-ppss)))
            ;; Not inside a string, but at the end of a string and not at
            ;; the beginning of another one? Move to beginning.
            (when (and (eq (char-syntax (or (char-before) ?x)) 34)
                       (not (eq (char-syntax (or (char-after) ?x)) 34)))
              (backward-sexp)))
          ;; Not inside a string and at the beginning of one.
          (when (and (not (nth 3 (syntax-ppss)))
                     (eq (char-syntax (or (char-after) ?x)) 34))
            (setq outside-beg (point))
            (forward-sexp)
            (when (eq (char-syntax (or (char-before) ?x)) 34)
              (setq outside-end (point))
              (backward-char)
              (setq inside-end (point))
              (goto-char outside-beg)
              (forward-char)
              (setq inside-beg (point))
              ;; It’s ok if point is at outside string and we return a
              ;; region marking inside the string: expreg will filter the
              ;; inside one out.
              (list `(string . ,(cons outside-beg outside-end))
                    `(string . ,(cons inside-beg inside-end))))))
      (scan-error nil))))

(defun expreg--list ()
  "Return a list of regions determined by sexp level."
  (save-excursion
    (let ((inside-list (expreg--inside-list))
          (list-at-point (expreg--list-at-point))
          outside-list lst)
      (while (setq lst (expreg--outside-list))
        (setq outside-list
              (append outside-list lst)))
      (append list-at-point inside-list outside-list))))

(defun expreg--comment ()
  "Return a list of regions containing comment."
  (save-excursion
    (let ((orig (point))
          (beg (point))
          (end (point))
          result forward-succeeded)
      ;; Go backward to the beginning of a comment (if exists).
      (while (nth 4 (syntax-ppss))
        (backward-char))
      ;; Now we are either at the beginning of a comment, or not on a
      ;; comment at all. (When there are multiple lines of comment,
      ;; each line is an individual comment.)
      (while (forward-comment 1)
        (setq end (point))
        (setq forward-succeeded t))
      (while (forward-comment -1)
        (setq beg (point)))

      ;; Move BEG to BOL.
      (goto-char beg)
      (skip-chars-backward " \t")
      (setq beg (point))

      ;; Move END to BOL.
      (goto-char end)
      (skip-chars-backward " \t")
      (setq end (point))

      (when (and forward-succeeded
                 ;; If we are at the BOL of the line below a comment,
                 ;; don’t include this comment. (END will be at the
                 ;; BOL of the line after the comment.)
                 (< orig end))
        (push `(comment . ,(cons beg end)) result))
      result)))

(defun expreg--paragraph ()
  "Return a list of regions containing paragraphs."
  (save-excursion
    (condition-case nil
        (let ((orig (point))
              beg end result)
          (cond
           ;; Using defun.
           ((or (derived-mode-p 'prog-mode)
                beginning-of-defun-function)
            (when (beginning-of-defun)
              (setq beg (point))
              (end-of-defun)
              (setq end (point))
              ;; If we are at the BOL right below a defun, don’t mark
              ;; that defun.
              (unless (eq orig end)
                (push `(paragraph . ,(cons beg end)) result))))
           ;; Use paragraph.
           ((or (derived-mode-p 'text-mode)
                (eq major-mode 'fundamental-mode))
            (backward-paragraph)
            (skip-syntax-forward "-")
            (setq beg (point))
            (forward-paragraph)
            (setq end (point))
            (push `(paragraph . ,(cons beg end)) result)))
          result)
      (scan-error nil))))


(provide 'expreg)

;;; expreg.el ends here

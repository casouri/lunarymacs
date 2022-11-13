;;; expreg.el --- Simple expand region  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This is just like expand-region, but much simpler. Bind
;; ‘expreg-expand’ and ‘expreg-contract’ and start using it.
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

;;; Code:

(require 'subword)
(require 'treesit)
(require 'cl-lib)

;;; Cutom options and variables

(defvar-local expreg-functions
    '( expreg--word expreg--sexp expreg--treesit
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
                     (< (- (cdr a) (car a))
                        (- (cdr b) (car b))))))

(defun expreg--valid-p (region orig)
  "Return non-nil if REGION = (BEG . END) valid regarding ORIG.
ORIG is the current position."
  (let ((beg (car region))
        (end (cdr region)))
    (and (<= beg orig end) (< beg end))))

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
                  (eq (region-beginning) (caar expreg--prev-regions))
                  (eq (region-end) (cdar expreg--prev-regions))))
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
                (let ((beg (caar expreg--next-regions))
                      (end (cdar expreg--next-regions)))
                  (and (<= (region-beginning) beg)
                       (<= end (region-end)))))
      ;; Pop from next-regions, push into prev-regions.
      (push (pop expreg--next-regions)
            expreg--prev-regions)))
  (when expreg--next-regions
    (let ((region (pop expreg--next-regions)))
      (set-mark (cdr region))
      (goto-char (car region))
      (push region expreg--prev-regions)))
  (when expreg--verbose
    (message "next: %S\nprev: %S"
             expreg--next-regions expreg--prev-regions)))

(defun expreg-contract ()
  "Contract region."
  (interactive)
  (when (and (region-active-p)
             (> (length expreg--prev-regions) 1))
    (push (pop expreg--prev-regions) expreg--next-regions)
    (set-mark (cdar expreg--prev-regions))
    (goto-char (caar expreg--prev-regions)))
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
          (inhibit-point-motion-hooks t)
          result
          beg end)
      ;; (1) subwords in camel-case.
      (subword-backward)
      (setq beg (point))
      (subword-forward)
      (setq end (point))
      (push (cons beg end) result)

      ;; (2) subwords by “-” or “_”.
      (goto-char orig)
      (skip-syntax-forward "w")
      (setq end (point))
      (skip-syntax-backward "w")
      (setq beg (point))
      (push (cons beg end) result)

      ;; (3) symbol-at-point
      (goto-char orig)
      (skip-syntax-forward "w_")
      (setq end (point))
      (skip-syntax-backward "w_")
      (setq beg (point))
      (push (cons beg end) result)

      ;; (4) within whitespace & paren. (Allow word constituents, symbol
      ;; constituents, punctuation.)
      (goto-char orig)
      (skip-syntax-forward "w_.")
      (setq end (point))
      (skip-syntax-backward "w_.")
      (setq beg (point))
      (push (cons beg end) result)
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
            (push (cons beg end) result)))
        (setq node (treesit-node-parent node)))
      result)))

(defun expreg--sexp ()
  "Return a list of regions determined by sexp level."
  (save-excursion
    (let ((orig (point))
          result beg end)
      (cl-labels ((inside-list ()
                    ;; Push the region inside the list. Leaves point
                    ;; at the beginning. Return t if success, nil if
                    ;; fail.
                    (if (eq (car (syntax-ppss)) 0)
                        ;; Don’t run if at top-level
                        nil
                      (if (nth 3 (syntax-ppss))
                          ;; Inside a string
                          (ignore-errors
                            (goto-char (nth 8 (syntax-ppss)))
                            (setq beg (1+ (point)))
                            (forward-sexp)
                            (setq end (1- (point)))
                            (push (cons beg end) result)
                            (goto-char beg))
                        ;; Not inside a string.
                        (let ((start (point)) beg end)

                          (while (condition-case nil
                                     (progn (forward-sexp) t)
                                   (scan-error nil)))
                          (setq end (point))
                          (goto-char start)
                          (while (condition-case nil
                                     (progn (backward-sexp) t)
                                   (scan-error nil)))
                          (setq beg (point))
                          (push (cons beg end) result)))
                      t))
                  (outside-list ()
                    ;; Assumes point at beginning of inside a list.
                    ;; Push the region covering the list. Return t if
                    ;; success, nil if fail.
                    (condition-case nil
                        (if (nth 3 (syntax-ppss))
                            ;; Inside a string
                            (progn
                              (goto-char (nth 8 (syntax-ppss)))
                              (setq beg (point))
                              (forward-sexp)
                              (setq end (point))
                              (push (cons beg end) result)
                              t)
                          ;; Not inside a string.
                          (backward-up-list)
                          (setq beg (point))
                          (forward-sexp)
                          (setq end (point))
                          (push (cons beg end) result)
                          t)
                      (scan-error nil))))
        (when (null (nth 3 (syntax-ppss)))
          ;; Point at beginning of a sexp.
          (condition-case nil
              (save-excursion
                (forward-sexp)
                (setq end (point))
                (backward-sexp)
                ;; Skip "'" and "#'", etc.
                (skip-syntax-forward "'")
                (when (eq (point) orig)
                  (skip-syntax-backward "'")
                  (push (cons (point) end) result)))
            (scan-error nil))
          ;; Point at end of a sexp.
          (condition-case nil
              (save-excursion
                (backward-sexp)
                (setq beg (point))
                (forward-sexp)
                (when (eq (point) orig)
                  (push (cons beg (point)) result)))
            (scan-error nil)))
        (inside-list)
        (while (outside-list)))
      result)))

(defun expreg--comment ()
  "Return a list of regions containing comment."
  (save-excursion
    (let ((orig (point))
          (beg (point))
          (end (point))
          start result
          forward-succeeded backward-succeeded)
      ;; Go backward to the beginning of a comment (if exists).
      (while (nth 4 (syntax-ppss))
        (backward-char))
      (setq start (point))

      ;; Now we are either at the beginning of a comment, or not on a
      ;; comment at all.
      (while (forward-comment 1)
        (setq end (point))
        (setq forward-succeeded t))

      (goto-char start)
      (while (forward-comment -1)
        (setq beg (point))
        (setq backward-succeeded t))

      ;; Move BEG to BOL.
      (goto-char beg)
      (skip-chars-backward " \t")
      (setq beg (point))

      ;; Move END to BOL.
      (goto-char end)
      (skip-chars-backward " \t")
      (setq end (point))

      (when (and (or forward-succeeded backward-succeeded)
                 ;; If we are at the BOL of the line below a comment,
                 ;; don’t include this comment. (END will be at the
                 ;; BOL of the line after the comment.)
                 (< orig end))
        (push (cons beg end) result))
      result)))

(defun expreg--paragraph ()
  "Return a list of regions containing paragraphs."
  (save-excursion
    (let (beg end result)
      (if (or (derived-mode-p 'prog-mode)
              beginning-of-defun-function)
          (when (beginning-of-defun)
            (setq beg (point))
            (end-of-defun)
            (setq end (point))
            (push (cons beg end) result))
        (backward-paragraph)
        (skip-syntax-forward "-")
        (setq beg (point))
        (forward-paragraph)
        (setq end (point))
        (push (cons beg end) result))
      result)))


(provide 'expreg)

;;; expreg.el ends here

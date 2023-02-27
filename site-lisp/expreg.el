;;; expreg.el --- Simple expand region  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This is just like expand-region, but (1) we generate all regions at
;; once, and (2) should be easier to debug. Bind ‘expreg-expand’ and
;; ‘expreg-contract’ and start using it.

;;; Developer
;;
;; It works roughly as follows: ‘expreg-expand’ collects a list of
;; possible expansions on startup with functions in
;; ‘expreg-functions’. Then it sorts them by each region’s size. It
;; also removes duplicates, etc. Then this list is stored in
;; ‘expreg--next-regions’. (There could be better sorting algorithms,
;; but so far I haven’t seen the need for one.)
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
;; ‘cadr’, accessing END is ‘cddr’. Sometimes FN is the function name
;; plus some further descriptions, eg, word, word--symbol,
;; word--within-space are all produced by ‘expreg--word’. I use double
;; dash to indicate the additional descriptor.
;;
;; Credit: I stole a lot of ideas on how to expand lists and strings
;; from ‘expand-region’ :-)

;;; Code:

(require 'subword)
(require 'treesit)
(eval-when-compile
  (require 'cl-lib)
  (require 'seq))

;;; Cutom options and variables

(defvar-local expreg-functions
    '( expreg--subword expreg--word expreg--list expreg--string
       expreg--treesit expreg--comment expreg--paragraph)
  "A list of expansion functions.

Each function is called with no arguments and should return a
list of (BEG . END). The list don’t have to be sorted, and can
have duplicates. It’s also fine to include invalid regions, such
as ones where BEG equals END, etc, these will be filtered out by
‘expreg-expand’.

The function could move point, but shouldn’t return any error.")

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

(defun expreg--filter-regions (regions orig)
  "Filter out invalid regions in REGIONS regarding ORIG.
ORIG is the current position. Each region is (BEG . END)."
  (let (orig-at-beg-of-something
        orig-at-end-of-something)

    (setq regions (seq-filter
                   (lambda (region)
                     (expreg--valid-p region orig))
                   regions))

    ;; It is important that this runs after the first filter. If this
    ;; is t, it means there are some REGION that starts/ends at ORIG.
    (dolist (region regions)
      (when (eq (cadr region) orig)
        (setq orig-at-beg-of-something t))
      (when (eq (cddr region) orig)
        (setq orig-at-end-of-something t)))

    ;; If there are regions that start at ORIG, filter out
    ;; regions that ends at ORIG.
    (setq regions (cl-remove-if
                   (lambda (region)
                     (and orig-at-beg-of-something
                          (eq (cddr region) orig)))
                   regions))

    ;; OTOH, if there are regions that ends at ORIG, filter out
    ;; regions that starts AFTER ORIGN, eg, special cases in
    ;; ‘expreg--list-at-point’.
    (setq regions (cl-remove-if
                   (lambda (region)
                     (and orig-at-end-of-something
                          (> (cadr region) orig)))
                   regions))
    regions))

;;; Syntax-ppss shorthands

(defsubst expreg--inside-comment-p ()
  "Test whether point is inside a comment."
  (nth 4 (syntax-ppss)))

(defsubst expreg--inside-string-p ()
  "Test whether point is inside a string."
  (nth 3 (syntax-ppss)))

(defsubst expreg--start-of-comment-or-string ()
  "Start position of enclosing comment/string."
  (nth 8 (syntax-ppss)))

(defsubst expreg--current-depth ()
  "Current list depth."
  (car (syntax-ppss)))

(defsubst expreg--start-of-list ()
  "Start position of innermost list."
  (nth 1 (syntax-ppss)))

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

  ;; If we are not already in the middle of expansions, compute them.
  (when (and (null expreg--next-regions)
             (null expreg--prev-regions))
    (let* ((orig (point))
           (regions (mapcan (lambda (fn) (save-excursion
                                           (funcall fn)))
                            expreg-functions))
           (regions (expreg--filter-regions regions orig))
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

  ;; Expand to the next expansion.
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

(defun expreg--subword ()
  "Return a list of regions of the CamelCase subword at point.
Only return something if ‘subword-mode’ is on, to keep consistency."
  (when subword-mode
    (let ((orig (point))
          beg end result)

      ;; Go forward then backward.
      (subword-forward)
      (setq end (point))
      (subword-backward)
      (setq beg (point))
      (skip-syntax-forward "w")
      ;; Make sure we stay in the word boundary. Because
      ;; ‘subword-backward/forward’ could go through parenthesis, etc.
      (when (>= (point) end)
        (push `(subword--forward . ,(cons beg end)) result))

      ;; Because ‘subword-backward/forward’ could go through
      ;; parenthesis, etc, we need to run it in reverse to handle the
      ;; case where point is at the end of a word.
      (goto-char orig)
      (subword-backward)
      (setq beg (point))
      (subword-forward)
      (setq end (point))
      (skip-syntax-backward "w")
      (when (<= (point) beg)
        (push `(subword--backward . ,(cons beg end)) result))

      result)))

(defun expreg--word ()
  "Return a list of regions within the word at point."
  ;; - subwords in camel-case (when ‘subword-mode’ is on).
  ;; - subwords by “-” or “_”.
  ;; - symbol-at-point
  ;; - within whitespace & paren/quote (but can contain punctuation)
  ;;   (“10–20”, “1.2”, “1,2”, etc). (This is technically not always
  ;;   within a word anymore...)
  (let ((orig (point))
        result
        beg end)

    ;; (2) subwords by “-” or “_”.
    (goto-char orig)
    (skip-syntax-forward "w")
    (setq end (point))
    (skip-syntax-backward "w")
    (setq beg (point))
    (push `(word--plain . ,(cons beg end)) result)

    ;; (3) symbol-at-point
    (goto-char orig)
    (skip-syntax-forward "w_")
    (setq end (point))
    (skip-syntax-backward "w_")
    (setq beg (point))
    (push `(word--symbol . ,(cons beg end)) result)

    ;; (4) within whitespace & paren. (Allow word constituents, symbol
    ;; constituents, punctuation, prefix (#' and ' in Elisp).)
    (goto-char orig)
    (skip-syntax-forward "w_.'")
    (setq end (point))
    (skip-syntax-backward "w_.'")
    (setq beg (point))
    (push `(word--within-space . ,(cons beg end)) result)

    ;; Return!
    result))

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
Does not move point."
  (condition-case nil
      (save-excursion
        ;; Inside a string? Move out of it first.
        (when (expreg--inside-string-p)
          (goto-char (expreg--start-of-comment-or-string)))

        (when (> (expreg--current-depth) 0)
          (let (beg end beg-w-spc end-w-spc)
            (goto-char (expreg--start-of-list))
            (save-excursion
              (forward-char)
              (setq beg-w-spc (point))
              (skip-syntax-forward "-")
              (setq beg (point)))

            (forward-list)
            (backward-char)
            (setq end-w-spc (point))
            (skip-syntax-backward "-")
            (setq end (point))

            `((inside-list . ,(cons beg end))
              (inside-list . ,(cons beg-w-spc end-w-spc))))))
    (scan-error nil)))

(defun expreg--list-at-point ()
  "Return a list of one region marking the list at point, or nil.
Point should be at the beginning or end of a list. Does not move
point."
  (unless (expreg--inside-string-p)
    (condition-case nil
        (save-excursion
          ;; Even if point is not at the beginning of a list, but
          ;; before a list (with only spaces between), we want to
          ;; return a region covering that list after point, for
          ;; convenience. But because this region will not cover
          ;; point, it will not pass the filtering, so this function
          ;; needs to be added to ‘expreg--validation-white-list’.
          (when (and (looking-at (rx (syntax whitespace)))
                     (not (looking-back ")" 1)))
            (skip-syntax-forward "-"))

          ;; If at the end of a list and not the beginning of another
          ;; one, move to the beginning of the list. Corresponding
          ;; char for each int: 40=(, 39=', 41=).
          (when (and (eq 41 (char-syntax (or (char-before) ?x)))
                     (not (memq (char-syntax (or (char-after) ?x))
                                '(39 40))))
            (backward-list 1))

          (when (memq (char-syntax (or (char-after) ?x))
                      '(39 40))
            (let ((beg (if (eq 39 (char-syntax (or (char-before) ?x)))
                           (1- (point))
                         (point))))
              (forward-list)
              (list `(list-at-point . ,(cons beg (point)))))))
      (scan-error nil))))

(defun expreg--outside-list ()
  "Return a list of one region marking outside the list, or nil.
If find something, leave point at the beginning of the list."
  (let (beg end)
    (condition-case nil
        (when (> (expreg--current-depth) 0)
          (save-excursion

            ;; If point inside a list but not at the beginning of one,
            ;; move to the beginning of enclosing list.
            (when (> (expreg--current-depth) 0)
              (goto-char (expreg--start-of-list)))
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
        (progn
          (if (expreg--inside-string-p)
              ;; Inside a string? Move to beginning.
              (goto-char (expreg--start-of-comment-or-string))

            ;; Not inside a string, but at the end of a string and not at
            ;; the beginning of another one? Move to beginning.
            (when (and (eq (char-syntax (or (char-before) ?x)) 34)
                       (not (eq (char-syntax (or (char-after) ?x)) 34)))
              (backward-sexp)))

          ;; Not inside a string and at the beginning of one.
          (when (and (not (expreg--inside-string-p))
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
  "Return a list of regions determined by sexp level.

This routine returns the following regions:
1. The list before/after point
2. The inside of the innermost enclosing list
3. The outside of every layer of enclosing list

Note that the inside of outer layer lists are not captured."
  (let (inside-results)
    (when (expreg--inside-string-p)
      ;; If point is inside a string, we narrow to the inside of that
      ;; string and compute again.
      (save-restriction
        (let ((orig (point))
              (string-start (expreg--start-of-comment-or-string)))

          ;; Narrow to inside list.
          (goto-char string-start)
          (forward-sexp)
          (narrow-to-region (1+ string-start) (1- (point)))
          (goto-char orig)
          (setq inside-results (expreg--list)))))

    ;; Normal computation.
    (let ((inside-list (expreg--inside-list))
          (list-at-point (expreg--list-at-point))
          outside-list lst)

      ;; Compute outer-list.
      (while (setq lst (expreg--outside-list))
        (setq outside-list
              (nconc lst outside-list)))

      (nconc inside-results inside-list list-at-point outside-list))))

(defun expreg--comment ()
  "Return a list of regions containing comment."
  (let ((orig (point))
        (beg (point))
        (end (point))
        result forward-succeeded trailing-comment-p)

    ;; Go backward to the beginning of a comment (if exists).
    (while (expreg--inside-comment-p)
      (backward-char))

    ;; Now we are either at the beginning of a comment, or not on a
    ;; comment at all. (When there are multiple lines of comment,
    ;; each line is an individual comment.)
    (while (forward-comment 1)
      (setq end (point))
      (setq forward-succeeded t))
    (while (forward-comment -1)
      (setq beg (point)))

    (goto-char beg)
    (setq trailing-comment-p
          (not (looking-back (rx bol (* whitespace))
                             (line-beginning-position))))
    (when (not trailing-comment-p)
      ;; Move BEG to BOL.
      (skip-chars-backward " \t")
      (setq beg (point))

      ;; Move END to BOL.
      (goto-char end)
      (skip-chars-backward " \t")
      (setq end (point)))

    (when (and forward-succeeded
               ;; If we are at the BOL of the line below a comment,
               ;; don’t include this comment. (END will be at the
               ;; BOL of the line after the comment.)
               (< orig end))
      (push `(comment . ,(cons beg end)) result))
    result))

(defun expreg--paragraph ()
  "Return a list of regions containing paragraphs."
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
              (push `(paragraph-defun . ,(cons beg end)) result))))

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
    (scan-error nil)))


(provide 'expreg)

;;; expreg.el ends here

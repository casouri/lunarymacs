;;; sfill.el --- Soft and smart fill      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package gives you word wrapping with more precision than the
;; default one. The default word wrapping (‘toggle-word-wrap’) can
;; only wrap on white spaces and tabs, thus is unable to wrap text
;; with both CJK characters and latin characters properly. Also it
;; can’t wrap on arbitrary columns. On the other hand,
;; ‘fill-paragraph’ can only work with mono spaced fonts, filling
;; variable pitch font usually gives sub-optimal result. (And, of
;; course, it destructively insert newlines, which may not be what you
;; want.)
;;
;; This package solves above problems. It wraps lines correctly no
;; matter the text is latin or CJK or both, and no matter it’s mono
;; spaces or variable pitch. It wraps on arbitrary columns and it
;; handles kinsoku correctly (thanks to kinsoku.el).
;;
;;   Usage
;;
;; 	M-x sfill-mode RET
;;
;;   Customization
;;
;; ‘sfill-column’.

;;; Code:
;;

(require 'subr-x)
(require 'cl-lib)

(defvar-local sfill-column 70
  "Fill Column for sfill.")

(defface sfill-debug-face (let ((spec '(:inherit highlight))
                                (display t))
                            `((,display . ,spec)))
  "Face for highlighting sfill overlays."
  :group 'sfill)

(define-minor-mode sfill-debug-mode
  "Toggle debug mode for sfill."
  :lighter ""
  :global t
  (sfill-region nil nil t))

(defun sfill-insert-newline ()
  "Insert newline at point by overlay."
  ;; We shouldn’t need to break line at point-max.
  (if (or (eq (point) (point-max)))
      (error "Cannot insert at the end of visible buffer")
    (let* ((beg (point))
           (end (1+ (point)))
           (ov (make-overlay beg end nil t)))
      (overlay-put ov 'sfill t)
      (overlay-put ov 'before-string "\n")
      (overlay-put ov 'evaporate t)
      (when sfill-debug-mode
        (overlay-put ov 'face 'sfill-debug-face)))))

(defun sfill-clear-overlay (beg end)
  "Clear overlays that `soft-insert' made between BEG and END."
  (let ((overlay-list (overlays-in beg end)))
    (dolist (ov overlay-list)
      (when (overlay-get ov 'sfill)
        (delete-overlay ov)))))

(defun sfill-delete-overlay-at (point)
  "Delete sfill overlay at POINT."
  (sfill-clear-overlay point (1+ point)))

(defun sfill-clear-newline (beg end)
  "Remove newlines in the region from BEG to END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\n" end t)
      ;; I can be more intelligent here, but since the break point
      ;; function is from fill.el, better keep in sync with it.
      ;; (see ‘fill-move-to-break-point’)
      (if (and (eq (char-charset (char-before (1- (point)))) 'ascii)
	       (eq (char-charset (char-after (point))) 'ascii))
          (replace-match " ")
        (replace-match "")))
    (put-text-property beg end 'sfill-bol nil)))

(defun sfill-forward-column (column)
  "Forward COLUMN columns.

This only works correctly in mono space setting."
  (condition-case nil
      (while (>= column 0)
        (forward-char)
        (setq column (- column (char-width (char-before)))))
    ('end-of-buffer nil)))

(defun sfill-forward-column-visual (column)
  "Forward COLUMN columns and return point.

Works for both mono space and variable pitch."
  ;; ‘column-x-pos’ is the x offset from widow’s left edge in pixels.
  ;; We want to break around this position.
  (condition-case nil
      (let ((column-x-pos (* column (window-font-width))))
        (while (>= column-x-pos 0)
          (forward-char)
          (setq column-x-pos
                (- column-x-pos
                   (car (mapcar (lambda (glyph) (aref glyph 4))
                                (font-get-glyphs (font-at (1- (point)))
                                                 (1- (point))(point)))))))
        (point))
    ('end-of-buffer (point))))

(defun sfill-go-to-break-point (linebeg bound)
  "Move to the position where the line should be broken.
LINEBEG is the beginning of current visual line.
We don’t go beyond BOUND."
  (if (display-multi-font-p)
      (sfill-forward-column-visual sfill-column)
    (sfill-forward-column sfill-column))
  (if (> (point) bound)
      (goto-char bound))
  ;; If this (visual) line is the last line of the (visual) paragraph,
  ;; (point) would be equal to bound, and we want to stay there, so
  ;; that later we don’t insert newline incorrectly.
  (unless (>= (point) bound)
    (fill-move-to-break-point linebeg)
    (skip-chars-forward " \t")))

(defsubst sfill-next-break (point bound)
  "Return the position of the first line break after POINT.
Don’t go beyond BOUND."
  (next-single-char-property-change
   (1+ point)
   'sfill nil bound))

(defsubst sfill-at-break (point)
  "Return non-nil if POINT is at a line break."
  (plist-get (text-properties-at point) 'sfill-bol))

(defsubst sfill-prev-break (point bound)
  "Return the position of the first line break before POINT.
Don’t go beyond BOUND."
  (1- (previous-single-char-property-change
       point 'sfill nil
       (1+ bound))))

(defun sfill-line (point &optional force)
  "Fill the line in where POINT is.
Return (BEG END) where the text is filled. BEG is the visual
beginning of current live. END is the actual end of line. If
FORCE is non-nil, update the whole line."
  (catch 'early-termination
    (save-window-excursion
      (save-excursion
        (if (eq point (point-max))
            (throw 'early-termination (cons point point)))
        (let* ((end (line-end-position))
               (prev-break (if (sfill-at-break point) point
                             (sfill-prev-break
                              point (line-beginning-position))))
               (prev-break (sfill-prev-break
                            prev-break (line-beginning-position)))
               next-existing-break
               (beg prev-break)
               (match-count 0))
          (goto-char beg)
          (while (< (point) end)
            (setq next-existing-break (sfill-next-break (point) end))
            (sfill-delete-overlay-at next-existing-break)
            (sfill-go-to-break-point (point) end)
            (unless (>= (point) end)
              (sfill-insert-newline))
            (if (eq next-existing-break (point))
                (setq match-count (1+ match-count)))
            (if (and (not force) (>= match-count 2))
                (throw 'early-termination (cons beg end))))
          (cons beg end))))))

;; Slightly faster but not completely correct
;;
;; (defun sfill-line (point &optional force)
;;   "Fill the line in where POINT is.
;; Return (BEG END) where the text is filled. BEG is the visual
;; beginning of current live. END is the actual end of line. If
;; FORCE is non-nil, update the whole line."
;;   (catch 'early-termination
;;     (save-window-excursion
;;       (save-excursion
;;         (if (eq point (point-max))
;;             (throw 'early-termination (cons point point)))
;;         (let* ((end (line-end-position))
;;                (prev-break (if (sfill-at-break point) point
;;                              (sfill-prev-break
;;                               point (line-beginning-position))))
;;                next-existing-break
;;                (beg prev-break))
;;           (goto-char beg)
;;           (while (< (point) end)
;;             (setq next-existing-break (sfill-next-break (point) end))
;;             (sfill-delete-overlay-at next-existing-break)
;;             (sfill-go-to-break-point (point) end)
;;             (unless (>= (point) end)
;;               (sfill-insert-newline))
;;             (if (and (not force) (eq next-existing-break (point)))
;;                 (throw 'early-termination (cons beg end))))
;;           (cons beg end))))))

(defun sfill-region (&optional beg end force)
  "Fill each line in the region from BEG to END.

If FORCE is non-nil, update the whole line. BEG and END default
to beginning and end of the buffer."
  (save-excursion
    (goto-char (or beg (point-min)))
    (sfill-line (point) force)
    (while (re-search-forward "\n" (or end (point-max)) t)
      (sfill-line (point) force))
    (cons (or beg (point-min)) (or end (point-max)))))

;; (defun sfill-paragraph ()
;;   "Fill current paragraph."
;;   (interactive)
;;   (let (beg end)
;;     (save-excursion
;;       (backward-paragraph)
;;       (skip-chars-forward "\n")
;;       (setq beg (point))
;;       (forward-paragraph)
;;       (skip-chars-backward "\n")
;;       (setq end (point))
;;       (sfill-region-destructive beg end))))

(defun sfill-unfill (&optional beg end)
  "Un-fill region from BEG to END, default to whole buffer."
  (sfill-clear-overlay (or beg (point-min)) (or end (point-max))))

(defun sfill-jit-lock-fn (beg end)
  "Fill line in region between BEG and END."
  (cons 'jit-lock-bounds (sfill-region beg end)))

(defvar sfill-mode-map (let ((map (make-sparse-keymap)))
                         ;; (define-key map (kbd "C-a") #'backward-sentence)
                         ;; (define-key map (kbd "C-e") #'forward-sentence)
                         map)
  "The keymap for minor mode ‘sfill-mode’.")

(define-minor-mode sfill-mode
  "Automatically wrap lines."
  :lighter ""
  :keymap 'sfill-mode-map
  (if sfill-mode
      (progn
        (jit-lock-register #'sfill-jit-lock-fn)
        (jit-lock-refontify))
    (jit-lock-unregister #'sfill-jit-lock-fn)
    (sfill-unfill)))

(provide 'sfill)

;;; sfill.el ends here

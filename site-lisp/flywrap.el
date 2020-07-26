;;; flywrap.el --- Soft and smart fill      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs
;;
;;; This package is obsolete.
;;
;; Emacs has added support for proper word wraps.

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
;; 	M-x flywrap-mode RET
;;
;;   Customization
;;
;; ‘flywrap-column’.

;;; Code:
;;

(require 'subr-x)
(require 'cl-lib)

(defvar-local flywrap-column 70
  "Fill Column for flywrap.")

;;; Backstage

(defface flywrap-debug-face (let ((spec '(:inherit highlight))
                                  (display t))
                              `((,display . ,spec)))
  "Face for highlighting flywrap overlays."
  :group 'flywrap)

(define-minor-mode flywrap-debug-mode
  "Toggle debug mode for flywrap."
  :lighter ""
  :global t
  (flywrap-region nil nil t))

(defun flywrap-insert-newline ()
  "Insert newline at point by overlay."
  ;; We shouldn’t need to break line at point-max.
  (if (or (eq (point) (point-max)))
      (error "Cannot insert at the end of visible buffer")
    (let* ((beg (point))
           (end (1+ (point)))
           (ov (make-overlay beg end nil t)))
      (overlay-put ov 'flywrap t)
      (overlay-put ov 'before-string "\n")
      (overlay-put ov 'evaporate t)
      (when flywrap-debug-mode
        (overlay-put ov 'face 'flywrap-debug-face)))))

(defun flywrap-clear-overlay (beg end)
  "Clear overlays that `soft-insert' made between BEG and END."
  (let ((overlay-list (overlays-in beg end)))
    (dolist (ov overlay-list)
      (when (overlay-get ov 'flywrap)
        (delete-overlay ov)))))

(defun flywrap-delete-overlay-at (point)
  "Delete flywrap overlay at POINT."
  (flywrap-clear-overlay point (1+ point)))

(defun flywrap-unfill (beg end)
  "Remove newlines in the region from BEG to END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\n" end t)
      (unless
          ;; If we are at point-max, ‘char-after’ returns nil.
          (eq (point) (point-max))
        ;; Regarding the 'ascii: I can be more intelligent here
        ;; (include iso-latin, etc), but since the break point function
        ;; is from fill.el, better keep in sync with it. (see
        ;; ‘fill-move-to-break-point’).
        ;; Don’t remove consecutive newlines.
        (cond ((or (eq (char-before (1- (point))) ?\n)
                   (eq (char-after (point)) ?\n))
               nil)
              ;; Separate ascii characters with space
              ((and (eq (char-charset (char-before (1- (point)))) 'ascii)
	            (eq (char-charset (char-after (point))) 'ascii))
               (replace-match " "))
              ;; Don’t separate CJK characters.
              (t (replace-match "")))))))

(defun flywrap-forward-column (column)
  "Forward COLUMN columns.

This only works correctly in mono space setting."
  (condition-case nil
      (while (>= column 0)
        (forward-char)
        (setq column (- column (char-width (char-before)))))
    ('end-of-buffer nil)))

(defun flywrap-forward-column-visual (column)
  "Forward COLUMN columns and return point.

Works for both mono space and variable pitch."
  ;; ‘column-x-pos’ is the x offset from widow’s left edge in pixels.
  ;; We want to break around this position.
  (condition-case nil
      (let ((column-x-pos (* column (window-font-width))))
        (while (>= column-x-pos 0)
          (forward-char)
          (unless (invisible-p (point))
            (setq column-x-pos
                  (- column-x-pos
                     (car (mapcar
                           (lambda (glyph) (aref glyph 4))
                           (font-get-glyphs (font-at (1- (point)))
                                            (1- (point))(point))))))))
        (point))
    ('end-of-buffer (point))))

(defun flywrap-go-to-break-point (linebeg bound)
  "Move to the position where the line should be broken.
LINEBEG is the beginning of current visual line.
We don’t go beyond BOUND."
  ;; Go to roughly the right place to break.
  (if (display-multi-font-p)
      (flywrap-forward-column-visual flywrap-column)
    (flywrap-forward-column flywrap-column))
  ;; If this (visual) line is the last line of the (visual) paragraph,
  ;; (point) would be equal to bound, and we want to stay there, so
  ;; that later we don’t insert newline incorrectly.
  (if (>= (point) bound)
      (goto-char bound))
  ;; Find the RIGHT place to break.
  (when (< (point) bound)
    (let ((fill-nobreak-invisible t))
      (fill-move-to-break-point linebeg))
    (skip-chars-forward " \t")))

(defsubst flywrap-next-break (point bound)
  "Return the position of the first line break after POINT.
Don’t go beyond BOUND."
  (if (eq point (point-max))
      point
    (next-single-char-property-change
     (1+ point)
     ;; If we pass a bound larger than point-max,
     ;; Emacs hangs. (#40000)
     'flywrap nil (min bound (point-max)))))

(defsubst flywrap-at-break (point)
  "Return non-nil if POINT is at a line break."
  (plist-get (mapcan #'overlay-properties
                     (overlays-at point))
             'flywrap))

(defsubst flywrap-prev-break (point bound)
  "Return the position of the first line break before POINT.
Don’t go beyond BOUND."
  (max (1- (previous-single-char-property-change
            point 'flywrap nil
            (max (1+ bound) (point-min))))
       (point-min)))

(defun flywrap-line (point &optional force)
  "Fill the line in where POINT is.
Return (BEG END) where the text is filled. BEG is the visual
beginning of current live. END is the actual end of line. If
FORCE is non-nil, update the whole line."
  (catch 'early-termination
    (save-excursion
      (if (eq point (point-max))
          (throw 'early-termination (cons point point)))
      (let* ((end (line-end-position))
             (prev-break (if (flywrap-at-break point) point
                           (flywrap-prev-break
                            point (line-beginning-position))))
             (prev-break (flywrap-prev-break
                          prev-break (line-beginning-position)))
             next-existing-break
             (beg prev-break)
             (match-count 0))
        (goto-char beg)
        (while (< (point) end)
          (setq next-existing-break (flywrap-next-break (point) end))
          (flywrap-delete-overlay-at next-existing-break)
          (flywrap-go-to-break-point (point) end)
          (unless (>= (point) end)
            (flywrap-insert-newline))
          (if (eq next-existing-break (point))
              (setq match-count (1+ match-count)))
          (if (and (not force) (>= match-count 2))
              (throw 'early-termination (cons beg end))))
        (cons beg end)))))

(defun flywrap-region (&optional beg end force)
  "Fill each line in the region from BEG to END.

If FORCE is non-nil, update the whole line. BEG and END default
to beginning and end of the buffer."
  (save-excursion
    (goto-char (or beg (point-min)))
    (flywrap-line (point) force)
    (while (re-search-forward "\n" (or end (point-max)) t)
      (flywrap-line (point) force))
    (cons (or beg (point-min)) (or end (point-max)))))

;; (defun flywrap-paragraph ()
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
;;       (flywrap-region-destructive beg end))))

(defun flywrap-unwrap (&optional beg end)
  "Un-fill region from BEG to END, default to whole buffer."
  (flywrap-clear-overlay (or beg (point-min)) (or end (point-max))))

(defun flywrap-jit-lock-fn (beg end)
  "Fill line in region between BEG and END."
  (cons 'jit-lock-bounds (flywrap-region beg end)))

;; Currently not used.
(defun flywrap-after-change-fn (beg _ _)
  "Hook called after buffer content change.
See ‘after-change-functions’ for explanation on BEG END LEN."
  (flywrap-region beg (line-end-position))
  ;; (if (eq (- end beg) 1)
  ;;     ;; Self insert command, only wrap on space
  ;;     (when (member (char-after beg) '(?\s ?， ?。 ?、))
  ;;       (flywrap-region beg (line-end-position)))
  ;;   (flywrap-region beg (line-end-position)))
  )

;;; Userland

(defun flywrap-next-line ()
  "Move to the next line."
  (interactive)
  (let ((col (current-column)))
    (goto-char (flywrap-next-break (point) (save-excursion (re-search-forward "\n")
                                                           (point))))))


(defun flywrap-move-end-of-line (&optional arg)
  "Move to the end of current visual line.

With argument ARG not nil, move to the next ARG line end."
  (interactive "^p")
  (let ((arg (or arg 1))
        (point (point)))
    ;; This way hitting C-e at (visual) EOL doesn’t move to next line.
    (if (flywrap-at-break (1+ point))
        (setq point (1- point)))
    (while (> arg 0)
      (setq point (flywrap-next-break point (1+ (line-end-position))))
      (setq arg (1- arg)))
    (goto-char (1- point))))

(defun flywrap-move-beginning-of-line (arg)
  "Move to the beginning of current visual line.

With argument ARG not nil, move to the previous ARG line beginning."
  (interactive "^p")
  (let ((arg (or arg 1))
        (point (point)))
    ;; This way hitting C-a at (visual) BOL doesn’t move to previous line.
    (if (flywrap-at-break point)
        (setq point (+ 2 point)))
    (while (> arg 0)
      (setq point (flywrap-prev-break point (line-beginning-position)))
      (setq arg (1- arg)))
    (goto-char point)))

(defvar flywrap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") #'flywrap-move-beginning-of-line)
    (define-key map (kbd "C-e") #'flywrap-move-end-of-line)
    map)
  "The keymap for minor mode ‘flywrap-mode’.")

(define-minor-mode flywrap-mode
  "Automatically wrap lines."
  :lighter ""
  :keymap 'flywrap-mode-map
  (if flywrap-mode
      (progn
        ;; We want to control the depth of ‘flywrap-jit-lock-fn’ so it
        ;; runs hopefully after other functions. For example, let Org
        ;; mode’s fortifier to add invisible property (for links, etc)
        ;; before we wrap lines.
        (add-hook 'jit-lock-functions #'flywrap-jit-lock-fn 90 t)
        ;; Fix problem with incorrect wrapping when unfold a org
        ;; header.
        (jit-lock-refontify))
    (jit-lock-unregister #'flywrap-jit-lock-fn)
    (flywrap-unwrap)))

(with-eval-after-load 'org-macs
  (advice-add #'org-flag-region :after
              (lambda (from to _ _1) (when flywrap-mode
                                       (flywrap-region from to)))))

(provide 'flywrap)

;;; flywrap.el ends here

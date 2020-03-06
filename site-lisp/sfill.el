;;; sfill.el --- Soft and smart fill      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package gives you word wrapping with more precision than the
;; default one. The default word wrapping (‘toggle-word-wrap’) can
;; only wrap on white spaces and tabs, thus is unable to wrap text
;; with both CJK character and latin character properly. Also it can’t
;; wrap on arbitrary columns. On the other hand, ‘fill-paragraph’ can
;; only work with mono spaced fonts, filling variable pitch font
;; usually gives sub optimal result.
;;
;; This package solves above problems. It wraps lines correctly no
;; matter the text is latin or CJK or both, no matter it’s mono spaces
;; or variable pitch. It wraps on arbitrary columns and it handles
;; kinsoku correctly (thanks to kinsoku.el).
;;
;;   Usage
;;
;; 	M-x sfill-mode RET
;;
;;   Customization
;;
;; Customize ‘sfill-column’ and ‘sfill-variable-pitch-column’. The
;; former is used in mono spaced wrapping mode and the latter is used
;; in variable pitch wrapping mode.
;;
;;   Two wrapping modes
;;
;; Sfill has two wrapping modes: mono spaced mode and variable pitched
;; mode. As their name suggests, you should use the mono spaced mode
;; for mono spaced text and variable pitched mode for variable pitched
;; font. Variable pitched mode can correctly wrap mono spaced text,
;; but slower (than mono spaced mode). Set ‘sfill-variable-pitch’ to
;; nil to enable mono spaced mode, and set it to t for variable
;; pitched mode. By default the variable is set to t.

;;; Code:
;;

(require 'subr-x)

(defvar-local sfill-column 70
  "Default fill Column for sfill.")

(defvar-local sfill-variable-pitch-column 70
  "Column used for variable pitch filling.")

(defvar-local sfill-variale-pitch t
  "Set to non-nil and sfill will assume variable pitch when filling.")

(defface sfill-debug-face (let ((spec '(:inherit default))
                            (display t))
                        `((,display . ,spec)))
  "Face for highlighting sfill overlays."
  :group 'sfill)

(define-minor-mode sfill-debug-mode
  "Toggle debug mode for sfill."
  :lighter ""
  (if sfill-debug-mode
      (set-face-attribute 'sfill-debug-face nil :inherit 'highlight)
    (set-face-attribute 'sfill-debug-face nil :inherit 'default)))

(defun sfill-insert (string)
  "Insert STRING at point by overlay."
  ;; We shouldn’t need to break line at point-max.
  (if (or (eq (point) (point-max)))
      (error "Cannot insert at the end of visible buffer")
    (let* ((beg (point))
           (end (1+ (point)))
           (ov (make-overlay beg end)))
      (overlay-put ov 'sfill t)
      (overlay-put ov 'before-string string)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'face 'sfill-debug-face))))

(defun sfill-clear-overlay (beg end)
  "Clear overlays that `soft-insert' made between BEG and END."
  (let ((overlay-list (overlays-in beg end)))
    (dolist (ov overlay-list)
      (when (overlay-get ov 'sfill)
        (delete-overlay ov)))))

(defun sfill-clear-char (char beg end)
  "Remove CHAR(string) in the region from BEG to END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward char end t)
      ;; I can be more intelligent here, but since the break point
      ;; function is from fill.el, better keep in sync with it.
      ;; (see ‘fill-move-to-break-point’)
      (if (and (eq (char-charset (char-before (1- (point)))) 'ascii)
	       (eq (char-charset (char-after (point))) 'ascii))
          (replace-match " ")
        (replace-match "")))))

(defun sfill-forward-column (column)
  "Forward COLUMN columns."
  (while (>= column 0)
    (forward-char)
    (setq column (- column (char-width (char-before))))))

(defun sfill-point-at-column-variable-pitch (column bound)
  "Return point at COLUMN. Works with mono space and variable pitch.

BOUND is point where we shouldn’t go beyond. Return nil if the
desired position is not in window or it is beyond BOUND."
  ;; X offset from widow’s left edge in pixels. We want to break
  ;; around this position.
  (when-let* ((column-x-pos (* column (window-font-width)))
              (initial-y (cadr (pos-visible-in-window-p nil nil t)))
              (point (posn-point (posn-at-x-y column-x-pos initial-y))))
    (when (< point bound) point)))

(defun sfill-region-destructive (beg end)
  "Fill region between BEG and END."
  (sfill-clear-char "\n" beg end)
  (sfill-region-1 beg end))

(defun sfill-region (beg end)
  "Fill region between BEG and END."
  ;; Save window configuration because we recenter window when text is
  ;; out of the window.
  (save-window-excursion
    (save-excursion
      ;; (1+ end) to clean the newline overlay at the very end.
      (sfill-clear-overlay beg (1+ end))
      (let (linebeg)
        (goto-char beg)
        (while (< (point) end)
          (setq linebeg (point))
          ;; We cannot use ‘move-to-column’ as fill.el does. Because we
          ;; break lines with overlays, so if we are at a fake newline,
          ;; ‘move-to-column’ doesn’t really go forward.
          (if sfill-variale-pitch
              (progn
                (unless (pos-visible-in-window-p)
                  (recenter 1 t))
                (goto-char (or (sfill-point-at-column-variable-pitch
                                sfill-variable-pitch-column end)
                               end)))
            (sfill-forward-column sfill-column))
          ;; Check again if we are in the desired range.
          (when (< (point) end)
            (fill-move-to-break-point linebeg)
            (skip-chars-forward " \t")
            (sfill-insert "\n")))))))

(defun sfill-jit-lock-fn (beg end)
  "Fill region between BEG and END.
Unlike ‘sfill-region’, we don’t remove newlines. Instead, we go
through each line in the region and wrap them."
  (save-excursion
    (goto-char beg)
    (sfill-wrap-line)
    (while (re-search-forward "\n" end t)
      (sfill-wrap-line))))

(defun sfill-wrap-line ()
  "Soft wrap current line with overlay."
  (interactive)
  (sfill-region (line-beginning-position) (line-end-position)))

(defun sfill-paragraph ()
  "Fill current paragraph."
  (interactive)
  (let (beg end)
    (save-excursion
      (backward-paragraph)
      (skip-chars-forward "\n")
      (setq beg (point))
      (forward-paragraph)
      (skip-chars-backward "\n")
      (setq end (point))
      (sfill-region-destructive beg end))))

(defun sfill-unfill (&optional beg end)
  "Un-fill region from BEG to END, default to whole buffer."
  (sfill-clear-overlay (or beg (point-min)) (or end (point-max))))

(define-minor-mode sfill-mode
  "Automatically wrap lines."
  :lighter ""
  (if sfill-mode
      (progn (add-to-list 'jit-lock-functions #'sfill-jit-lock-fn)
             (sfill-jit-lock-fn (point-min) (point-max)))
    (setq jit-lock-functions
          (remove 'sfill-jit-lock-fn jit-lock-functions))
    (sfill-unfill)))

(provide 'sfill)

;;; sfill.el ends here

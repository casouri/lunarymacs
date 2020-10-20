;;; iscroll.el --- Smooth scrolling over images      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Gone are the days when images jumps in and out of the window when
;; scrolling! This package makes scrolling over images as if the image
;; is made of many lines, instead of a single line. (Indeed, sliced
;; image with default scrolling has the similar behavior as what this
;; package provides.)
;;
;; To use this package:
;;
;;     M-x iscroll-mode RET
;;
;; This mode remaps mouse scrolling functions. If you use other
;; commands, you need to adapt them accordingly. See
;; `iscroll-mode-map' and `iscroll-mode' for some
;; inspiration.
;;
;; Commands provided:
;;
;; - iscroll-up
;; - iscroll-down
;; - iscroll-next-line
;; - iscroll-previous-line
;;
;; Used to be image-scroll.el, but image-scroll-up/down collide with
;; image.el
;;
;; Limitations:
;;
;; - Scrolling over images in other window sometimes doesn’t work.
;; - If you scroll over an image partially and move point to another
;;   window, sometimes the image jumps back to display completely.
;; - If there is only one line after an image, you can’t scroll that
;;   image down. Why? I have no idea.


;;; Developer
;;
;; About the implementation:
;;
;; Scrolling n lines once is much more efficient than scrolling 1 line
;; n times (see benchmarks at the end of the file). So we prefer
;; scrolling in batch. For that reason, we try to do a ”dry run”
;; before actually scrolling. ”Dry run” means moving point when
;; scrolling over logical lines, and simply incrementing vscroll
;; amount when scrolling over images. When we finished the dry-run, we
;; apply the logical scrolls with `set-window-start` and vscroll
;; amount with `set-window-vscroll`.
;;
;; In practice this is indeed much more efficient when ARG (number of
;; lines to scroll) is larger than 1. That’s important for me because
;; I usually scroll 3 lines at once.

;;; Code:
;;

(require 'cl-lib)

(defvar iscroll-preserve-screen-position
  scroll-preserve-screen-position
  "Whether to preserve screen position when scrolling.
I want to control this for iscroll separately, don’t ask why.")

(defun iscroll-up (&optional arg)
  "Scroll up ARG lines.
Normally just calls `scroll-up'. But if the top of the window is
an image, scroll inside the image. Return the number of logical
lines scrolled."
  (interactive "p")
  (let ((arg (or arg 1))
        (display-lines-scrolled 0)
        (original-point (point))
        (scroll-amount nil)
        (need-to-recalculate-img-height t)
        (preserve-screen-pos iscroll-preserve-screen-position)
        img-height
        hit-end-of-buffer)
    ;; 1) We first do a dry-run: not actually scrolling, just moving
    ;; point and modifying SCROLL-AMOUNT. See Developer section for
    ;; rationale.
    (goto-char (window-start))
    (while (> arg 0)
      ;; Initialize SCROLL-AMOUNT when we arrive at a new line or first
      ;; entered the command.
      (when (null scroll-amount)
        (setq scroll-amount (window-vscroll nil t)))
      ;; `line-pixel-height' is expensive so we try to call it as few
      ;; as possible.
      (when need-to-recalculate-img-height
        (setq img-height (line-pixel-height)
              need-to-recalculate-img-height nil))
      ;; Scroll.
      (if (and (>= img-height (* 2 (default-line-height)))
               (< scroll-amount img-height))
          ;; If we are in the middle of scrolling an image, scroll
          ;; that image.
          (setq scroll-amount
                (min (+ scroll-amount (default-line-height))
                     img-height))
        ;; If we are not on an image or the image is scrolled over,
        ;; scroll display line.
        (cl-incf display-lines-scrolled)
        (setq need-to-recalculate-img-height t)
        ;; We hit the end of buffer, stop.
        (when (not (eq (vertical-motion 1) 1))
          (setq hit-end-of-buffer t)
          (setq arg 0))
        (setq scroll-amount nil))
      (cl-decf arg))
    ;; 2) Finally, we’ve finished the dry-run, apply the result.
    ;;
    ;; The third argument `t' tells redisplay that (point) doesn't
    ;; have to be the window start and be completely visible. That
    ;; allows our vscroll value to survive.
    (set-window-start nil (point) t)
    (if scroll-amount
        (set-window-vscroll nil scroll-amount t)
      (set-window-vscroll nil 0 t))
    ;; 3) Misc stuff.
    ;;
    ;; If the original point is after window-start, it is in the
    ;; visible portion of the window, and is safe to go back to.
    (if (> original-point (window-start))
        (goto-char original-point)
      ;; If not, we just stay at current position, i.e. window-start.
      (setq preserve-screen-pos nil))
    ;; (Maybe) move point to preserve screen position.
    (when preserve-screen-pos
      (vertical-motion display-lines-scrolled))
    ;; Show “error message”.
    (when hit-end-of-buffer
      (message "%s" (error-message-string '(end-of-buffer))))
    display-lines-scrolled))

(defun iscroll-down (&optional arg)
  "Scroll down ARG lines.
Normally just calls `scroll-down'. But if the top of the window is
an image, scroll inside the image. Return the number of logical
lines scrolled. If PRESERVE-SCREEN-POS non-nil, try to preserve
screen position."
  (interactive "p")
  (let ((arg (or arg 1))
        (display-lines-scrolled 0)
        (original-point (point))
        ;; Nil means needs to re-measure.
        (scroll-amount nil)
        (preserve-screen-pos iscroll-preserve-screen-position)
        hit-beginning-of-buffer)
    ;; 1) Dry-run.
    (goto-char (window-start))
    (while (> arg 0)
      (when (null scroll-amount)
        (setq scroll-amount (window-vscroll nil t)))
      (let ((img-height (line-pixel-height)))
        (if (and (>= img-height (* 2 (default-line-height)))
                 (> scroll-amount 0))
            ;; Scroll image.
            (setq scroll-amount
                  (- scroll-amount (default-line-height)))
          ;; Scroll display line.
          (if (not (eq (vertical-motion -1) -1))
              ;; If we hit the beginning of buffer, stop.
              (progn (setq hit-beginning-of-buffer t
                           arg 0))
            (cl-incf display-lines-scrolled)
            ;; If the line we stopped at is an image, we don't want to
            ;; show it completely, instead, modify vscroll and only
            ;; show a bottom strip of it. If we are at the beginning
            ;; of the buffer and `vertical-motion' returns 0, we don't
            ;; want to do this.
            (let ((img-height (line-pixel-height)))
              (if (>= img-height (* 2 (default-line-height)))
                  (setq scroll-amount (- img-height (default-line-height)))
                (setq scroll-amount nil))))))
      (cl-decf arg))
    ;; 2) Apply result.
    (set-window-start nil (point) t)
    (if scroll-amount
        (set-window-vscroll nil scroll-amount t)
      (set-window-vscroll nil 0 t))
    ;; 3) Misc
    ;;
    ;; HACK: There is no fast and reliable way to get the last visible
    ;; point, hence this hack: move point up until it is visible.
    (goto-char original-point)
    ;; Checking point > window-start is important, otherwise we could
    ;; fall into infinite loop. E.g., when point = window-start and
    ;; under the point is an image that is not completely visible.
    (while (and (> (point) (window-start))
                (not (pos-visible-in-window-p (point))))
      (setq preserve-screen-pos nil)
      (vertical-motion -2))
    (when preserve-screen-pos
      (vertical-motion (- display-lines-scrolled)))
    (when hit-beginning-of-buffer
      (message "%s" (error-message-string '(beginning-of-buffer))))
    display-lines-scrolled))

(defvar iscroll--goal-column nil
  "Goal column when scrolling.")

(defun iscroll-forward-line (&optional arg)
  "Smooth `forward-line'.
ARG is the number of lines to move."
  (interactive "p")
  (let* ((arg (or arg 1))
         (abs-arg (abs arg))
         (step (if (> arg 0) 1 -1))
         (scroll-fn (if (> arg 0)
                        #'iscroll-up
                      #'iscroll-down))
         (old-point (point))
         (first-command-p (not (memq last-command
                                     '(iscroll-next-line
                                       iscroll-previous-line))))
         ;; Calculate the goal column. The goal column is either
         ;; inherited from previous calls to this command, or
         ;; calculated by visual column.
         (goal-column (if (or first-command-p (not iscroll--goal-column))
                          (let ((old-point (point)))
                            (vertical-motion 0)
                            (setq iscroll--goal-column
                                  (- old-point (point))))
                        (or iscroll--goal-column 0)))
         ;; We don't want to preserve screen position when moving point.
         (iscroll-preserve-screen-position nil)
         hit-boundary)
    ;; Because in most cases we move into visible portions, we move
    ;; first and check after, this should be faster than check first
    ;; and move after.
    (while (> abs-arg 0)
      ;; Move point.
      (when (not (eq (vertical-motion (cons goal-column step)) step))
        ;; If we hit beginning or end of buffer, stop.
        (setq hit-boundary t
              abs-arg 0))
      (when (not (pos-visible-in-window-p (point)))
        ;; The new point is not visible! Scroll up/down one line to try
        ;; to accommodate that line.
        (funcall scroll-fn 1))
      ;; We scrolled one line but that line is still not fully
      ;; visible, move the point back so that redisplay doesn’t force
      ;; the whole line into visible region. Partially visible is ok,
      ;; completely invisible is not ok.
      (when (and (not (pos-visible-in-window-p (point)))
                 ;; If the image is taller than the window and is the
                 ;; first row of the window, it is ok to leave point
                 ;; on it.
                 (<= (line-pixel-height) (window-text-height nil t)))
        (goto-char old-point)
        (setq hit-boundary nil))
      (cl-decf abs-arg))
    ;; If we hit buffer boundary and didn’t back off, show “error
    ;; message”.
    (when hit-boundary
      (message "%s" (error-message-string
                     (list (if (> arg 0)
                               'end-of-buffer
                             'beginning-of-buffer)))))))

(defun iscroll-next-line (&optional arg _)
  "Smooth `next-line'.
ARG is the number of lines to move."
  (interactive "p")
  (iscroll-forward-line arg))

(defun iscroll-previous-line (&optional arg _)
  "Smooth `previous-line'.
ARG is the number of lines to move."
  (interactive "p")
  (iscroll-forward-line (- (or arg 1))))

(defvar iscroll-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [remap next-line] #'iscroll-next-line)
    ;; (define-key map [remap previous-line] #'iscroll-previous-line)
    map)
  "Mode map for `iscroll-mode'.")

(define-minor-mode iscroll-mode
  "Smooth scrolling over images."
  :global t
  :lighter " IS"
  :keymap iscroll-mode-map
  :group 'scrolling
  (if iscroll-mode
      (progn
        (setq mwheel-scroll-up-function #'iscroll-up
              mwheel-scroll-down-function #'iscroll-down)
        ;; We don’t remap next/previous-line in the minor mode map
        ;; because that shallows ivy’s binding.
        (global-set-key [remap next-line] #'iscroll-next-line)
        (global-set-key [remap previous-line]
                        #'iscroll-previous-line))
    (setq mwheel-scroll-up-function #'scroll-up
          mwheel-scroll-down-function #'scroll-down)
    (global-set-key [remap next-line] nil)
    (global-set-key [remap previous-line] nil)))

;;; Rough Benchmarks
;;
;; Clearly, scrolling in batch has much better performance.

;; (benchmark-run 100 (luna-scroll-down-reserve-point)) ; 1.37
;; (benchmark-run 100 (luna-scroll-up-reserve-point)) ; 0.15

;; (benchmark-run 100 (scroll-up 1)) ; 0.16
;; (benchmark-run 100 (scroll-down 1)) ; 1.32

;; (benchmark-run 50 (scroll-up 2)) ; 0.05
;; (benchmark-run 50 (scroll-down 2)) ; 0.67

;; (benchmark-run 33 (scroll-up 3)) ; 0.05
;; (benchmark-run 33 (scroll-down 3)) ; 0.326

;; (benchmark-run 1 (scroll-up 100)) ; 0.01
;; (benchmark-run 1 (scroll-down 100)) ; 0.04

(provide 'iscroll)

;;; iscroll.el ends here

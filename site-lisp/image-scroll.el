;;; image-scroll.el --- Smooth scrolling over images      -*- lexical-binding: t; -*-

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
;;     M-x image-scroll-mode RET
;;
;; This mode remaps mouse scrolling functions. If you use other
;; commands, you need to adapt them accordingly. See
;; `image-scroll-mode-map' and `image-scroll-mode' for some
;; inspiration.
;;
;; Commands provided:
;;
;; - image-scroll-up
;; - image-scroll-down
;; - image-scroll-next-line
;; - image-scroll-previous-line
;;
;; Limitations:
;;
;; - currently we only look for images at the beginning of a line.

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

(defun image-scroll--image-at (point)
  "Return image at POINT or nil."
  (let ((img (plist-get (text-properties-at point) 'display)))
    (if (and (consp img) (eq (car img) 'image))
        img
      nil)))

(defun image-scroll--to (scroll-amount point)
  "Scroll image at POINT to SCROLL-AMOUNT.
Also set text property at point."
  (set-window-vscroll nil scroll-amount t)
  (put-text-property
   point (min (1+ point) (point-max))
   'image-scroll-amount scroll-amount))

(defun image-scroll--image-height-at (point)
  "Return image height at POINT or nil."
  (when-let ((img (image-scroll--image-at point)))
    (cdr (image-size img t))))

(defun image-scroll--current-scroll-amount (point direction)
  "Return the scroll amount of image at POINT in pixels.
Assumes POINT is at an image. DIRECTION can be either 'up for
'down."
  (let ((amount (or (plist-get (text-properties-at point)
                               'image-scroll-amount)
                    0)))
    ;; If the scroll-amount property doesn’t agree with the actual
    ;; window-vscroll, we are NOT scrolling in this image. In that
    ;; case we must just entered this image, set scroll amount to 0.
    (if (eq direction 'up)
        (if (eq amount (window-vscroll nil t)) amount 0)
      ;; Scroll down.
      (if (and (eq amount (window-vscroll nil t))
               ;; This = is important.
               (>= point (window-start)))
          amount
        ;; If POINT < window start, we are scrolling into this
        ;; image, so just set AMOUNT to IMG-HEIGHT.
        (or (image-scroll--image-height-at point) 0)))))

(defun image-scroll-up (&optional arg)
  "Scroll up ARG lines.
Normally just calls `scroll-up'. But if the top of the window is
an image, scroll inside the image. Return the number of logical
lines scrolled."
  (interactive "p")
  (let ((arg (or arg 1))
        (logical-lines-scrolled 0)
        (original-point (point))
        (scroll-amount nil))
    (goto-char (window-start))
    ;; We first do a dry-run: not actually scrolling, just moving
    ;; point and modifying SCROLL-AMOUNT. See Developer section for
    ;; rationale.
    (while (> arg 0)
      ;; Initialize SCROLL-AMOUNT when we arrive at a new line or first
      ;; entered the command.
      (when (null scroll-amount)
        (setq scroll-amount
              (image-scroll--current-scroll-amount (point) 'up)))
      ;; Scroll.
      (let ((img-height (image-scroll--image-height-at (point))))
        (if (and img-height (< scroll-amount img-height))
            ;; If we are in the middle of scrolling an image, scroll
            ;; that image.
            (setq scroll-amount
                  (min (+ scroll-amount (frame-char-height))
                       img-height))
          ;; If we are not on an image or the image is scrolled over,
          ;; scroll logical line.
          (cl-incf logical-lines-scrolled)
          (vertical-motion 1)
          (setq scroll-amount nil)))
      (cl-decf arg))
    ;; Finally, we’ve finished the dry-run, apply the result.
    ;;
    ;; This `t' is important for the code to take effect. I’m not an
    ;; expert of Emacs redisplay so I don’t know why, but this works.
    (set-window-start nil (point) t)
    ;; This is also important for the code to work.
    (set-window-vscroll nil 0 t)
    (when scroll-amount
      (image-scroll--to scroll-amount (point)))
    ;; If the original point is out of visible portion, move it in.
    (when (> original-point (window-start))
      (goto-char original-point))
    logical-lines-scrolled))

(defun image-scroll-down (&optional arg)
  "Scroll down ARG lines.
Normally just calls `scroll-down'. But if the top of the window is
an image, scroll inside the image. Return the number of logical
lines scrolled."
  (interactive "p")
  (let ((arg (or arg 1))
        (logical-lines-scrolled 0)
        (original-point (point))
        ;; Nil means needs to re-measure.
        (scroll-amount nil))
    (goto-char (window-start))
    (while (> arg 0)
      (when (null scroll-amount)
        (setq scroll-amount
              (image-scroll--current-scroll-amount (point) 'down)))
      (let ((img-height (image-scroll--image-height-at (point))))
        (if (and img-height (> scroll-amount 0))
            ;; Scroll image.
            (setq scroll-amount (- scroll-amount (frame-char-height)))
          ;; Scroll logical line.
          (cl-incf logical-lines-scrolled)
          (vertical-motion -1)
          (setq scroll-amount nil)))
      (cl-decf arg))
    (set-window-start nil (point) t)
    (set-window-vscroll nil 0 t)
    (when scroll-amount
      (image-scroll--to scroll-amount (point)))
    ;; HACK: There is no fast and reliable way to get the last visible
    ;; point, hence this hack: move point up until it is visible.
    (goto-char original-point)
    ;; Checking point > window-start is important, otherwise we could
    ;; fall into infinite loop. E.g., when point = window-start and
    ;; under the point is an image that is not completely visible.
    (while (and (> (point) (window-start))
                (not (pos-visible-in-window-p (point))))
      (vertical-motion -2))
    logical-lines-scrolled))

(defvar image-scroll--goal-column nil
  "Goal column when scrolling.")

(defun image-scroll-forward-line (&optional arg)
  "Smooth `forward-line'.
ARG is the number of lines to move."
  (interactive "p")
  (let* ((arg (or arg 1))
         (abs-arg (abs arg))
         (step (if (> arg 0) 1 -1))
         (scroll-fn (if (> arg 0)
                        #'image-scroll-up
                      #'image-scroll-down))
         (old-point (point))
         (first-command-p (not (memq last-command
                                     '(image-scroll-next-line
                                       image-scroll-previous-line)))))
    ;; Because in most cases we move into visible portions, we move
    ;; first and check after, this should be faster than check first
    ;; and move after.
    (while (> abs-arg 0)
      ;; The goal column is either inherited from previous calls to
      ;; this command, or calculated by visual column.
      (if (or first-command-p (not image-scroll--goal-column))
          (let ((old-point (point)))
            (vertical-motion 0)
            (setq image-scroll--goal-column (- old-point (point)))))
      (vertical-motion (cons image-scroll--goal-column step))
      ;; The new point is not visible! Scroll up/down one line to try
      ;; to accommodate that line. TODO: `pos-visible-in-window-p' is
      ;; very slow, how can we replace it? `pos-visible-in-window-p'
      ;; doesn’t seem to be the culprit (judging from source and
      ;; profiling), it might be because it invokes redisplay which is
      ;; slow?
      (when (not (pos-visible-in-window-p (point)))
        (funcall scroll-fn 1))
      ;; We scrolled one line but that line is still not fully
      ;; visible, move the point back so that redisplay doesn’t force
      ;; the whole line into visible region.
      (when (not (pos-visible-in-window-p (point)))
        (goto-char old-point))
      (cl-decf abs-arg))))

(defun image-scroll-next-line (&optional arg _)
  "Smooth `next-line'.
ARG is the number of lines to move."
  (interactive "p")
  (image-scroll-forward-line arg))

(defun image-scroll-previous-line (&optional arg _)
  "Smooth `previous-line'.
ARG is the number of lines to move."
  (interactive "p")
  (image-scroll-forward-line (- (or arg 1))))

(defvar image-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [remap next-line] #'image-scroll-next-line)
    ;; (define-key map [remap previous-line] #'image-scroll-previous-line)
    map)
  "Mode map for `image-scroll-mode.'")

(define-minor-mode image-scroll-mode
  "Smooth scrolling over images."
  :global t
  :lighter " IS"
  :keymap image-scroll-mode-map
  :group 'scrolling
  (if image-scroll-mode
      (progn
        (setq mwheel-scroll-up-function #'image-scroll-up
              mwheel-scroll-down-function #'image-scroll-down)
        ;; We don’t remap next/previous-line in the minor mode map
        ;; because that shallows ivy’s binding.
        (global-set-key [remap next-line] #'image-scroll-next-line)
        (global-set-key [remap previous-line]
                        #'image-scroll-previous-line))
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


(provide 'image-scroll)

;;; image-scroll.el ends here

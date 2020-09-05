;;; delicate-click.el --- Delicate click position      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; If you set ‘cursor-type’ to bar and clicks on the right side of a
;; character, you would expect the point to be set after that
;; character. Normally that’s not the case, this package provides
;; ‘delicate-click-mode’ that makes Emacs to do the right thing.
;;
;; To use:
;;     M-x delicate-click-mode RET

;;; Code:
;;

(defun adjust-point-after-click (event &optional _)
  "Adjust point.
Adjust point depending on which portion of the character the
cursor clicked on, if on the right half, move point after.
EVENT is the mouse event."
  (let* ((posn (event-end event))
         (x (car (posn-object-x-y posn)))
         (w (car (posn-object-width-height posn))))
    ;; ‘mouse-set-point’ is called twice when you click mouse, first
    ;; in ‘down-mouse-1’, called by ‘mouse-drag-region’ ->
    ;; ‘mouse-drag-track’ to set point, second in ‘mouse-1’, when
    ;; mouse released and Emacs realized that this is a click event.
    ;; We want to adjust point in both cases.
    (when (and (null (posn-object posn))
               (> x (/ w 2))
               (not (eq (char-after) ?\n)))
      (forward-char))))

(define-minor-mode delicate-click-mode
  "Accurate point position on click.
That is, if you click on the right half of a character, the point
is set to after it."
  :global t
  :lighter ""
  (if delicate-click-mode
      (advice-add 'mouse-set-point :after #'adjust-point-after-click)
    (advice-remove 'mouse-set-point #'adjust-point-after-click)))

(provide 'delicate-click)

;;; delicate-click.el ends here

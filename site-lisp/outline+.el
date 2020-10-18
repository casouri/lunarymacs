;;; outline+.el --- Outline improvements      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Provide heading cycling commands for outline.
;; - outline-cycle
;; - outline-cycle-buffer

;;; Code:
;;

(defun outline--cycle-state ()
  "Return the cycle state of current heading.
Return either 'hide-all, 'headings-only, or 'show-all."
  (save-excursion
    (let (start end ov-list heading-end)
      (outline-back-to-heading)
      (setq start (point))
      (outline-end-of-heading)
      (setq heading-end (point))
      (outline-end-of-subtree)
      (setq end (point))
      (setq ov-list (cl-remove-if-not
                     (lambda (o) (eq (overlay-get o 'invisible) 'outline))
                     (overlays-in start end)))
      (cond ((eq ov-list nil) 'show-all)
            ;; (eq (length ov-list) 1) wouldn’t work: what if there is
            ;; one folded subheading?
            ((and (eq (overlay-end (car ov-list)) end)
                  (eq (overlay-start (car ov-list)) heading-end))
             'hide-all)
            (t 'headings-only)))))

(defun outline-has-subheading-p ()
  "Return t if this heading has subheadings, nil otherwise."
  (save-excursion
    (outline-back-to-heading)
    (< (save-excursion (outline-next-heading) (point))
       (save-excursion (outline-end-of-subtree) (point)))))

(defun outline-cycle ()
  "Cycle between “hide all”, “headings only” and “show all”.

“Hide all” means hide all subheadings and their bodies.
“Headings only” means show sub headings but not their bodies.
“Show all” means show all subheadings and their bodies."
  (interactive)
  (condition-case nil
      (pcase (outline--cycle-state)
        ('hide-all (if (outline-has-subheading-p)
                       (progn (outline-show-children)
                              (message "Only headings"))
                     (outline-show-subtree)
                     (message "Show all")))
        ('headings-only (outline-show-subtree)
                        (message "Show all"))
        ('show-all (outline-hide-subtree)
                   (message "Hide all")))
    ;; If error: "Before first heading" occurs, ignore it.
    (error nil)))

(defvar-local outline--cycle-buffer-state 'show-all
  "Interval variable used for tracking buffer cycle state.")

(defun outline-cycle-buffer ()
  "Cycle the whole buffer like in ‘outline-cycle’."
  (interactive)
  (pcase outline--cycle-buffer-state
    ('show-all (save-excursion
                 (outline-hide-sublevels
                  (or (ignore-errors
                        (outline-back-to-heading)
                        (outline-level))
                      1)))
               (setq outline--cycle-buffer-state 'top-level)
               (message "Top level headings"))
    ('top-level (outline-show-all)
                (outline-hide-region-body (point-min) (point-max))
                (setq outline--cycle-buffer-state 'all-heading)
                (message "All headings"))
    ('all-heading (outline-show-all)
                  (setq outline--cycle-buffer-state 'show-all)
                  (message "Show all"))))

(provide 'outline+)

;;; outline+.el ends here

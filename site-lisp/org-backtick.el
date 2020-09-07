;;; org-backtick.el --- Display ~ and = as backticks in Org mode      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

(defun org-backtick-fontify (beg end)
  "Fontify ~ and = between BEG and END."
  (goto-char beg)
  (cl-labels ((verbatim-p (face)
                          (and (consp face)
                               (or (memq 'org-code face)
                                   (memq 'org-verbatim face))))
              (face-at (point)
                       (plist-get (text-properties-at point)
                                  'face)))
    (while (re-search-forward (rx (or "~" "=")) end t)
      (let* ((face (face-at (match-beginning 0)))
             (face-before (face-at (max (1- (match-beginning 0))
                                        (point-min))))
             (face-after (face-at (min (1+ (match-beginning 0))
                                       (point-max)))))
        ;; Make it display backtick if the face indicates that
        ;; it’s a code/verbatim delimiter.
        (if (or (and (verbatim-p face)
                     (not (verbatim-p face-before)))
                (and (verbatim-p face)
                     (not (verbatim-p face-after))))
            (put-text-property
             (match-beginning 0) (match-end 0) 'display "`")
          ;; Clean up our face if it’s not a code/verbatim
          ;; delimiter anymore.
          (when (equal (plist-get (text-properties-at point) 'display)
                       "`")
            (put-text-property
             (match-beginning 0) (match-end 0) 'display nil))))))
  (cons 'jit-lock-bounds (cons beg end)))

(define-minor-mode org-backtick-mode
  "Display ~ and = as backticks."
  :lighter ""
  (if org-backtick-mode
      ;; We want to run after org-mode’s font-lock function.
      (add-hook 'jit-lock-functions #'org-backtick-fontify 91 t)
    (remove-hook 'jit-lock-functions #'org-backtick-fontify t))
  (jit-lock-refontify))

;;; Code:
;;

(provide 'org-backtick)

;;; org-backtick.el ends here

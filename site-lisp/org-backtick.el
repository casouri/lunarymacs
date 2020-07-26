;;; org-backtick.el --- Display ~ and = as backticks in Org mode      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

(defun org-backtick-fontify (beg end)
  "Fontify ~ and = between BEG and END."
  (goto-char beg)
  (while (re-search-forward (rx (or "~" "=")) end t)
    (let* ((text-props (text-properties-at (match-beginning 0)))
           (face (plist-get text-props 'face)))
      ;; Make it display backtick if the face indicates that
      ;; it’s a code/verbatim delimiter.
      (if (or (equal face '(org-code))
              (equal face '(org-verbatim)))
          (put-text-property
           (match-beginning 0) (match-end 0) 'display "`")
        ;; Clean up our face if it’s not a code/verbatim
        ;; delimiter anymore.
        (when (equal (plist-get text-props 'display) "`")
          (put-text-property
           (match-beginning 0) (match-end 0) 'display nil)))))
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

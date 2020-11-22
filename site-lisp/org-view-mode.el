;;; org-view-mode.el --- View Org Mode files with prettification      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This mode hides some markups in Org Mode. Currently this is
;; implemented by remapping certain faces to an invisible face and
;; changing some variables.

;;; Code:

(defvar org-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'org-view-mode)
    (define-key map "?" #'describe-mode)
    map)
  "Keymap for `org-view-mode'.")

(define-minor-mode org-view-mode
  "View Org Mode files with prettification."
  :lighter ""
  :keymap org-view-mode-map
  (if (derived-mode-p 'org-mode)
      (let* ((invisible-face
              (list :foreground (face-attribute 'default :background)))
             (remapping-list `((org-block-begin-line . ,invisible-face)
                               (org-block-end-line . ,invisible-face)
                               ;; (org-meta-line . ,invisible-face)
                               )))
        (if org-view-mode
            (progn
              (setq-local org-hide-emphasis-markers t
                          face-remapping-alist
                          (append remapping-list face-remapping-alist)
                          buffer-read-only t)
              (org-display-inline-images))
          (kill-local-variable 'org-hide-emphasis-markers)
          (kill-local-variable 'buffer-read-only)
          ;; Font-rescale also uses this variable, so don't simply kill
          ;; the local variable.
          (dolist (remapping remapping-list)
            (setq-local face-remapping-alist
                        (remove remapping face-remapping-alist)))
          (org-remove-inline-images))
        (jit-lock-refontify))
    (user-error "Not in Org Mode")))

;; Currently not used.
(defun org-view-mode-fontify ()
  "Hide stuff for Org View Mode."
  (goto-char (point-min))
  (dolist (face '(org-block-begin-line
                  org-block-end-line
                  org-meta-line))
    (let (match)
      (while (setq match (text-property-search-forward 'face face))
        (let ((ov (make-overlay (prop-match-beginning match)
                                (prop-match-end match))))
          (overlay-put ov 'invisible t)
          (overlay-put ov 'org-view-mode t))))))

(provide 'org-view-mode)

;;; org-view-mode.el ends here

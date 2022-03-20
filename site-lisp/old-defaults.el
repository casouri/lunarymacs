;;; old-defaults.el --- Get off my lawn!!  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; We use custom themes to implement this because 1) this way we don’t
;; reinvent the wheels and 2) we don’t need advanced features like
;; choosing what variables to not revert or only revert variables or
;; faces.

;;; Code:


(defvar old-defaults--changelog
  '((emacs-28
     :variable
     ((kill-ring-max 60 t)
      (gnus-treat-fold-headers nil))
     :face
     ((mode-line-inactive (:inherit mode-line) t "In Emacs 29.1, mode-line defaults to proportion font, this setting sets it back to monospace")
      (mode-line-active (:inherit mode-line) t "In Emacs 29.1, mode-line defaults to proportion font, this setting sets it back to monospace")
      (gnus-header (:inherit 'unspecified) t "In Emacs 29.1, gnus-header-* defaults to proportion font, this setting sets them back to monospace"))))
  "Stores old default values.
An alist of (EMACS-VERSION . SETTINGS) where EMACS-VERSION is a
symbol representing a particular version and SETTINGS is a plist
(:variable VARIABLES :face FACES). VARIABLES is a list of custom
variable forms suitable for ‘custom-theme-set-variables’ and
FACES is a list of face forms suitable for
‘custom-theme-set-faces’.

The alist should be ordered in ascending release time, i.e., the
oldest release at the front.")

(defmacro old-defaults-deftheme (version)
  "Define the theme that restores defaults to VERSION.
VERSION is a version symbol that appears in
‘old-defaults--changelog’."
  (when-let* ((theme-name (intern (format "old-defaults-%s" version)))
              (changes-needed
               (cl-loop for change in old-defaults--changelog
                        for idx = 0 then (1+ idx)
                        if (eq (car change) version)
                        return (cl-subseq old-defaults--changelog
                                          0 (1+ idx))))
              ;; CHANGES-NEEDED looks like ((emacs-28 . SETTINGS)
              ;; (emacs-29 . SETTINGS) ...).
              (var-changes-needed
               (cl-loop for change in changes-needed
                        append (plist-get (cdr change) :variable)))
              (face-changes-needed
               (cl-loop for change in changes-needed
                        append (plist-get (cdr change) :face))))
    `(progn
       (deftheme ,theme-name
         ,(format "Restore variable and face defaults to %s" version))
       (custom-theme-set-variables
        ',theme-name
        ,@var-changes-needed)
       (custom-theme-set-faces
        ',theme-name
        ,@face-changes-needed)
       (provide-theme ',theme-name))))

(provide 'old-defaults)

;;; old-defaults.el ends here

;;; accent-char.el --- Spanish accent char      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Written for Chen Yi for typing Spanish.

;;; Code:

(defvar accent-char-mapping
  '(?a ?á ?e ?é ?i ?í ?o ?ó ?u ?ú ?ú ?ü ?U ?Ü ?A ?Á
       ?E ?É ?I ?Í ?O ?Ó ?U ?Ú ?n ?ñ ?N ?Ñ ?! ?¡ ?? ?¿)
  "Mapping from normal char to accented char.")

(defun accent-char-before ()
  "Accent the character before, if possible."
  (interactive)
  (when (looking-back (rx (or (seq (any ?a ?e ?i ?o ?u
                                        ?A ?E ?I ?O ?U
                                        ?n ?N ?? ?!)
                                   "''")
                              "ú'"))
                      (- (point) 3))
    (let ((char (char-after (match-beginning 0))))
      (delete-region (match-beginning 0)
                     (match-end 0))
      (insert (plist-get accent-char-mapping char)))))

(define-minor-mode accent-char-mode
  "Type accented char."
  :lighter ""
  :global t
  (if accent-char-mode
      (add-hook 'post-self-insert-hook #'accent-char-before -50)
    (remove-hook 'post-self-insert-hook #'accent-char-before)))

(provide 'accent-char)

;;; accent-char.el ends here

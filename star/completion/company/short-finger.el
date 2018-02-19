;;; short-finger.el --- Automatic syntax replacement for short fingers

;;; Commentary:

;; This package provides a minor mode `short-finger-mode' that can automatically
;; replace syntax.
;;
;; For example, with a rule of "replace everything between "sl" before and "sl"
;; after with [[text in between][text in between]]", you can achieve the effect of
;; sl source link sl ==> [[source link][source link]]
;;
;; To use this package, simply put the file under load-path and require it.
;; If you want to use autoload, autoload `short-finger-mode'.
;;
;; To enable minor mode use command `shor-finger-mode'.
;;
;; For every major mode you want to enable short-finger for, customize variable
;; `short-finger-replace-book-for-MAJOR-MODE'. Substitue `MAJOR-MODE' with
;; symbol name of major mode(org-mode, markdown-mode, etc).
;;
;; The format of `short-finger-replace-book-for-MAJOR-MODE' is:
;; (
;;   ("indicater before" "indicater after" "replacement template")
;;   ("indicater before" "indicater after" "replacement template")
;; )
;; replace template can use "%s" to substitude text being surrounded by
;; before and after indicater.
;;
;; Note: Remember to include white space around your template.
;; Because both indicater will be removed with the spaces surround them
;; in text. That is " sl " ==> "".
;;
;; So if you set `replace-book' to ("sl" "sl" "[%s]"),
;; "test test sl something sl test test" => "test test[somthing]test test"
;; The reason of this decision is I don't want to make the function too "clever"
;; so it gets in the way. This way you can have more controll in what to replace.


;;; Code:

(defvar short-finger-replace-book-for-org-mode '(("sl" "sl" " [[%s][%s]] "))
  "A list of cons of (key . replacement) for replacing in org mode.

In replacement, use `format' compatible %-sequnce regarding surrounded text
as substitute.

For example (\"sc\" . \"```%s```\") replace
\" sc some code sc\" into \"```some code```\".")

(defvar short-finger-replace-book-for-markdown-mode '(("sc" "sc" " ```%s``` "))
  "A list of cons of (key . replacement) for replacing in markdown mode.

In replacement, use `format' compatible %-sequnce regarding surrounded text
as substitute.

For example (\"sc\" . \"```%s```\") replace
\" sc some code sc\" into \"```some code```\".")

(defun short-finger-format-line (key-replace-con)
  "Format current line based on KEY-REPLACE-CON."
  (interactive)
  (save-excursion
    (let ((match-str ; the pattern we are going to replace
           (format "[ \n\t]%s[ \n\t]\\(.+?\\)[ \n\t]%s[ \n\t]"
                   (nth 0 key-replace-con)
                   (nth 1 key-replace-con))))
      (move-beginning-of-line nil) ; to search whole line
      (while (re-search-forward match-str (line-end-position) t)
        (replace-match
         (replace-regexp-in-string
          "\\%s"
          (match-string-no-properties 1)
          (nth 2 key-replace-con))
         nil t)))))

(defun short-finger-auto-format (short-finger-active-book)
  "If char before point is space(just typped space)
and word before space match to ‘key-replace-con’,
call `short-finger-format-line'.

Supposed to be added to post-`self-insert-hook'"
  (if (or (eq ?\s (char-before)) (eq ?\n (char-before)))
      (dolist (key-replace-con short-finger-active-book)
        (if (char-equal (string-to-char
                         (substring-no-properties
                          (nth 1 key-replace-con)
                          -1))
                        (char-before (- (point) 1)))
            (short-finger-format-line key-replace-con)))))

;;;###autoload
(define-minor-mode short-finger-mode
  "Activate short-finger-mode. So matched pattern will be replaced automatically.

`short-finger-active-book' will be set to corresponding major mode book.

i.e. `short-finger-replace-book-for-org-mode' for `org-mode'.
Note: name have to exactily match( org-mode -> org-mode)."
  nil ; default disabled
  :lighter " SF"
  (add-hook 'post-self-insert-hook
            (lambda ()
              (short-finger-auto-format
               (eval (intern-soft (format
                             "short-finger-replace-book-for-%s"
                             (symbol-name major-mode))))))
            t t)
  )

(provide 'short-finger)

;;; short-finger.el ends here

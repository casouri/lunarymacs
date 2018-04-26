;;; -*- lexical-binding: t -*-



;; not used anymore
;;;###autoload
(defun moon/query-replace-region ()
  "Query replace selected region."
  (interactive)
  (when (evil-visual-state-p) (evil-normal-state 1))
  (query-replace-regexp (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))
                 (completing-read "Replace to: " ())
                 ))

;; not used anymore
;;;###autoload
(defun moon/query-relace-point ()
  "Query replace thing at point."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (query-replace-regexp word
                   (completing-read (format "Replace \"%s\" to: " word) ())
                   nil (beginning-of-line))))

;;;###autoload
(defun moon/clear-evil-search ()
  "Clear highlights of matched patterns."
  (interactive)
  (pcase evil-search-module
        ('isearch (evil-search-highlight-persist-remove-all))
        ('evil-search (evil-ex-nohighlight))))

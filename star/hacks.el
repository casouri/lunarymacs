;; -*- lexical-binding: t -*-

;;; Helpful

(defvar helpful-html-manual-dir "/Users/yuan/emacs/doc/lispref/elisp.html"
  "Absolute path to the directory where HTML manuals are.")

(define-minor-mode helpful-html-manual-mode
  "View HTML manuals in helpful."
  :lighter ""
  :global t
  (if helpful-html-manual-mode
      (advice-add #'helpful--manual :override #'helpful--html-manual)
    (advice-remove #'helpful--manual #'helpful--html-manual)))

(defun helpful--html-manual (button)
  "Open the HTML manual for the symbol that this BUTTON represents."
  (let ((sym (symbol-name (button-get button 'symbol)))
        (index-file (expand-file-name "Index.html" helpful-html-manual-dir))
        manual-page)
    (with-temp-buffer
      (insert-file-contents index-file)
      (goto-char (point-min))
      (if (not (re-search-forward
                (format "<a href=\"\\(.*\\)\"><code>%s</code>" sym) nil t))
          (message "No manual index for %s" sym)
        (message "%s" (setq manual-page (expand-file-name
                                         (match-string 1) helpful-html-manual-dir)))
        (shell-command-to-string (format "open 'file://%s'" manual-page))))))

(with-eval-after-load 'helpful
  (helpful-html-manual-mode))

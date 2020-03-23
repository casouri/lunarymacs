;; -*- lexical-binding: t -*-

;;; Helpful

(defvar helpful-html-manual-base-url "https://archive.casouri.cat/emacs-manuals/master/elisp"
  "Base URL for the online manual. No trailing slash.")

(defvar helpful-html-manual-index (expand-file-name "site-lisp/index.html" user-emacs-directory))

(define-minor-mode helpful-html-manual-mode
  "View HTML manuals in helpful."
  :lighter ""
  :global t
  (if helpful-html-manual-mode
      (advice-add #'helpful--manual :around #'helpful--html-manual)
    (advice-remove #'helpful--manual #'helpful--html-manual)))

(defun helpful--html-manual (oldfn button)
  "Open the HTML manual for the symbol that this BUTTON represents."
  (let ((sym (symbol-name (button-get button 'symbol)))
        (index-file helpful-html-manual-index))
    (with-temp-buffer
      (insert-file-contents index-file)
      (goto-char (point-min))
      (if (not (re-search-forward
                (format "<a href=\"\\(.*\\)\"><code>%s</code>" sym) nil t))
          (funcall oldfn button)
        (browse-url
         (format "%s/%s" helpful-html-manual-base-url (match-string 1)))))))

(with-eval-after-load 'helpful
  (helpful-html-manual-mode))

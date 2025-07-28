;;; eclectic-quote.el --- My take on electric-quote-mode  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'rx)

(defun eclectic-quote--point-in-text-p ()
  "Return non-nil if point is in text.

“Text” includes comment and string in prog-mode, and any text in
text-mode."
  (or (derived-mode-p 'text-mode)
      (nth 8 (syntax-ppss))
      (save-excursion
        (beginning-of-line)
        (and comment-start-skip
             ;; FIXME: This doesn't work for the case where there are
             ;; two matches of comment-start-skip, and the first one
             ;; is, say, inside a string. We need to call
             ;; re-search-forward repeatedly until either reached EOL
             ;; or (nth 4 (syntax-ppss)) returns non-nil. But this
             ;; case is rare enough that I don’t want to bother.
             (re-search-forward comment-start-skip (pos-eol) t)
             (nth 8 (syntax-ppss))))
      (save-excursion
        (beginning-of-line)
        (and (re-search-forward "\\s-*\\s<" (line-end-position) t)
             (nth 8 (syntax-ppss))))))

(defun eclectic-quote--post-command ()
  "Adjust inserted quotes after each insertion."
  (when (and (memq this-command '(self-insert-command
                                  org-self-insert-command))
             (eclectic-quote--point-in-text-p))
    (cond
     ((and (looking-back (rx "’'") 2))
      (backward-delete-char 2)
      (insert "‘’"))
     ((and (looking-back (rx "'") 1)
           (not (looking-at (rx "'"))))
      (backward-delete-char 1)
      (insert "’"))
     ((and (looking-back (rx "\"") 1)
           (looking-at (rx "\"") 1)
           ;; If user inserted "", the previous
           ;; ‘eclectic-quote--point-in-text-p’ guard always return
           ;; non-nil, so we need to check if we’re actually in a text
           ;; context.
           (save-excursion
             (backward-char)
             (eclectic-quote--point-in-text-p)))
      (delete-region (1- (point)) (1+ (point)))
      (insert "“”")
      (goto-char (1- (point))))
     ((looking-back (rx "”\"") 2)
      (backward-delete-char 2)
      (insert "“”"))
     ((and (looking-back (rx "\"") 1)
           (not (looking-at (rx "\"") 1)))
      (backward-delete-char 1)
      (insert "”")))))

;;;###autoload
(define-minor-mode eclectic-quote-minor-mode
  "An alternative ‘electric-quote-mode’ that frees the backtick.

Typing ' inserts ’
Typing '' inserts ‘’
Typing \" inserts ”
typing \"\" inserts “”."
  :lighter ""
  (if eclectic-quote-minor-mode
      (add-hook 'post-command-hook #'eclectic-quote--post-command 0 t)
    (remove-hook 'post-command-hook #'eclectic-quote--post-command t)))

(provide 'eclectic-quote)

;;; eclectic-quote.el ends here

;; -*-lexical-binding: t -*-

(defvar c-style-alist)

(defun c-style-by-indent (indent)
  "Return the first C style with INDENT indent steps."
  (catch 'ret
    (dolist (style c-style-alist)
      (when (eq (alist-get 'c-basic-offset style)
                indent)
        (throw 'ret (car style))))))

(defvar guess-indent-fn-alist
  '((c-mode . (lambda ()
                (save-excursion
                  (goto-char (point-min))
                  (if (re-search-forward "{\n" nil t)
                      (if-let* ((indent (current-indentation))
                                (style (luna-c-style-by-indent
                                        indent)))
                          (progn (c-set-style style)
                                 (message "Set to %s indent style w/ %d indent steps"
                                          style indent))
                        (message "Can’t find style with %d indent steps"
                                 indent))
                    (message "Can’t guess indent"))))))
  "An alist of major modes and indent guessing & setting functions.")

(defun guess-indent ()
  "Guess the indent of current buffer and set to it."
  (interactive)
  (when-let ((fn (alist-get major-mode guess-indent-fn-alist)))
    (funcall fn)))

(provide 'guess-indent)

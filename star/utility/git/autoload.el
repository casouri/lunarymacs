;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/gitlog ()
  "Generate a org file containing current git repo's log."
  (interactive)
  (find-file "./gitlog.org")
  (insert-buffer-substring (shell-command-to-string
   "git log --pretty=format:'** %h - %s%n%b' > gitlog.org")))

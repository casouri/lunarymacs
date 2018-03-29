;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/gitlog ()
  "Generate a temp org-mode buffer containing current git repo's log."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*gitlog*"))
  (org-mode)
  (insert (shell-command-to-string
   "git log --pretty=format:'** %h - %s%n%b'")))

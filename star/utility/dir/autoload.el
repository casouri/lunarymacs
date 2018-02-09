;;;###autoload
(defun moon/toggle-hidden-file ()
  "Toggle show/hide hidden files in dir explorer.

Affect both ranger and neotree."
  (interactive)
  (setq ranger-show-hidden t)
  (setq neo-show-hidden-files t))

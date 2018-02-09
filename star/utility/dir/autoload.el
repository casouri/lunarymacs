;;;###autoload
(defun moon/toggle-hidden-file ()
  "Toggle show/hide hidden files in dir explorer.

Affect both ranger and neotree."
  (interactive)
  (setq ranger-show-hidden (not ranger-show-hidden))
  (setq neo-show-hidden-files (not neo-show-hidden-files)))

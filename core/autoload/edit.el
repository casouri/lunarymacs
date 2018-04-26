;;;###autoload
(defun moon/toggle-format-on-save ()
  "Toggle format-on-save feature of current major mode.

requires `moon-format-on-save-var-book'."
  (interactive)
  (setq moon-format-on-save (not moon-format-on-save))
  (eval (plist-get moon-format-on-save-func-book major-mode)))

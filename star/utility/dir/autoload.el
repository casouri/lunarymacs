;;;###autoload
(defun moon/toggle-hidden-file ()
  "Toggle show/hide hidden files in dir explorer.

Affect both ranger and neotree."
  (interactive)
  ;; make sure moon-show-hidden is bounded
  (unless (boundp 'moon-show-hidden-p)
    (defvar moon-show-hidden-p
      ;; set to any exist value, if nor exist, set to nil
      (if (boundp 'ranger-show-hidden)
          ranger-show-hidden
        (if (boundp 'neo-show-hidden-files)
            neo-show-hidden-files
          nil))
      "Whether to show hidden files in file explorers like neotree or ranger."))
  (setq moon-show-hidden-p (not moon-show-hidden-p))
  (setq ranger-show-hidden moon-show-hidden-p)
  (setq neo-show-hidden-files moon-show-hidden-p))

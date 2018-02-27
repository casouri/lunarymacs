;;;###autoload
(defun moon/redraw-homepage ()
  (interactive)
  (kill-buffer moon-homepage-buffer)
  (get-buffer-create moon-homepage-buffer)
  (switch-to-buffer moon-homepage-buffer)
  (moon/draw-homepage)
  )

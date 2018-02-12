;;;###autoload
(defun moon/kill-other-buffer ()
  "Kill all other buffers (besides the current one).

If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  ;; copied from doom-emacs
  (interactive)
  (let ((buffers (buffer-list))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (moon-kill-buffer-and-window buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun moon-kill-buffer-and-window (buffer)
  ;; copied from doom-emacs
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun moon/kill-helper ()
  (interactive)
  "Kill Helper buffer"
  (moon-kill-buffer-and-window "*Help*"))

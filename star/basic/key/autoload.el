;;;###autoload
(defun moon/quit-evreything ()
  (interactive)
  (evil-force-normal-state)
  (keyboard-quit))

;;;###autoload
(defun switch-to-prev-buffer ()
"Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
(interactive)
(switch-to-buffer (other-buffer (current-buffer) 1)))

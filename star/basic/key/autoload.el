;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/quit-evreything ()
  (interactive)
  (evil-force-normal-state)
  (keyboard-quit))

;;;###autoload
(defun moon/switch-between-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (if moon-jumped
      (progn
	(next-buffer)
	(setq moon-jumped nil)
      )
    (previous-buffer)
    (setq moon-jumped t))
  )

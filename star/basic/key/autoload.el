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
  (defvar moon-jumped nil
    "Have you jumped yet?
Used to jump back and forth between two buffers.")
  (if moon-jumped
      (progn
	(next-buffer)
	(setq moon-jumped nil)
      )
    (previous-buffer)
    (setq moon-jumped t))
  )

;;;###autoload
(defun moon/close-help ()
  "Close help buffer"
  (interactive)
  (kill-buffer "*Help*"))

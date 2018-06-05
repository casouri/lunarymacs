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

;;;###autoload
(defun moon/open-init-file ()
  "Open init.el."
  (interactive)
  (find-file (concat moon-emacs-d-dir "init.el")))

;;;###autoload
(defun moon/compare-init-to-example ()
  "Compare init.el to init.example.el to get the latest update."
  (interactive)
  (ediff (concat moon-emacs-d-dir "init.el")
         (concat moon-emacs-d-dir "init.example.el")))

;;;###autoload
(defun moon/open-in-iterm ()
  "Open current dir in iTerm2."
  (interactive)
  (shell-command-to-string (format "open %s -a iterm" default-directory)))

;;;###autoload
(defun moon/open-in-finder ()
  "Open current dir in Finder."
  (interactive)
  (shell-command-to-string (format "open %s" default-directory)))

;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;;;###autoload
(defun moon/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

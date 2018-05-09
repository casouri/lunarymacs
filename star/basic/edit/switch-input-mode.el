;;; -*- lexical-binding: t -*-

(defvar switch-input-mode-second-input "com.apple.inputmethod.SCIM"
  "Name of the second input.")

(defvar switch-input-mode-first-input "com.apple.keyboardlayout.all"
  "Name of the first (primary) input.")

(defun switch-input-to (input-name)
  "Switch input to INPUT-NAME"
  (shell-command-to-string (concat "swim use " input-name)))

(defun switch-input-to-second-input ()
  "Switch input to `switch-input-mode-second-input'."
  (interactive)
  (switch-input-to switch-input-mode-second-input)
  (message "switched to second input method."))

(defun switch-input-to-first-input ()
  "Switch input to `switch-input-mode-first-input'."
  (interactive)
  (switch-input-to switch-input-mode-first-input)
  (message "switched to first input method."))

(define-minor-mode switch-input-mode
  "A Minor mode that switches input method to a different one in insert state."
  :lighter "SI"
  (if switch-input-mode
      (progn
        (add-hook 'evil-insert-state-exit-hook #'switch-input-to-first-input nil t)
        (add-hook 'evil-insert-state-entry-hook #'switch-input-to-second-input nil t))
    (remove-hook 'evil-insert-state-exit-hook #'switch-input-to-first-input t)
    (remove-hook 'evil-insert-state-entry-hook #'switch-input-to-second-input t)))

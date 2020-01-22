;; -*- lexical-binding: t -*-

;;; Key

(with-eval-after-load 'luna-general-config
  (general-define-key
   :keymaps 'dired-mode-map
   "b" #'dired-up-directory
   "q" #'luna-quit-window
   "C-c C-s" #'dired-narrow
   "C-c C-o" #'luna-dired-open-file-at-point))

;;; Config

(add-hook 'dired-mode-hook #'auto-revert-mode)
(setq dired-dwim-target t)

;;; Function

(defun luna-dired-open-file-at-point ()
  (interactive)
  (if-let ((file (dired-file-name-at-point)))
      (shell-command (format "open %s" file))
    (message "Not file found at point")))


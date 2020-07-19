;; -*- lexical-binding: t -*-

;;; Key

(with-eval-after-load 'luna-general-config
  (general-define-key
   :keymaps 'dired-mode-map
   "b" #'dired-up-directory
   "q" #'luna-quit-window
   "C-c C-o" #'luna-dired-open-file-at-point
   "s-v" #'trivial-copy-paste
   "s-c" #'trivial-copy-copy
   "s-M-v" #'trivial-copy-move
   ;; command+shift+.
   "s->" #'dired-omit-mode
   "C-/" #'dired-undo))

;;; Package

(add-to-list 'load-path "~/p/trivial-copy")
(load-package trivial-copy
  :commands
  trivial-copy-copy
  trivial-copy-move
  trivial-copy-paste)

;;; Config

(add-hook 'dired-mode-hook #'auto-revert-mode)
(with-eval-after-load 'dired
  (require 'dired-x))
(setq dired-dwim-target t)
(advice-add #'dired-maybe-insert-subdir :after
            (lambda (&rest _) (recenter-top-bottom)))

;;; Function

(defun luna-dired-open-file-at-point ()
  (interactive)
  (if-let ((file (dired-file-name-at-point)))
      (shell-command (format "open %s" file))
    (message "Not file found at point")))


;; -*- lexical-binding: t -*-

;;; Key

(luna-def-key
 :keymaps 'dired-mode-map
 "b" #'dired-up-directory
 "q" #'luna-quit-window
 "<s-mouse-1>" #'dired-toggle-mark-click
 "C-c C-o" #'luna-dired-open-file-at-point
 
 "s-c" #'dired-copy
 "s-v" #'dired-paste
 ;; option+command+v
 "s-M-v" #'dired-move

 ;; "s-v" #'trivial-copy-paste
 ;; "s-c" #'trivial-copy-copy
 ;; "s-M-v" #'trivial-copy-move
 
 ;; command+shift+.
 "s->" #'dired-omit-mode
 "C-/" #'dired-undo

 "C-c C-r" #'dired-rsync)

;;; Package

(luna-on "Brown"
  (load-package trivial-copy
    :load-path "~/p/trivial-copy"
    :commands
    trivial-copy-copy
    trivial-copy-move
    trivial-copy-paste))

(load-package dired-rsync
  :commands dired-rsync)

;;; Config

(load-package dired
  :defer
  :hook (dired-mode-hook
         . ((lambda () (require 'dired+))
            auto-revert-mode
            toggle-truncate-lines
            ;; dired-omit-mode
            dired-hide-details-mode))
  :config
  ;; On Linux this sorts numbers in natural order.
  (setq dired-listing-switches "-lah1v"
        dired-dwim-target t)
  (luna-on "Brown"
    (setq dired-listing-switches "-lah")))

;;; Function

(defun luna-dired-open-file-at-point ()
  (interactive)
  (if-let ((file (dired-file-name-at-point)))
      (shell-command (format "open %s" file))
    (user-error "File not found at point")))


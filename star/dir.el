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
 "C-/" #'dired-undo)

;;; Package

(luna-on "Brown"
  (load-package trivial-copy
    :load-path "~/p/trivial-copy"
    :commands
    trivial-copy-copy
    trivial-copy-move
    trivial-copy-paste))

;;; Config

(load-package dired
  :defer
  :hook (dired-mode-hook
         . (auto-revert-mode
            toggle-truncate-lines
            dired-omit-mode
            dired-hide-details-mode))
  :config
  (require 'autorevert)
  (require 'dired-x)
  (require 'dired+)
  ;; On Linux this sorts numbers in natural order.
  (setq dired-listing-switches "-lah1v"
        dired-dwim-target t)
  (luna-on "Brown"
    (setq dired-listing-switches "-lah"))
  
  ;; Focus on the new subdir when after inserting it.
  (advice-add #'dired-maybe-insert-subdir :after
              (lambda (&rest _)
                (when (called-interactively-p 'interactive)
                  (recenter-top-bottom)))))

;;; Function

(defun luna-dired-open-file-at-point ()
  (interactive)
  (if-let ((file (dired-file-name-at-point)))
      (shell-command (format "open %s" file))
    (user-error "File not found at point")))


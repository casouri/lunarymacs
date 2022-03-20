;;; after-save.el --- Run commands after save  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'project)

(defvar after-save-exclude-list ()
  "A list of file patterns excluded from running aftersave.")

(defun after-save-run ()
  "Run the after-save file"
  (let* ((root (project-root (project-current)))
         (after-save-file (expand-file-name ".after-save" root))
         (output-buffer (get-buffer-create
                         (format "*after save output for %s*"
                                 root))))
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string) "\n\n")))
    (when (file-exists-p after-save-file)
      (call-process "bash" after-save-file output-buffer))))

(define-minor-mode after-save-mode
  "Run script after saving a buffer."
  :lighter ""
  (if after-save-mode
      (add-hook 'after-save-hook #'after-save-run 0 t)
    (remove-hook 'after-save-hook #'after-save-run t)))

(provide 'after-save)

;;; aftersave.el ends here

;;; -*- lexical-binding: t -*-

;;; Variables

(defvar luna-autoload-file (expand-file-name "autoload.el" user-emacs-directory)
  "The path of autoload file which has all the autoload functions.")

(defvar luna-star-enable-alist nil
  "((star1 . t) (star2 . nil)), then start1 is enabled and star2 is not.")

(defvar luna-cache-dir (expand-file-name "cache" user-emacs-directory)
  "The dir for cache files.")

(defvar luna-package-list nil
  "List of package symbols. Added by ‘load-package’.")

(defvar luna--feature-hook-alist nil
  "Stores hooks for each feature declared by ‘luna-provide’.
Each element is like (feature . (hooks ...)).")

(defvar luna--feature-enable-alist nil
  "Stores whether a particular feature is provided already.")

;;; Functions

(defun luna-provide (feature)
  "Provide FEATURE."
  (let ((hook-list (alist-get feature luna--feature-hook-alist)))
    (when hook-list
      ;; something is already appended to hook
      (mapc #'funcall hook-list))
    (setf (alist-get feature luna--feature-enable-alist)
          t)))

(defun luna-eval-after-load (feature hook)
  "Ensure FEATURE (a symbol) is provided before running HOOK.
HOOK is a function."
  (if (alist-get feature luna--feature-enable-alist)
      ;; feature already provided
      (funcall hook)
    (setf (alist-get feature luna--feature-hook-alist)
          (append (alist-get feature luna--feature-hook-alist)
                  (list hook)))))

(defmacro luna-with-eval-after-load (feature &rest body)
  "Ensure FEATURE (a symbol) is provided before evaluating BODY.
HOOK is a function."
  (declare (indent 1))
  `(luna-eval-after-load ,feature (lambda () ,@body)))

(defmacro luna-message-error (&rest body)
  "Eval BODY and print error message if any."
  `(condition-case err
       (progn ,@body)
     (error (message (format "Error occured:\n%s\n" (error-message-string err))))))

(defun luna-safe-load (file &rest args)
  "Load FILE and don’t error out.
ARGS is as same as in `load'."
  (luna-message-error
   (apply #'load file args)))

(defun luna-load-or-create (file &rest args)
  "Load FILE if file exists, otherwise create it.
ARGS is as same as in `load'."
  (if (file-exists-p file)
      (apply #'luna-safe-load file args)
    (save-excursion
      (find-file file)
      (save-buffer)
      (kill-buffer))))

(defun luna-load-relative (file &rest args)
  "Load FILE relative to user-emacs-directory. ARGS are applied to ‘load'."
  (apply #'luna-load-or-create (expand-file-name file user-emacs-directory) args))

(defmacro load-package (package &rest body)
  "Thin wrapper around ‘use-package’."
  (declare (indent 1))
  `(luna-message-error (add-to-list 'luna-package-list ',package t)
                       (use-package ,package
                         ,@body)))

(defvar luna-prepared-p nil
  "T if ‘luna-before-install-package’ has ran.")

(defun luna-before-install-package ()
  "Setup for installing packages."
  (interactive)
  (require 'cowboy)
  (require 'package)
  (package-initialize t)
  (package-refresh-contents)
  (setq luna-prepared-p t))

(defun luna-install-all-package ()
  "Install  packages."
  (interactive)
  (display-buffer "*Messages*")
  (unless luna-prepared-p
    (luna-before-install-package))
  (dolist (package luna-package-list)
    (cowboy-install package)))

(defmacro luna-lsp/eglot (lsp eglot)
  "Run LSP or EGLOT based on `luna-lsp'."
  `(pcase luna-lsp
     ('lsp ,lsp)
     ('eglot ,eglot)))

;;; Convienient

(defun luna-jump-to-package (package)
  "Jump to the configuration of package string."
  (interactive (list (completing-read "Package: " (mapcar #'symbol-name luna-package-list))))
  (find-file user-init-file)
  (goto-char (point-min))
  (unless (re-search-forward (format "(load-package %s" package) nil t)
    (message "Not found")))

;;;; ENV

(defun luna-load-env ()
  "Load PATH and CPATH from a file."
  (interactive)
  (condition-case err
      (progn (load "~/.emacsenv")
             (setq exec-path (split-string (getenv "PATH") ":")))
    (error (message (error-message-string err)))))

(provide 'lunary)


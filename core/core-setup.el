;;; core-setup.el --- Install packages      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;; 

;;; Code:
;;

(eval-when-compile (load (concat (expand-file-name user-emacs-directory) "core/core-general.el")))

(moon-set-load-path)

(require 'f)

(defvar moon-ignore-package-list '(system-packages)
  "Ignore system packages.")

(defmacro moon-message&result (message &rest body)
  "Pring message and eval BODY, then show result."
  `(progn
     (princ ,message)
     (princ (if (progn ,@body)
                green-OK
              red-ERROR))
     (princ "\n")))

(defun moon-ing-msg (ing symbol)
  "\(moon-ing-msg \"Installing\" 'package\) -> \"Installing package\"."
  (format "%s %s %s " ing (symbol-name symbol)
          (make-string (abs (- 30 (length (symbol-name symbol))))
                       ?\s)))

(defun moon/install-package (&optional package)
  "Install packages specified in `package.el' files in each star.

If PACKAGE non-nill, install only that package."
  ;; TODO concurrency when possible
  ;; make-thread is not concurrent
  ;; TODO extract cowboy-install out from if statement
  (interactive)
  (let ((all-file-in-load-path
         (mapcan (lambda (dir) (append (mapcar #'file-name-base (directory-files-recursively dir "\\.el$"))
                                       (mapcar #'file-name-base (f-directories moon-package-dir))))
                 (list moon-package-dir moon-site-lisp-dir))))
    (dolist (package (if package (list package) moon-package-list))
      (let ((package-symbol (if (symbolp package)
                                package
                              (car package)))
            (system-package-p (when (listp package) (plist-get (cdr package) :system))))
        (unless (or (member (symbol-name package-symbol)
                            all-file-in-load-path)
                    (member package-symbol moon-ignore-package-list)
                    system-package-p)
          (moon-message&result (moon-ing-msg "Installing" package-symbol)
                               (cowboy-install package)))))

    (moon-message&result (moon-ing-msg "Compiling" 'packages) (silent| (cowboy-compile) t))))

(defun moon/update-package (&optional package)
  "Update packages to the latest version.

If PACKAGE non-nil, install only that package."
  ;; TODO concurrency when possible
  ;; make-thread is not concurrent
  ;; TODO extract cowboy-update from if statement
  (interactive)
  (if package
      (cowboy-update package)
    (dolist (package-dir (f-directories cowboy-package-dir))
      (moon-message&result (moon-ing-msg "Updating" (cowboy--package-symbol package-dir))
                           (cowboy-update package-dir))))
  (moon-message&result (moon-ing-msg "Compiling" 'packages) (silent| (cowboy-compile) t)))


(defun moon/generate-autoload-file ()
  "Extract autload file from each star to `moon-autoload-file'."
  (interactive)
  (let ((autoload-file-list
         (file-expand-wildcards
	  ;; core autoload
          (expand-file-name "autoload/*.el" moon-core-dir)))
        package-autoload-file-list)
    ;; package autoload
    (dolist (file (directory-files-recursively
		   moon-package-dir
		   "\\.el$"))
      (push file package-autoload-file-list))
    ;; star autoload
    (dolist (star-path moon-star-path-list)
      (let ((auto-dir (expand-file-name "autoload" star-path))
            (auto-file (expand-file-name "autoload.el" star-path)))
        (when (file-exists-p auto-file)
          (push auto-file autoload-file-list))
        (when (file-directory-p auto-dir)
          (dolist (file (directory-files-recursively auto-dir "\\.el$"))
            (push file autoload-file-list))))
      (when (file-exists-p moon-autoload-file)
        (delete-file moon-autoload-file)
        (message "Delete old autoload file")))
    ;; autoload file in stars
    (dolist (file (reverse autoload-file-list))
      (message
       (condition-case err
           (cond ((update-file-autoloads file t moon-autoload-file)
                  "Nothing in %s")
                 (t "Scanned %s"))
         (error (format "%s: %s %s" red-error err red-cross)))
       (file-relative-name file moon-emacs-d-dir)))
    ;; autoload files in packages
    (princ "Loading autoload file from packages ")
    (let ((count 0))
      (dolist (file (reverse package-autoload-file-list))
        (when (eq (% count 250) 0)
          (princ ". "))
        (setq count (1+ count))
        (ignore-errors
          (update-file-autoloads file t moon-autoload-file))))
    (princ green-OK)
    (princ "\n")))


(defun moon/make ()
  "Install package and autoload file."
  (moon/install-package)
  (moon/generate-autoload-file))

;;; Run code

(bootstrap)
(load| core-ui)
(load| core-edit)

(provide 'core-setup)

;;; core-setup.el ends here

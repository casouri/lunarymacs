;;; core-setup.el --- Install packages      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;; 

;;; Code:
;;

(eval-when-compile (load (concat (expand-file-name user-emacs-directory) "core/core-general.el")))

(defun moon-setup-setup ()
  "Setup for setup. Run after init.el so Emacs knows all the stars."
  (load| core-ui)
  (load| core-edit)

  (moon-load-config moon-star-path-list)
  (moon-set-load-path)

  (require 'cowboy)
  (setq cowboy-package-dir moon-package-dir)

  (require 'package)
  (package-initialize t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents))

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
  "Install PACKAGE or all packages in `moon-package-list'."
  (let ((package-list (if package (list package) moon-package-list)))
    (dolist (package package-list)
      (unless (cowboy-installedp package)
        (cowboy-install package)))))

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
        (cl-incf count)
        (let ((auto-save-default nil))
          (silent| (ignore-errors
                     (update-file-autoloads file t moon-autoload-file))))))
    (princ green-OK)
    (princ "\n")))


(defun moon/make ()
  "Install package and autoload file."
  (moon/install-package)
  (moon/generate-autoload-file))

(provide 'core-setup)

;;; core-setup.el ends here

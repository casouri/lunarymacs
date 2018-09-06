;;; core-setup.el --- Install packages      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;; 

;;; Code:
;;

(eval-when-compile (load (concat (expand-file-name user-emacs-directory) "core/core-general.el")))

(defun moon/install-package ()
  "Install packages specified in `package.el' files in each star.

It will not print messages printed by `package-install'
because it's too verbose."
  (interactive)
  (dolist (package moon-quelpa-package-list)
    (unless (or (package-installed-p (car package))
                (require (car package) nil t))
      (princ (format "Installing %s %s " (symbol-name (car package))
                     (make-string (abs (- 30 (length (symbol-name (car package)))))
                                  ?\s)))
      (princ (or
              (ignore-errors
                (silent| (quelpa package))
                green-OK)
              red-ERROR))
      (princ "\n")))
  (quelpa-save-cache)
  (dolist (package moon-package-list)
    (unless (or (package-installed-p package)
                (require package nil t))
      (princ (format "Installing %s %s " (symbol-name package)
                     (make-string (abs (- 30 (length (symbol-name package))))
                                  ?\s)))
      ;; installing packages prints too many messages
      (princ (or
              (ignore-errors
                (silent| (package-install package))
                green-OK)
              red-ERROR))
      (princ "\n"))))

(defun moon/update-package ()
  "Update packages to the latest version.

It will not print messages printed by updating packages
because it's too verbose."
  (interactive)
  (dolist (package moon-quelpa-package-list)
    (silent| (quelpa (append package '(:upgrade t)))))
  ;; https://oremacs.com/2015/03/20/managing-emacs-packages/
  
  ;; If there is no package to update,
  ;; package.el will throw "No operation specified"
  ;; but I didn't find any code throwing error
  ;; in package.el...
  ;; TODO find out a better implementation
  (silent| ; don't print message
   (condition-case err
       (save-window-excursion
         (package-list-packages t)
         (package-menu-mark-upgrades)
         (package-menu-execute t))
     ;; if there is no package to upgrade,
     ;; this errr will emit
     (user-error nil))))

(defun moon/remove-unused-package ()
  "Remove packages that are not declared in any star with `package|' macro."
  (interactive)
  (dolist (package package-alist)
    (let ((package-name (car package))
          (package-description (car (cdr package)))
          (non-dependency-list (package--find-non-dependencies)))
      (when (and (not (member package-name moon-package-list))
                 (package-built-in-p package)
                 (member package-name non-dependency-list))
        (package-delete package-description))
      )))

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
;; (moon-set-load-path)

(provide 'core-setup)

;;; core-setup.el ends here

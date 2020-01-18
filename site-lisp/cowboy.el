;;; cowboy.el --- Package manager      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'package)
(require 'luna-f)
(require 'cl-lib)

;;; Variable

(defvar cowboy-package-dir (luna-f-join user-emacs-directory "ranch")
  "The directory where cowboy downloads packages to.")

(defvar cowboy--backend-alist '((github . (cowboy--github-install cowboy--github-update))
                                (url . (cowboy--url-install cowboy--url-update)))
  "Alist of (key . (install-fn update-fn)).

Each function takes the same arguments: package (symbol),
recipe-list and optionally option-plist.")

(defvar cowboy-recipe-alist ()
  "Contains the recopies for each package.
This is an alist of form: ((package . properties)).

package is a symbol, properties is a plist.
Avaliable keywords: :fetcher, :repo, :dependency, :pseudo.

:fetcher is a symbol representing the source, available options are 'github, 'url.
If none specified, default to 'github.

:repo is a string representing a repository from github, it should be like \"user/repo\".

TODO :branch fetch a particular branch of repo.

:dependency is a list of symbols of packages thar this package depends on.

:pseudo is for pseudo packages. for example, ivy, counsel & swiper are in one repo,
then you only need one recipe. The other two can be configured as pseudo packages.

TODO :load-path is for additional load-path entries. By default cowboy adds package dir
and subdir under that into load-path, if the package needs to add subdirs that are deeper
to load-path, use this key to specify a relative path to package-dir. No preceeding slash or dot.")

;;;; Backstage

(defun cowboy--install-fn (backend)
  "Return the install function of BACKEND (symbol) or nil."
  (nth 0 (alist-get backend cowboy--backend-alist)))

(defun cowboy--update-fn (backend)
  "Return the update function of BACKEND (symbol) or nil."
  (nth 1 (alist-get backend cowboy--backend-alist)))

(defmacro cowboy--message-error (package &rest body)
  "Run BODY and print error message mentioning PACKAGE."
  (declare (indent 1))
  (let ((err-sym (gensym)))
    `(condition-case ,err-sym
         (progn ,@body)
       ((debug error)
        (message "Error installing %s: %s"
                 ,(symbol-name package)
                 (error-message-string ,err-sym))))))

(defun cowboy-installedp (package)
  "Return t if PACKAGE (symbol) is installed, nil if not."
  (or (package-installed-p package)
      (member (symbol-name package)
              (directory-files cowboy-package-dir))))

(defun cowboy--command (command dir &rest args)
  "Call process with COMMAND and ARGS in DIR."
  (let ((default-directory dir))
    (with-temp-buffer
      (when (not (eq 0 (apply #'call-process command nil t nil
                              args)))
        (error (buffer-string))))))

;;;; Userland

(defun cowboy-ensure-refresh-content (&optional force)
  "Make sure package list is refreshed.
If FORCE non-nil, always refresh."
  (package-initialize)
  (when (or force
            (not (get 'package-refresh-contents 'cowboy-did)))
    (package-refresh-contents)
    (put 'package-refresh-contents 'cowboy-did t)))

(defun cowboy-install (package &optional option-plist)
  "Install PACKAGE (symbol).

OPTION-PLIST contains user options that each backend may use."
  (interactive "SPackage: ")
  (cowboy--message-error package
    (message "Installing %s" package)
    (let ((recipe (alist-get package cowboy-recipe-alist)))
      (if recipe
          (progn
            (message "Found recipe")
            (if (cowboy-installedp package)
                (message "%s is already installed" package)
              (when-let ((dependency-list (plist-get recipe :dependency)))
                (message "Found dependencies: %s" dependency-list)
                (dolist (dep dependency-list)
                  (cowboy-install dep option-plist)))
              (let ((fetcher (or (plist-get recipe :fetcher) 'github)))
                (message "Installing package with %s backend." fetcher)
                (funcall (cowboy--install-fn fetcher)
                         package recipe option-plist)))
            (add-to-list 'load-path (luna-f-join cowboy-package-dir
                                                 (symbol-name package)))
            (require (intern-soft package) nil t))
        (message "Recipe not found, installing with package.el")
        (cowboy-ensure-refresh-content)
        (package-install package))
      (message "Package %s installed" package))))

(defun cowboy-update (package)
  "Update PACKAGE."
  (interactive "SPackage: ")
  (cowboy--message-error package
    (message "Updating %s" package)
    (let ((recipe (alist-get package cowboy-recipe-alist)))
      (if recipe
          (progn (message "Found recipe")
                 ;; handle dependency
                 (when-let ((dependency-list (plist-get recipe :dependency)))
                   (message "Found dependencies: %s" dependency-list)
                   (mapc #'cowboy-update dependency-list))
                 ;; update this package
                 (let ((fetcher (or (plist-get recipe :fetcher)
                                    'github)))
                   (message "Updating package with %s backend" fetcher)
                   (funcall (cowboy--update-fn fetcher)
                            package recipe)))
        ;; no recipe
        (message "Recipe not found, updating with package.el")
        (cowboy-ensure-refresh-content)
        (package-delete (car (alist-get package package-alist)) t)
        (package-install package))
      (message "Package %s updated" package))))

(defun cowboy-delete (package)
  "Delete PACKAGE."
  (interactive "SPackage: ")
  (cowboy--message-error package
    (message "Deleting %s" package)
    (let ((recipe (alist-get package cowboy-recipe-alist)))
      (if recipe
          (progn (message "Found recipe")
                 (delete-directory
                  (luna-f-join cowboy-package-dir (symbol-name package))
                  t t))
        ;; try to use package.el to delete
        (message "Recipe not found, deleting with package.el")
        (cowboy-ensure-refresh-content)
        (package-delete (alist-get package package-alist)))
      (message "Package %s deleted" package))))

(defun cowboy-reinstall (package)
  "Reinstall PACKAGE."
  (cowboy-delete package)
  (cowboy-install package))

(defun cowboy-add-load-path ()
  "Add packages to `load-path'."
  ;; add first and second level directories to load-path
  ;; this is usually enough
  (dolist (package-dir-path (luna-f-list-directory cowboy-package-dir t))
    (add-to-list 'load-path package-dir-path)
    (dolist (package-subdir-path (luna-f-list-directory package-dir-path t))
      (add-to-list 'load-path package-subdir-path))))

;;; Fetchers

;;;; Git

(defun cowboy--github-install (package recipe &optional option-plist)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol).

OPTION-PLIST contains installation options.
In OPTION-PLIST, if :full-clone is t, full clone.

In RECIPE, :repo is of form \"user/repo\"."
  (let ((full-clone (plist-get option-plist :full-clone)))
    (cowboy--command "git" cowboy-package-dir "clone"
                     (unless full-clone "--depth=1")
                     (if (plist-get recipe :repo)
                         (format "https://github.com/%s.git" (plist-get recipe :repo))
                       (if (plist-get recipe :http)
                           (error "No :repo nor :http in recipe: %s" recipe)))
                     (symbol-name package))))

(defun cowboy--github-shallowp (package)
  "Return t if PACKAGE (a symbol) is shallow cloned, nil if not."
  (let ((default-directory (luna-f-join cowboy-package-dir (symbol-name package))))
    (with-temp-buffer
      (and (eq 0 (funcall #'call-process "git" nil t nil
                          "rev-parse" "--is-shallow-repository"))
           ;; return non-nil if true (shallow), nil if false (not shallow)
           (search-backward "true" nil t)))))

(defun cowboy--github-update (package _)
  "Pull PACKAGE (a symbol) with RECIPE from upstream. Return t if success, nil if fail."
  (cowboy--command "git" (luna-f-join cowboy-package-dir (symbol-name package))
                   "fetch"
                   (if (cowboy--github-shallowp package)
                       "--depth=1")))



;;;; URL

(defun cowboy--url-install (package recipe &optional _)
  "Download the PACKAGE with RECIPE directly from URL."
  (let ((resp-buf (url-retrieve-synchronously
                   (plist-get recipe :url) t nil 10)))
    (unwind-protect
        (with-current-buffer resp-buf
          (goto-char (point-min))
          (re-search-forward "\n\n")
          (delete-region (point-min) (match-end 0))
          (let ((dir (luna-f-join cowboy-package-dir package))
                (coding-system-for-write 'utf-8))
            (unless (file-exists-p dir) (mkdir dir))
            (write-region nil nil (luna-f-join dir (format "%s.el" package)))))
      (kill-buffer resp-buf))))

(defun cowboy--url-update (package recipe)
  "Update PACKAGE with RECIPE."
  ;; TODO
  (cowboy-delete package)
  (cowboy--url-install package recipe))


(provide 'cowboy)

;;; cowboy.el ends here

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

package is a symbol, properties is a plist. Avaliable keywords:
:fetcher, :repo, :dependency, :pseudo.

:fetcher is a symbol representing the source, available options
are 'github, 'url. If none specified, default to 'github.

:repo is a string representing a repository from github, it
should be like \"user/repo\".

TODO :branch fetch a particular branch of repo.

:dependency is a list of symbols of packages thar this package
depends on.

:pseudo is for pseudo packages. for example, ivy, counsel &
swiper are in one repo, then you only need one recipe. The other
two can be configured as pseudo packages.

:subdir is for additional load-path entries. By default cowboy
adds package dir into load-path, if the package needs to add
subdirs to load-path, use this key to specify a
relative path to package-dir. No preceding slash or dot.")

;;; Backstage

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

(defun cowboy--avaliable-package-list ()
  "Return a list of available packages as string."
  (cowboy-ensure-refresh-content)
  (append (mapcar (lambda (pkg) (symbol-name (car pkg)))
                  package-archive-contents)
          (mapcar (lambda (recipe) (symbol-name (car recipe)))
                  cowboy-recipe-alist)))

(defun cowboy--installed-package-list ()
  "Return a list of installed packages as string."
  (append (mapcar (lambda (pkg) (symbol-name pkg))
                  package-activated-list)
          (luna-f-list-directory cowboy-package-dir)))

(defun cowboy--add-package-load-path (package)
  "Add PACKAGE (symbol) to `load-path'."
  (let* ((package-dir-path (luna-f-join cowboy-package-dir (symbol-name package)))
         (recipe (alist-get package cowboy-recipe-alist))
         (subdir-list (when recipe
                        (plist-get recipe :subdir))))
    ;; add main dir
    (add-to-list 'load-path package-dir-path)
    ;; add sub dir
    (when subdir-list
      (dolist (subdir subdir-list)
        (add-to-list 'load-path (luna-f-join package-dir-path subdir))))))

;;; Userland

(defun cowboy-ensure-refresh-content (&optional force)
  "Make sure package list is refreshed.
If FORCE non-nil, always refresh."
  (package-initialize)
  (let ((last-time (get 'package-refresh-contents 'cowboy-last-refresh-time)))
    (when (or force
              (not last-time)
              ;; haven’t update for more than 1 week
              (> (time-to-number-of-days
                  (time-since last-time))
                 7))
      (package-refresh-contents)
      (put 'package-refresh-contents 'cowboy-last-refresh-time
           (current-time)))))

(defun cowboy-install (package &optional option-plist)
  "Install PACKAGE (symbol).

OPTION-PLIST contains user options that each backend may use."
  (interactive (list (intern
                      (completing-read "Package: "
                                       (cowboy--avaliable-package-list)))))
  (cowboy--message-error package
    (let ((recipe (alist-get package cowboy-recipe-alist)))
      (if (cowboy-installedp package)
          (message "%s is already installed" package)
        (message "Installing %s" package)
        (if recipe
            (progn (message "Found recipe")
                   (when-let ((dependency-list (plist-get recipe :dependency)))
                     (message "Found dependencies: %s" dependency-list)
                     (dolist (dep dependency-list)
                       (cowboy-install dep option-plist)))
                   (let ((fetcher (or (plist-get recipe :fetcher) 'github)))
                     (message "Installing package with %s backend." fetcher)
                     (funcall (cowboy--install-fn fetcher)
                              package recipe option-plist))
                   (cowboy--add-package-load-path package)
                   (require package nil t))
          (message "Recipe not found, installing with package.el")
          (cowboy-ensure-refresh-content)
          (package-install package))
        (message "Package %s installed" package)))))

(defun cowboy-update (package)
  "Update PACKAGE."
  (interactive (list (intern
                      (completing-read "Package: "
                                       (cowboy--installed-package-list)))))
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
  (interactive (list (intern
                      (completing-read "Package: "
                                       (cowboy--installed-package-list)))))
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
  (dolist (package (luna-f-list-directory cowboy-package-dir))
    (cowboy--add-package-load-path (intern package))))

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

(defun cowboy--github-update (package recipe)
  "Pull PACKAGE (a symbol) with RECIPE from upstream."
  (if (cowboy--github-shallowp package)
      (progn
        (cowboy-delete package)
        (cowboy--github-install package recipe))
    (cowboy--command "git" (luna-f-join cowboy-package-dir (symbol-name package))
                     "pull" "--rebase")))



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

;;; cowboy.el --- Package manager      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>
;; With code form auto-package-update.el

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

(defun cowgirl-installed-p (package)
  "Return t if PACKAGE (symbol) is installed, nil if not."
  (if (cowgirl-use-cowboy package)
      (cowboy-installed-p package)
    (package-installed-p package)))

(defun cowboy-installed-p (package)
  "Return t if PACKAGE (symbol) is installed, nil if not."
  (member (symbol-name package)
          (directory-files cowboy-package-dir)))

(defun cowboy--command (command dir &rest args)
  "Call process with COMMAND and ARGS in DIR."
  (let ((default-directory dir))
    (with-temp-buffer
      (when (not (eq 0 (apply #'call-process command nil t nil
                              args)))
        (error (buffer-string))))))

(defun cowgirl--avaliable-package-list ()
  "Return a list of available packages as string."
  (cowgirl-ensure-refresh-content)
  (append (mapcar (lambda (pkg) (symbol-name (car pkg)))
                  package-archive-contents)
          (cowboy--avaliable-package-list)))

;; From auto-package-update.el
(defun cowgirl--up-to-date-p (package)
  "Return t if PACKAGE (symbol) is up-to-date."
  (when (and (package-installed-p package)
             (cadr (assq package package-archive-contents)))
    (let* ((newest-desc (cadr (assq package package-archive-contents)))
           (installed-desc (cadr (or (assq package package-alist)
                                     (assq package package--builtins))))
           (newest-version  (package-desc-version newest-desc))
           (installed-version (package-desc-version installed-desc)))
      (version-list-<= newest-version installed-version))))

(defun cowboy--avaliable-package-list ()
  "Return a list of available packages as string."
  
  (mapcar (lambda (recipe) (symbol-name (car recipe)))
          cowboy-recipe-alist))

(defun cowgirl--installed-package-list ()
  "Return a list of installed packages as string."
  (append (mapcar (lambda (pkg) (symbol-name pkg))
                  package-activated-list)
          (cowboy--installed-package-list)))

(defun cowboy-delete-1 (package)
  "Delete PACKAGE (symbol)."
  (delete-directory
   (luna-f-join cowboy-package-dir (symbol-name package)) t t))

(defun cowboy--installed-package-list ()
  "Return a list of installed packages as string."
  (luna-f-list-directory cowboy-package-dir))

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

;;;; Cowgirl

(defun cowgirl-use-cowboy (package)
  "Return t if this PACKAGE (string) is handled by cowboy."
  (and (member package (mapcar (lambda (e) (symbol-name (car e)))
                               cowboy-recipe-alist))
       ;; ‘member’ doesn’t return t.
       t))

(defun cowgirl-ensure-refresh-content (&optional force)
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

(defun cowgirl-install (package &optional option-plist)
  "Install PACKAGE (symbol).

OPTION-PLIST contains user options that each backend may use."
  (interactive (list (intern
                      (completing-read "Package: "
                                       (cowgirl--avaliable-package-list)))))
  (if (cowgirl-use-cowboy package)
      (cowboy-install package option-plist)
    (package-install-from-archive
     (cadr (assoc package package-archive-contents)))))

(defun cowgirl-delete (package)
  "Delete PACKAGE."
  (interactive (list (intern
                      (completing-read "Package: "
                                       (cowgirl--installed-package-list)))))
  (if (cowgirl-use-cowboy package)
      (cowboy-delete package)
    (package-delete package)))

;;;; Cowboy

(defun cowboy-install (package &optional option-plist)
  "Install PACKAGE (symbol).

OPTION-PLIST contains user options that each backend may use."
  (interactive (list
                (intern
                 (completing-read "Package: "
                                  (cowboy--avaliable-package-list)))))
  (cowboy--message-error package
    (let ((recipe (alist-get package cowboy-recipe-alist)))
      (if (cowboy-installed-p package)
          (message "%s is already installed" package)
        (if (not recipe)
            (message "No recipe for %s" package)
          (when-let ((dependency-list (plist-get recipe :dependency)))
            (message "Found dependencies: %s" dependency-list)
            (dolist (dep dependency-list)
              (cowgirl-install dep option-plist)))
          (let ((fetcher (or (plist-get recipe :fetcher) 'github)))
            (funcall (cowboy--install-fn fetcher)
                     package recipe option-plist)
            (cowboy--add-package-load-path package)
            (require package nil t)
            (message "%s installed with %s backend" package fetcher)))))))

(defun cowboy-update (package)
  "Update PACKAGE (symbol)."
  (interactive (list
                (intern
                 (completing-read "Package: "
                                  (cowboy--installed-package-list)))))
  (cowboy--message-error package
    (let ((recipe (alist-get package cowboy-recipe-alist)))
      (if (not recipe)
          (message "No recipe for %s" package)
        ;; handle dependency
        (when-let ((dependency-list (plist-get recipe :dependency)))
          (message "Found dependencies: %s" dependency-list)
          (mapc #'cowgirl-update dependency-list))
        ;; update this package
        (let ((fetcher (or (plist-get recipe :fetcher)
                           'github)))
          (funcall (cowboy--update-fn fetcher)
                   package recipe)
          (message "Package %s updated with %s backend" package fetcher))))))

(defun cowboy-delete (package)
  "Delete PACKAGE (symbol)."
  (interactive (list
                (intern
                 (completing-read "Package: "
                                  (cowboy--installed-package-list)))))
  (cowboy--message-error package
    (if (not (member (symbol-name package) (cowboy--installed-package-list)))
        (message "Package %s is not installed" package)
      (cowboy-delete-1 package)
      (message "Package %s deleted" package))))

(defun cowboy-update-all ()
  "Update all packages."
  (interactive)
  (dolist (package (luna-f-list-directory cowboy-package-dir))
    (cowboy-update (intern package))))

(defun cowboy-reinstall (package)
  "Reinstall PACKAGE."
  (cowboy-delete package)
  (cowboy-install package))

(defun cowboy-add-load-path ()
  "Add packages to `load-path'."
  ;; add first and second level directories to load-path
  ;; this is usually enough
  (dolist (package-name (luna-f-list-directory cowboy-package-dir))
    (cowboy--add-package-load-path (intern package-name))))

(defun cowboy-prune ()
  "Delete installed packages that don’t have recipe."
  (interactive)
  (let ((package-list
         (cl-loop for package-name in (cowboy--installed-package-list)
                  if (not (alist-get
                           (intern package-name) cowboy-recipe-alist))
                  collect package-name)))
    (if package-list
        (when (yes-or-no-p
               (format "Deleting these packages: %sproceed?"
                       (cl-reduce
                        (lambda (str name) (concat str name ", "))
                        package-list :initial-value "")))
          (dolist (package-name package-list)
            (cowboy-delete (intern package-name))))
      (message "No packages to prune"))))

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
        (cowboy-delete-1 package)
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

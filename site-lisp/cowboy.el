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

;;; Function

;;;; Userland

(defun cowboy-install (package &optional option-plist)
  "Install PACKAGE (a symbol, a recipe or a directory) by cloning it down.
Do nothing else (no autoload, no byte compile). Return t if success, nil if fail.

OPTION-PLIST contains user options that each backend may use.

If package is a directory string,
the directory file name will be used as package name."
  (cowboy--handle-error
   (cowboy--with-recipe (package recipe package-symbol)
     (if recipe
         (if (cowboy-installedp package)
             (message "%s is already installed" package)
           (when-let ((dependency-list (plist-get recipe :dependency)))
             (dolist (dep dependency-list)
               (cowboy-install dep option-plist)))
           (funcall (intern (format "cowboy--%s-install"
                                    (symbol-name (or (plist-get recipe :fetcher) 'github))))
                    package-symbol recipe option-plist))
       (package-install package-symbol)))))

(defun cowboy-update (package)
  "Update PACKAGE from upstream. Return t if success, nil if fail.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir."
  (cowboy--handle-error
   (cowboy--with-recipe (package recipe package-symbol)
     (if recipe
         (progn
           ;; handle dependency
           (when-let ((dependency-list (plist-get recipe :dependency)))
             (mapcar #'cowboy-update dependency-list))
           ;; update this package
           (funcall
            (intern (format "cowboy--%s-update"
                            (symbol-name
                             (or (plist-get recipe :fetcher) 'github))))
            package-symbol recipe))
       ;; no cowboy recipe found, try with package.el
       (package-delete (alist-get package package-alist))
       (package-install package)))))

(defun cowboy-delete (package)
  "Delete PACKAGE.  Return t if success, nil if fail.
If PACKAGE is a symbol, treat as a package, if a string, treat as a dir."
  (cowboy--handle-error
   (cowboy--with-recipe (package recipe package-symbol)
     (cond ((stringp package)
            ;; package is a path, delete that path
            (delete-directory package t t))
           ;; there exists a cowboy recipe, delete that cowboy package
           (recipe
            (luna-f-join cowboy-package-dir (symbol-name package-symbol)))
           ;; try to use package.el to delete
           (t (package-delete (alist-get package package-alist)))))))

(defun cowboy-reinstall (package)
  "Reinstall PACKAGE."
  (cowboy-delete package)
  (cowboy-install package))

(defun cowboy-compile ()
  "Compile all packages."
  ;; cpmpile all file but only when .elc file is older than .el file
  (let ((inhibit-message t))
    (byte-recompile-directory cowboy-package-dir 0)))

(defun cowboy-add-load-path ()
  "Add packages to `load-path'."
  ;; add first and second level directories to load-path
  (dolist (package-dir-path (luna-f-list-directory cowboy-package-dir t))
    (add-to-list 'load-path package-dir-path)
    (dolist (package-subdir-path (luna-f-list-directory package-dir-path t))
      (add-to-list 'load-path package-subdir-path)))
  (package-initialize))

;;;; Backstage

;;;;; Helpers

(defun cowboy-installedp (package)
  "Return t if PACKAGE (symbol, recipe, dir string) is installed, nil if not."
  (ignore package)
  (cowboy--with-recipe (package recipe package-symbol)
    (or (package-installed-p package-symbol)
        (member (symbol-name package-symbol) (directory-files cowboy-package-dir)))))

(defun cowboy--package-symbol (package)
  "PACKAGE can be a recipe, a symbol or a dir. Return package symbol."
  (pcase package
    ((pred symbolp) package)
    ((pred stringp) (intern (file-name-base (directory-file-name package))))
    ((pred listp) (car package))
    ;; TODO rephrase
    (_ (error "Cannot make into package symbol: %s" package))))

(defmacro cowboy--only-with-recipe (&rest body)
  "Process package.
With package recipe, eval BODY. Return nil if no recipe found.
If PACKAGE is a symbol or list, treat as package,
if it is a string, treate as dir.

Variable PACKAGE should be defined prior to this macro,
inside the macro you get variable PACKAGE-SYMBOL and RECIPE."
  `(cowboy--with-recipe (package recipe package-symbol)
     (if recipe
         ,@body
       (message "Cannot find recipe for %s" (symbol-name package-symbol))
       nil)))

(defmacro cowboy--with-recipe (symbols &rest body)
  "Process package and evaluate BODY.
If PACKAGE is a symbol or list, treat as package,
if it is a string, treate as dir.

RECIPE and PACKAGE-SYMBOL is the symbol represents
the recipe and package symbol.

\(fn (PACKAGE RECIPE PACKAGE-SYMBOL) BODY...)"
  (declare (indent 1))
  (let ((package-sym (nth 0 symbols))
        (recipe-sym (nth 1 symbols))
        (package-symbol-sym (nth 2 symbols)))
    `(let* ((,package-symbol-sym (cowboy--package-symbol ,package-sym))
            (,recipe-sym (if (listp ,package-sym) ; in-place recipe always override recipe in cowboy-recipe-alist
                             (cdr ,package-sym)
                           (alist-get ,package-symbol-sym cowboy-recipe-alist))))
       ,@body)))

(defvar cowboy-error-func (lambda (err) (message (error-message-string err)))
  "The default error handling function used by `cowboy--handle-error'.")

(defmacro cowboy--handle-error (&rest form)
  "Eval FORM. Handle error with `cowboy-error-func'.

Return t if success, nil if fail."
  `(condition-case err (progn ,@form t)
     ((error) (funcall cowboy-error-func err)
      nil)))

(defun cowboy--command (command dir &rest args)
  "Call process with COMMAND and ARGS in DIR."
  (let ((default-directory dir))
    (with-temp-buffer
      (if (eq 0 (apply #'call-process command nil t nil
                       args))
          nil
        (error (buffer-string))))))


;;;;; Fetchers

;;;;;; Git

(defun cowboy--github-install (package recipe &optional option-plist)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol).

OPTION-PLIST contains installation options.
In OPTION-PLIST, if :full-clone is nil, shallow clone.

In RECIPE, :repo is of form \"user/repo\"."
  (let ((full-clone (plist-get option-plist :full-clone)))
    (cowboy--command "git" cowboy-package-dir "clone" (unless full-clone "--depth")
                     (unless full-clone "1")
                     (if (plist-get recipe :repo)
                         (format "https://github.com/%s.git" (plist-get recipe :repo))
                       (plist-get recipe :http))
                     (symbol-name package))))

(defun cowboy--github-shallowp (package)
  "Return t if PACKAGE (a symbol) is shallow cloned, nil if not."
  (let ((default-directory (luna-f-join cowboy-package-dir (symbol-name package))))
    (with-temp-buffer
      (and (eq 0 (funcall #'call-process "git" nil t nil
                          "rev-parse" "--is-shallow-repository"))
           ;; return t if true (shallow), nil if false (not shallow)
           (search-backward "true" nil t)))))

(defun cowboy--github-update (package recipe)
  "Pull PACKAGE (a symbol) with RECIPE from upstream. Return t if success, nil if fail."
  (if (cowboy--github-shallowp package)
      ;; simply reinstall
      (progn (cowboy-delete package)
             (cowboy--github-install package recipe))
    (cowboy--command "git" (luna-f-join cowboy-package-dir (symbol-name package))
                     "fetch")))



;;;;;; URL

(defun cowboy--url-install (package recipe &optional option-plist)
  "Download the PACKAGE (file) directly from URL.

OPTION-PLIST contains installation options.

RECIPE is a plist."
  (with-current-buffer (url-retrieve-synchronously
                        (plist-get recipe :url) t nil 10)
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (delete-region (point-min) (match-end 0))
    (let ((file-content (buffer-substring (point-min) (point-max)))
          (dir (format "%s%s/" cowboy-package-dir package))
          (coding-system-for-write 'utf-8))
      (unless (file-exists-p dir) (mkdir dir))
      (find-file (format "%s%s/%s.el" cowboy-package-dir package package))
      (insert file-content)
      (save-buffer))
    ;; (let ((redirection (plist-get status :redirect)))
    ;;   (if redirection
    ;;       (cowboy--http-clone package (plist-put recipe 'url redirection))
    ;;     ;; current buffer is retrieved data
    ;;     ))
    ))

(defun cowboy--url-update (package recipe)
  "Download PACKAGE with RECIPE again.
Return t if success, nil if fail.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir."
  ;; TODO
  (cowboy-delete package)
  (cowboy--url-install package recipe))

(provide 'cowboy)

;;; cowboy.el ends here

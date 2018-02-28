;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar moon-base-load-path
  (append (list moon-core-dir moon-star-dir)
          load-path)
  "A backup of `load-path' before it was altered by `doom-initialize'.

Contains only core dir ,star dir and load path for built in libraries")


(defvar moon-package-dir (concat user-emacs-directory ".local/package/")
  "Where melpa and elpa packages are installed.")


(defvar moon-package-list '("use-package" "bind-key")
  "A list of packages to install. Packages are represented by strings not symbols.")

(defvar moon--refreshed-p nil
  "Have you refreshed contents?")

;; DEBUG
;; (setq moon-package-dir "/Users/yuan/.emacs.second/.local/package")

(defvar moon-package-load-path ()
  "The load path of package libraries installed via ELPA and QUELPA.")

(defvar moon-star-path-list ()
  "The path to each stars.")

(defvar moon-autoload-file (concat moon-local-dir "autoloads.el")
  "The path of autoload file which has all the autoload functions.")


(fset 'moon-grand-use-package-call
      '(lambda ()
         "A bunch of (use-package blah blah blah) collected by use-package| macro from each config file of stars."))


;;
;; Config
;;

(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-user-dir (expand-file-name "elpa" moon-package-dir)
      package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))


;;
;; Func
;;

;; TODO change (package-initialize) to (package-initialize t)
(defun moon-initialize ()
  "Initialize installed packages (using package.el)."
  (package-initialize t)
  )

(defun moon-initialize-load-path ()
  "Add each package to load path."
    (setq moon-package-load-path (directory-files package-user-dir t nil t) ;; get all sub-dir
          load-path (append moon-base-load-path moon-package-load-path))
    (add-to-list 'load-path moon-local-dir)
    ;; make sure use-package and bind-key are in load path on the very first install
    (add-to-list 'load-path (car (directory-files (concat moon-package-dir "elpa/") t "use-package.+")) t)
    (add-to-list 'load-path (car (directory-files (concat moon-package-dir "elpa/") t "bind-key.+")) t)
    )

(defun moon-initialize-star ()
  "Load each star in `moon-star-list'."
  (unless noninteractive
    (moon-load-autoload))
  (timeit| "load package and config"
   (moon-load-package moon-star-path-list)
   (unless noninteractive
     (moon-load-config moon-star-path-list)))
  (timeit| "use-package"
    (require 'use-package)
   (moon-grand-use-package-call))
  )

(defun moon-load-autoload ()
  "Load `moon-autoload-file'."
  ;; (dolist (package-path moon-package-load-path)
  ;;   (when (file-directory-p package-path)
  ;;     (let (
  ;; 	        (file-list (directory-files package-path t ".+-autoloads.el$"))
  ;; 	        )
  ;;       (dolist (file file-list)
  ;; 	      (load file))
  ;;       )
  ;;     )
  ;;   )
  (load moon-autoload-file))

;; TEST
;; (defvar moon-star-dir "/Users/yuan/.emacs.d/star/")
;; (setq moon-star-path-list ())

(defun keyword-to-name-str (keyword)
  "Remove the colon in KEYWORD symbol and turn it into string.

i.e. :keyword to \"keyword\"."
  (replace-regexp-in-string "^:" "" (symbol-name keyword)))


(defun moon-load-config (path-list)
  "Load config.el in each star in PATH-LIST."
  (dolist (star-path path-list)
    (let ((path (concat star-path "config.el")))
      (if (file-exists-p path)
          (load path)
        (message (format "%s does not exist!" path)))
      )))

(defun moon-load-package (path-list)
  "Load package.el in each star in PATH-LIST."
  (dolist (star-path path-list)
    (let ((path (concat star-path "package.el")))
      (if (file-exists-p path)
          (load path)
        (message (format "%s does not exist!" path)))
      )))

;; TEST
;; (setq test-star-list '(:ui basic-ui))
;; (moon-generate-star-path test-star-list)


(defun moon-display-benchmark ()
  "Display the total time of loading lunarymacs."
  (message "loaded %s packages across %d stars in %.03fs"
           (length moon-package-list)
           (length moon-star-path-list)
           (setq moon-init-time (float-time (time-subtract (current-time) before-init-time)))
           ))

;;
;; Macro
;;

(defmacro load| (filesym &optional path)
  "Load FILESYM relative to current file path.

FILESYM is a symbol.

If PATH is given, load FILESYM relative to PATH.

Note: don't use this macro in `use-package|'
because forms in `use-package' are not evaluated
in the file in where they are wriiten."
  (let ((path (or path
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name (file-name-directory buffer-file-name))
                  (error "Could not detect path to look for '%s' in" filesym)))
        (filename (symbol-name filesym)))
    (load (concat path filename))))

(defmacro package| (&rest package-list)
  "Add packages in PACKAGE-LIST to ‘moon-package-list’.
so it will be installed by make.
Create `pre-init-xxx' and `post-config-xxx' functions.
Modify them with `pre-init|' and `post-config|' macro.

Can take multiple packages.
e.g. (package| evil evil-surround)"
  (dolist (package package-list)
    (add-to-list 'moon-package-list (symbol-name package))
    ;; (fset (intern (format "post-config-%s" (symbol-name package))) '(lambda () ()))
    ;; (fset (intern (format "pre-init-%s" (symbol-name package))) '(lambda () ()))
    ))

(defmacro moon| (&rest star-list)
  "Declare stars in STAR-LIST. Separate stars with sub-directories' name.
Basically adding path to star to `moon-star-path-list'.

Example: (moon| :feature evil :ui custom) for star/feature/evil
and star/ui/custom.
If called multiple times, the stars declared first will be
in the front of moon-star-list"
  (dolist (star star-list)
    (cond ((keywordp star) (setq mode star))
          ((not      mode) (error "No sub-folder specified in `moon|' for %s" star))
          (t               (let ((star-path (format "%s%s/%s/" moon-star-dir (keyword-to-name-str mode) star)))
                             (add-to-list 'moon-star-path-list star-path t)))
          )))

(defmacro post-config| (package &rest to-do-list)
  "Add expressions in TO-DO-LIST to be called after (use-package PACKAGE :config)."
  (declare (indent defun))
  (let (
        (func-symbol (intern (format "post-config-%s" package)))
        )
    (unless (fboundp func-symbol)
      (fset func-symbol '(lambda () ()))
      )
    (fset func-symbol (append (symbol-function func-symbol) to-do-list))
    ))

(defmacro pre-init| (package &rest to-do-list)
  "Add expressions in TO-DO-LIST to be called after (use-package PACKAGE :init)."
  (declare (indent defun))
  (let (
        (func-symbol (intern (format "pre-init-%s" package)))
        )
    (unless (fboundp func-symbol)
      (fset func-symbol '(lambda () ()))
      )
    (fset func-symbol (append (symbol-function func-symbol) to-do-list))
    ))

(defmacro after-load| (feature &rest rest-list)
  "A smart wrapper around `with-eval-after-load'.

FEATURE is a library declared with `provide'.
REST-LIST is a list of expressions to evaluate.

Expressions inside will be called right after the library is loaded,
before `post-config|' but after `pro-init'."
  (declare (indent defun) (debug t))
  `(with-eval-after-load ',feature ,@rest-list))

(defmacro use-package| (package &rest rest-list)
  "Thin wrapper around `use-package', just add some hooks.

Basically (use-package| evil :something something) adds
\(use-package :something something
:init (pre-init-evil)
:config (post-config-evil))
to `moon-grand-use-pacage-call'
to be evaluated at the end of `moon-initialize-star'"
  (declare (indent defun))
  `(fset
    'moon-grand-use-package-call
    (append
     (symbol-function 'moon-grand-use-package-call)
     '((use-package
         ,package
         ,@rest-list
         :init
         (let (
               (symb (intern (format "pre-init-%s" (symbol-name ',package))))
               )
           (when (fboundp symb)
             (eval (list symb)))
           )
         ;; (eval (list (intern (format "pre-init-%s" (symbol-name ',package)))))
         :config
         (let (
               (symb (intern (format "post-config-%s" (symbol-name ',package))))
               )
           (when (fboundp symb)
             (eval (list symb)))
           )
         ;; (eval (list (intern (format "post-config-%s" (symbol-name ',package)))))
         )))))


(defmacro customize| (&rest exp-list)
  "Set some customization in init.el.

Accepts expressions EXP-LIST, they will be run in `moon-post-init-hook'.
Expressions will be appended."
  `(add-hook 'moon-post-init-hook
             (lambda () ,@exp-list) t))



(defmacro add-hook-for-once| (hook func &optional append addlocal removelocal)
  "Add FUNC to HOOK. And remove FUNC from HOOK at first call of FUNC.

FUNC is a named function

If APPEND or ADDLOCAL is set, APPEND or ADDLOCAL is passed to `add-hook'
as APPEND and LOCAL. Similarly REMOVELOCAL is passed to `remove-hook' as LOCAL."
  `(fset (symbol-name ,func)
         (append
          (symbol-function ,func)
          '((remove-hook ',hook #',func ,removelocal))))
  `(add-hook ',hook #',func ,append ,addlocal)
  )

(defmacro delay-load| (func)
  "Add FUNC to `after-change-major-mode-hook'.
And remove FUNC from the hook at first call."
  `(add-hook 'after-change-major-mode-hook
             (lambda () (,func) (remove-hook 'after-change-major-mode-hook #',func))))


(defmacro async-load| (package &optional name)
  "Expand to a expression.

\(make-thread (lambda () (require 'PACKAGE)) NAME)

Use example:

\(use-package| PACKAGE :init (async-load| PACKAGE)"
  `(make-thread (lambda () (require ',package)) ,name))


;; (defun post-config-evil () (message "it works!"))
;; (defun pre-init-evil () (message "it works!"))

;; (use-package| evil :config (message "post config!"))

;; (defun post-config-gerneral () (message "evaluate general"))
;; (post-config| general (message "it works!"))

;;
;; Interactive
;;


(defun moon/install-package ()
  "Install packages specified in `package.el' files in each star."
  (interactive)
  (moon-initialize)
  ;; moon-star-path-list is created by `moon|' macro
  ;; moon-load-package loads `moon-package-list'
  (moon-load-package moon-star-path-list)
  (package-refresh-contents)
  (dolist (package moon-package-list)
    (unless (package-installed-p (intern package))
      (package-install (intern package))
      )))

(defun moon/update-package ()
  "Update packages to the latest version."
  (interactive)
  (moon-initialize)
  ;; moon-star-path-list is created by `moon|' macro
  ;; moon-load-package loads `moon-package-list'
  (moon-load-package moon-star-path-list)
  ;; https://oremacs.com/2015/03/20/managing-emacs-packages/
  
  ;; If there is no package to update,
  ;; package.el will throw "No operation specified"
  ;; but I didn't find any codd throwing error
  ;; in package.el...
  ;; TODO find out a better implementation
  (condition-case nil
   (save-window-excursion
     (package-list-packages t)
     (package-menu-mark-upgrades)
     (package-menu-execute t))
      (error nil)))

(defun moon/remove-unused-package ()
  "Remove packages that are not declared in any star with `package|' macro."
  (interactive)
  (moon-initialize-load-path)
  (moon-initialize)
  ;; moon-star-path-list is created by `moon|' macro
  ;; moon-load-package loads `moon-package-list'
  (moon-load-package moon-star-path-list)
  (dolist (package package-alist)
    (let ((package-name (car package))
          (package-description (car (cdr package))))
      (unless (member (symbol-name package-name) moon-package-list)
        ;; if the package is a dependency of other,
        ;; it will be be deleted, but rather throw an error.
        (ignore-errors (package-delete package-description)))
      )))

(defun moon/generate-autoload-file ()
  "Extract autload file from each star to `moon-autoload-file'."
  (interactive)
  (moon-initialize-load-path)
  (moon-initialize-star)
  (let ((autoload-file-list
         (file-expand-wildcards
	  ;; core autoload
          (expand-file-name "autoload/*.el" moon-core-dir))))
    ;; package autoload
    (dolist (file (directory-files-recursively
		   (concat moon-package-dir "elpa/")
		   "\\.el$"))
      (push (expand-file-name file) autoload-file-list))
    ;; star autoload
    (dolist (star-path moon-star-path-list)
      (let ((auto-dir (expand-file-name "autoload" star-path))
            (auto-file (expand-file-name "autoload.el" star-path)))
        (when (file-exists-p auto-file)
          (push auto-file autoload-file-list))
        (when (file-directory-p auto-dir)
          (dolist (file (directory-files-recursively auto-dir "\\.el$"))
            (push file autoload-file-list)))
        )
      (when (file-exists-p moon-autoload-file)
        (delete-file moon-autoload-file)
        (message "Delete old autoload file")
        )
      )
    (dolist (file (reverse autoload-file-list))
      (message
       (cond ((update-file-autoloads file t moon-autoload-file)
              "Nothing in %s")
             (t "Scanned %s"))
       (file-relative-name file moon-emacs-d-dir))
      )))

(provide 'core-package)
;;; core-package.el ends here

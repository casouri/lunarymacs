;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar moon-base-load-path
  (append (list moon-core-dir moon-star-dir)
          load-path)
  "A backup of `load-path' before it was altered by `moon-initialize'.

Contains only core dir ,star dir and load path for built in libraries")


(defvar moon-package-dir (concat moon-local-dir "package/")
  "Where melpa and elpa packages are installed.")


(defvar moon-package-list '(use-package bind-key)
  "A list of packages to install. Packages are represented by symbols.")

(defvar moon-quelpa-package-list ()
  "A list of packages to install by quelpa. Packages are represented by recipe list.")

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

(defvar moon-star-loaded nil
  "Whether `moon-load-star' has been called.")

(defvar moon-package-loaded nil
  "Whether `moon-load-package' has been called.")

(defvar moon-config-loaded nil
  "Whether `moon-load-config' has been called.")

(defvar moon-load-path-loaded nil
  "Whether `moon-initialize-load-path' has been called.")

(defvar moon-star-prepared nil
  "Whether `moon-initialize-star' has been called.")

(fset 'moon-grand-use-package-call
      '(lambda ()
         "A bunch of (use-package blah blah blah) collected by use-package| macro from each config file of stars."))

(defvar green-check "\033[00;32m✔\033[0m")
;;
;; Config
;;

(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-user-dir moon-package-dir
      package-archives
      '(("melpa" . "http://elpa.emacs-china.org/melpa/")
        ("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("org" . "https://orgmode.org/elpa/")))


;;
;; Func
;;

(defun moon-initialize ()
  "Initialize installed packages (using package.el)."
  (require 'package)
  (package-initialize t)
  )

(defun moon-initialize-load-path ()
  "Add each package to load path."
  (setq moon-package-load-path (directory-files moon-package-dir t nil t) ;; get all sub-dir
          load-path (append moon-base-load-path moon-package-load-path))
    (add-to-list 'load-path moon-local-dir)
    (setq moon-load-path-loaded t)
    )

(defun moon-initialize-star ()
  "Prepare each star in `moon-star-list'.
Then they can be loaded by `moon-load-star'."
  (unless noninteractive
    (moon-load-autoload))
  (timeit| "load package and config"
    ;; load twice, first time for `moon|',
    ;; second time for `package|'.
    ;; if there is a star which is some star's
    ;; dependencies' dependenciy,
    ;; if will not load.

    ;; save the original path list to another symbol
    ;; so the second run can load less file.
    (setq moon-star-path-list
          (let ((original-moon-star-path-list moon-star-path-list)
                (moon-star-path-list ())
                (previous-path-list-length 0))
            (defun get-path-list-length () (length moon-star-path-list))
            ;; moon-star-path-list will be the
            ;; additional paths
            ;; keep loading package.el until path-list doesn't grow anymore
            ;; that means all dependencies are loaded
            (moon-load-package original-moon-star-path-list)
            (while (not (eq previous-path-list-length (get-path-list-length)))
              (moon-load-package moon-star-path-list)
              (setq previous-path-list-length (get-path-list-length)))
            (append original-moon-star-path-list moon-star-path-list)))
    ;; now `moon|' and `package|' are evaled.
    ;; so 1) `moon-star-path-list' is ready
    ;; 2) additional packages are added to `moon-package-list'

    ;; in config.el, `use-package|' will
    ;; 1) add package to `moon-package-list'
    ;; 2) add `use-package' sexp to `moon-grand-use-package-call'
    (moon-load-config moon-star-path-list))
  (setq moon-star-prepared t)
  )

(defun moon-load-star ()
  "Actually load stars."
  (moon-initialize-star)
  (timeit| "use-package"
    (require 'use-package)
    (moon-grand-use-package-call))
  (setq moon-star-loaded t))

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
      ))
  (setq moon-config-loaded t))

(defun moon-load-package (path-list)
  "Load package.el in each star in PATH-LIST."
  (dolist (star-path path-list)
    (let ((path (concat star-path "package.el"))
          (config-path (concat star-path "config.el")))
      (if (file-exists-p path)
          (load path)
        (message (format "%s does not exist!" path)))
      ;; parse config.el and add all packages
      ;; declared in `use-package' and `use-packge|'
      ;; (when (file-exists-p path)
      ;;   (with-current-buffer (find-file path)
      ;;     (while (re-search-forward "use-package.? \\(.+?\\)\\b" nil t)
      ;;       (add-to-list 'moon-package-list (match-string-no-properties 1)))))
      ))
  (setq moon-package-loaded t))

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
  `(dolist (package ',package-list)
     (if (symbolp package)
         (add-to-list 'moon-package-list package)
       (print package)
       (add-to-list 'moon-quelpa-package-list package))
     ;; (fset (intern (format "post-config-%s" (symbol-name package))) '(lambda () ()))
     ;; (fset (intern (format "pre-init-%s" (symbol-name package))) '(lambda () ()))
     ))

(defmacro moon| (&rest star-list)
  "Declare stars in STAR-LIST.
Separate stars with sub-directories' name.
Basically adding path to star to `moon-star-path-list'.

Example: (moon| :feature evil :ui custom) for star/feature/evil
and star/ui/custom.
If called multiple times, the stars declared first will be
in the front of moon-star-list.

`moon|' can be used in star's `package.el',
but try to only depend on stars in `:basic' sub directory.

Because a star's dependencies' dependency will not be added automatically.
If your star's dependency star denpend of some other star,
that star will not be included by lunarymacs framework
when loading and installing packages.

In a word, denpend of stars that don't depend on other stars!"
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

If FEATURE loaded, rest-list is evaluated.
If not, this macro will befave like `with-eval-after-load'.

Expressions inside will be called right after the library is loaded,
before `post-config|' but after `post-init'."
  (declare (indent defun) (debug t))
  `(if (featurep ',feature)
        (progn ,@rest-list)
      (with-eval-after-load ',feature ,@rest-list)))

(defmacro get-package-symbol| (sexp)
  "If SEXP is a symbol, return it. If SEXP is a sequence, return car."
  `(if (symbolp ,sexp)
       ,sexp
     (car ,sexp)))

(defmacro use-package| (package &rest rest-list)
  "Thin wrapper around `use-package', just add some hooks.

Basically (use-package| evil :something something) adds
\(use-package :something something
:init (pre-init-evil)
:config (post-config-evil))
to `moon-grand-use-pacage-call'
to be evaluated at the end of `moon-initialize-star'

PACKAGE can also be a straight recipe."
  (declare (indent defun))
  `(progn
     (if (symbolp ',package)
         (add-to-list 'moon-package-list ',package)
       (print ',package)
       (add-to-list 'moon-quelpa-package-list ',package))
     (unless noninteractive
       (fset
        'moon-grand-use-package-call
        (append
         (symbol-function 'moon-grand-use-package-call)
         '((use-package
             ,(get-package-symbol| package)
             ,@rest-list
             :init
             (let ((symb (intern (format "pre-init-%s" (symbol-name ',(get-package-symbol| package))))))
               (when (fboundp symb)
                 (eval (list symb))))
             :config
             (let ((symb (intern (format "post-config-%s" (symbol-name ',(get-package-symbol| package))))))
               (when (fboundp symb)
                 (eval (list symb))))
             )))))))


(defmacro customize| (&rest exp-list)
  "Set some customization in init.el.

Accepts expressions EXP-LIST, they will be run in `moon-post-init-hook'.
Expressions will be appended."
  `(unless noninteractive
     (add-hook 'moon-init-hook
               (lambda () ,@exp-list) t)))

(defmacro async-load| (package &optional name)
  "Expand to a expression.

\(make-thread (lambda () (require 'PACKAGE)) NAME)

Use example:

\(use-package| PACKAGE :init (async-load| PACKAGE)"
  `(make-thread (lambda () (require ',package)) ,name))

(defmacro silent| (&rest form)
  "Run expressions in FORM without displaying message."
  `(let ((inhibit-message t))
    ,@form))


;; (defun post-config-evil () (message "it works!"))
;; (defun pre-init-evil () (message "it works!"))

;; (use-package| evil :config (message "post config!"))

;; (defun post-config-gerneral () (message "evaluate general"))
;; (post-config| general (message "it works!"))

;;
;; Interactive
;;


(defun moon/use-package ()
  "Install packages specified in `package.el' files in each star.

It will not print messages printed by `package-install'
because it's too verbose."
  (interactive)
  (package-refresh-contents)
  (bootstrap-quelpa)
  (unless moon-load-path-loaded
    (moon-initialize-load-path))
  ;; (moon-initialize)
  ;; moon-star-path-list is created by `moon|' macro
  ;; moon-load-package loads `moon-package-list'
  ;; (moon-load-package moon-star-path-list)
  (unless moon-star-prepared
    (moon-initialize-star))
  (print moon-quelpa-package-list)
  (dolist (package moon-quelpa-package-list)
    (message (format "Installing %s" (symbol-name (car package))))
    (quelpa package))
  (dolist (package moon-package-list)
    (unless (or (package-installed-p package)
                (require package nil t))
      (message (format "Installing %s" (symbol-name package)))
      ;; installing packages prints lot too many messages
      (silent| (condition-case nil
                   (package-install package)
                   (error nil)))
      )))

(defun moon/update-package ()
  "Update packages to the latest version.

It will not print messages printed by updating packages
because it's too verbose."
  (interactive)
  (moon-initialize)
  (unless moon-load-path-loaded
    (moon-initialize-load-path))
  ;; moon-star-path-list is created by `moon|' macro
  ;; moon-load-package loads `moon-package-list'
  ;; (moon-load-package moon-star-path-list)
  (unless moon-star-prepared
    (moon-initialize-star))
  ;; https://oremacs.com/2015/03/20/managing-emacs-packages/
  
  ;; If there is no package to update,
  ;; package.el will throw "No operation specified"
  ;; but I didn't find any code throwing error
  ;; in package.el...
  ;; TODO find out a better implementation
  (silent| ; don't print message
   (condition-case nil
       (save-window-excursion
         (package-list-packages t)
         (package-menu-mark-upgrades)
         (package-menu-execute t))
     (error nil)))
  )

(defun moon/remove-unused-package ()
  "Remove packages that are not declared in any star with `package|' macro."
  (interactive)

  (unless moon-load-path-loaded
    (moon-initialize-load-path))
  (moon-initialize)
  
  ;; moon-star-path-list is created by `moon|' macro
  ;; moon-load-package loads `moon-package-list'
  (unless moon-star-prepared
    ;; (moon-load-package moon-star-path-list)
    (moon-initialize-star))
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
  
  (unless moon-load-path-loaded
    (moon-initialize-load-path))
  ;; (unless moon-star-loaded
  ;;   (moon-initialize-star))

  (let ((autoload-file-list
         (file-expand-wildcards
	  ;; core autoload
          (expand-file-name "autoload/*.el" moon-core-dir)))
        (package-autoload-file-list ()))
    ;; package autoload
    (dolist (file (directory-files-recursively
		   moon-package-dir
		   "\\.el$"))
      (push (expand-file-name file) package-autoload-file-list))
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
    ;; autoload file in stars
    (dolist (file (reverse autoload-file-list))
      (message
       (cond ((update-file-autoloads file t moon-autoload-file)
              "Nothing in %s")
             (t "Scanned %s   %s"))
       (file-relative-name file moon-emacs-d-dir)
       green-check))
    ;; autoload files in packages
    (message "Loading autoload file from packages")
    (let ((count 0))
      (dolist (file (reverse package-autoload-file-list))
        (when (eq (% count 250) 0)
          (message "."))
        (setq count (1+ count))
        (update-file-autoloads file t moon-autoload-file)))
    (message green-check)))

(defun bootstrap-quelpa ()
  "Install quelpa."
  (package-initialize)
  (unless (require 'quelpa nil t)
    (package-install 'quelpa)
    (moon-initialize-load-path)))

(provide 'core-package)
;;; core-package.el ends here

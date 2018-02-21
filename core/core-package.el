;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar moon-base-load-path
  (append (list moon-core-dir moon-star-dir)
          load-path)
  "A backup of `load-path' before it was altered by `doom-initialize'.

Contains only core dir ,star dir and load path for built in libraries"
  )


(defvar moon-package-dir (concat user-emacs-directory ".local/package/")
  "Where melpa and elpa packages are installed.")

(defvar moon-package-list '("use-package")
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
        ("gnu"   . "https://elpa.gnu.org/packages/"))
      )


;;
;; Func
;;

;; TODO change (package-initialize) to (package-initialize t)
(defun moon-initialize ()
  "Initialize installed packages (using package.el)."
  ;; (setq package-activated-list nil
  ;;       package--init-file-ensured) ;; no `custom-set-variables' block in init.el
  ;;   (condition-case _ (package-initialize t)
  ;;     ('error (package-refresh-contents)
  ;;             (setq moon--refreshed-p t)
  ;;             (package-initialize)))
  (package-initialize)
  )

(defun moon-initialize-load-path ()
  "add each package to load path"
    ;; (setq moon-package-load-path (directory-files package-user-dir t nil t) ;; get all sub-dir
    ;;       load-path (append moon-base-load-path moon-package-load-path))
  (add-to-list 'load-path moon-local-dir)
    )

(defun moon-initialize-star ()
  "Load each star in `moon-star-list'."
  (unless noninteractive
    (moon-load-autoload))
  (setq moon-before-package-time (current-time))
  (moon-load-package moon-star-path-list)
  (unless noninteractive
    (moon-load-config moon-star-path-list)
    (message (format "load package and config time: %.03f" (float-time (time-subtract (current-time) moon-before-package-time))))
    (require 'use-package)
    (setq moon-before-grand-time (current-time))
    (moon-grand-use-package-call)
    (message (format "use-package-time: %.03f" (float-time (time-subtract (current-time) moon-before-grand-time))))
    )
  )

(defun moon-load-autoload ()
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
  "Remove the colon in keyword symbol and turn it into string."
  (replace-regexp-in-string "^:" "" (symbol-name keyword)))


(defun moon-load-config (path-list)
  "load config.el in each star"
  (dolist (star-path path-list)
    (let ((path (concat star-path "config.el")))
      (if (file-exists-p path)
          (load path)
        (message (format "%s does not exist!" path)))
      )
    )
  )

(defun moon-load-package (path-list)
  "load package.el in each star"
  (dolist (star-path path-list)
    (let ((path (concat star-path "package.el")))
      (if (file-exists-p path)
          (load path)
        (message (format "%s does not exist!" path)))
      )
    )
  )

;; TEST
;; (setq test-star-list '(:ui basic-ui))
;; (moon-generate-star-path test-star-list)


(defun moon-display-benchmark ()
  (message "loaded %s packages across %d stars in %.03fs"
           (length moon-package-list)
           (length moon-star-path-list)
           (setq moon-init-time (float-time (time-subtract (current-time) before-init-time)))
           )
  )

;;
;; Macro
;;

(defmacro load| (filesym &optional path)
  (let ((path (or path
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name (file-name-directory buffer-file-name))
                  (error "Could not detect path to look for '%s' in" filesym)))
        (filename (symbol-name filesym)))
    (load (concat path filename))
    )
  )

(defmacro package| (&rest package-list)
  "Add package to moon-package-list so it will be installed by make.
Create `pre-init-xxx' and `post-config-xxx' functions. Modify them with `pre-init|' and `post-config|' macro.

Can take multiple packages.
e.g. (package| evil evil-surround)"
  (dolist (package package-list)
    (add-to-list 'moon-package-list (symbol-name package))
    ;; (fset (intern (format "post-config-%s" (symbol-name package))) '(lambda () ()))
    ;; (fset (intern (format "pre-init-%s" (symbol-name package))) '(lambda () ()))
    ))

(defmacro moon| (&rest star-list)
  "Declare stars. Separate stars with sub-directories' name. Basically adding path to star to `moon-star-path-list'.

Example: (moon| :feature evil :ui custom) for star/feature/evil and star/ui/custom.
If called multiple times, the stars declared first will be in the front of moon-star-list"
  (dolist (star star-list)
    (cond ((keywordp star) (setq mode star))
          ((not      mode) (error "No sub-folder specified in `moon|' for %s" star))
          (t               (let ((star-path (format "%s%s/%s/" moon-star-dir (keyword-to-name-str mode) star)))
                             (add-to-list 'moon-star-path-list star-path t)))
          )
    )
  )

(defmacro post-config| (package &rest to-do-list)
  "Expressions to be called after (use-package PACKAGE :config)"
  (declare (indent defun))
  (let (
        (func-symbol (intern (format "post-config-%s" package)))
        )
    (unless (fboundp func-symbol)
      (fset func-symbol '(lambda () ()))
      )
    (fset func-symbol (append (symbol-function func-symbol) to-do-list))
    )
  )

(defmacro pre-init| (package &rest to-do-list)
  "Expressions to be called after (use-package PACKAGE :init)"
  (declare (indent defun))
  (let (
        (func-symbol (intern (format "pre-init-%s" package)))
        )
    (unless (fboundp func-symbol)
      (fset func-symbol '(lambda () ()))
      )
    (fset func-symbol (append (symbol-function func-symbol) to-do-list))
    )
  )

(defmacro after| (feature &rest rest-list)
  "A smart wrapper around `with-eval-after-load'.

Expressions inside will be called right after the library is loaded,
before `post-config|' but after `pro-init'."
  (declare (indent defun) (debug t))
  `(with-eval-after-load ',feature ,@rest-list)
   )

(defmacro use-package| (package &rest rest-list)
  "Thin wrapper around `use-package', just add some hooks.

It seems that this macro slows down init. Don't use it when you don't need
pre-init and post-config hooks.

Basically (use-package| evil :something something) adds
(use-package :something something :init (pre-init-evil) :config (post-config-evil))
to `moon-grand-use-pacage-call' to be evaluated at the end of `moon-initialize-star'"
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
         ))
     )
    )
  )


(defmacro customize| (&rest exp-list)
  "Set some customization in init.el.

Accepts expressions, they will be run in `moon-post-init-hook'.
Expressions will be appended."
  `(add-hook 'moon-post-init-hook
             (lambda () ,@exp-list) t))



(defmacro add-hook-for-once| (hook func &optional append addlocal removelocal)
"Add FUNC to HOOK. And remove FUNC from HOOK at first call of FUNC.

FUNC is a named function

If APPEND or ADDLOCAL is set, APPEND or ADDLOCAL is passed to `add-hook'
as APPEND and LOCAL. Similarly REMOVELOCAL is passed to `remove-hook' as LOCAL."
  `(add-hook ',hook
             (lambda ()
               (,func)
               (remove-hook ',hook #',func ,removelocal))
             ,append
             ,addlocal)
  )

(defmacro delay-load| (func)
  "Add FUNC to `after-change-major-mode-hook' 
and remove FUNC from the hook at first call."
  `(add-hook 'after-change-major-mode-hook
             (lambda () (,func) (remove-hook 'after-change-major-mode-hook #',func))))


(defmacro async-load| (package &optional name)
  "Expand to a expression.

(make-thread (lambda () (require 'PACKAGE)) NAME)

Use example:

(use-package| PACKAGE :init (async-load| PACKAGE))
"
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
  (interactive)
  (moon-initialize)
  (moon-initialize-star)
  (package-refresh-contents)
  (dolist (package moon-package-list)
    (unless
        (package-installed-p (intern package))
        (package-install (intern package))
        )
    ))

(defun moon/update-package ()
  (interactive)
  (moon-initialize)
  (moon-initialize-star)
  ;; https://oremacs.com/2015/03/20/managing-emacs-packages/
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun moon/remove-unused-package ()
  (interactive)
  (moon-initialize)
  (moon-initialize-star)
  (dolist (package package-activated-list)
    (when (member (symbol-name package) moon-package-list)
      (package-delete '(:name package)))
    )
  )

(defun moon/generate-autoload-file ()
  (interactive)
  (moon-initialize-star)
  (let ((autoload-file-list
         (file-expand-wildcards
	  ;; core autoload
          (expand-file-name "autoload/*.el" moon-core-dir))))
    ;; package autoload
    ;; (dolist (file (directory-files-recursively
		;;    (concat moon-package-dir "elpa/")
		;;    "\\.el$"))
    ;;   (push (expand-file-name file) autoload-file-list))
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
      )
    )
  )

(provide 'core-package)

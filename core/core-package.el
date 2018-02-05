;;
;; Var
;;

(defvar moon--base-load-path
  (append (list moon-core-dir moon-star-dir)
          load-path)
  "A backup of `load-path' before it was altered by `doom-initialize'.
   Contains only core dir ,star dir and load path for built in libraries"
  )

(defvar moon-package-dir (concat user-emacs-directory ".local/package/")
  "Where melpa and elpa packages are installed.")

(defvar moon-package-list ()
  "A list of packages to install. Packages are represented by strings not symbols.")

(defvar moon--refreshed-p nil
  "Have you refreshed contents?")

;; DEBUG
;; (setq moon-package-dir "/Users/yuan/.emacs.second/.local/package")

(defvar moon--package-load-path ()
  "The load path of package libraries installed via ELPA and QUELPA.")

(defvar moon-star-path-list ()
  "The path to each stars.")

(defvar moon-autoload-file (concat moon-local-dir "autoloads.el")
  "The path of autoload file which has all the autoload functions.")



;;
;; Func
;;

;; TODO change (package-initialize) to (package-initialize t)
(defun moon-initialize ()
  "Initialize installed packages (using package.el)."
  (setq package-activated-list nil
        package--init-file-ensured t) ;; no `custom-set-variables' block in init.el
    (condition-case _ (package-initialize)
      ('error (package-refresh-contents)
              (setq moon--refreshed-p t)
              (package-initialize)))
  )

(defun moon-initialize-load-path ()
  "add each package to load path"
    (setq moon--package-load-path (directory-files package-user-dir t nil t) ;; get all sub-dir
          load-path (append moon--base-load-path moon--package-load-path)))

(defun moon-initialize-star ()
  "Load each star in `moon-star-list'."
  ;; (moon-generate-star-path moon-star-list)
  (moon-load-package moon-star-path-list)
  (require 'use-package)
  (unless noninteractive
    (moon-load-autoload)
    (moon-load-config moon-star-path-list)
    )
  )

(defun moon-load-autoload ()
  (load moon-autoload-file))

;; TEST
;; (defvar moon-star-dir "/Users/yuan/.emacs.d/star/")
;; (setq moon-star-path-list ())

(defun keyword-to-name-str (keyword)
  ":something - : = something "
  (replace-regexp-in-string "^:" "" (symbol-name keyword)))


(defun moon-load-config (path-list)
  "load config.el in each star"
  (dolist (star-path path-list)
    (load (concat star-path "config.el"))))

(defun moon-load-package (path-list)
  "load package.el in each star"
  (dolist (star-path path-list)
    (load (concat star-path "package.el"))))

;; TEST
;; (setq test-star-list '(:ui basic-ui))
;; (moon-generate-star-path test-star-list)


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
    (fset (intern (format "post-config-%s" (symbol-name package))) '(lambda () ()))
    (fset (intern (format "pre-init-%s" (symbol-name package))) '(lambda () ()))
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
  "Add expression to a function that will be called in (use-package PACKAAGE :config)

"
  (let (
        (hook-symbol (intern (format "post-config-%s" package)))
        )
    (fset hook-symbol (append (symbol-function hook-symbol) to-do-list))
    )
  )

(defmacro pre-init (package &rest to-do-list)
  (let (
        (hook-symbol (intern (format "pre-init-%s" package)))
        )
    (fset hook-symbol (append (symbol-function hook-symbol) to-do-list))
    )
  )

;; (defun post-config-gerneral () (message "evaluate general"))
;; (post-config| general (message "it works!"))

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
;; Interactive
;;


(defun moon/install-package ()
  (interactive)
  (package-refresh-contents)
  (dolist (package moon-package-list)
    (package-install (intern package))
    (message (format "Installed %s" package))))

(defun moon/generate-autoload-file ()
  (interactive)
  (let ((autoload-file-list
         (file-expand-wildcards
          (expand-file-name "autoload/*.el" moon-core-dir))))
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

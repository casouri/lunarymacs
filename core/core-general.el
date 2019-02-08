;;; -*- lexical-binding: t -*-

;;; Commentary:
;;
;; No third parry packages supported in this file,
;; however you require package in  site-lisp once this file is loaded
;; more see the very bottom, Run Code section.

(require 'cl-lib)

;;; Variable

;;;; Directories

(defvar moon-emacs-d-dir (expand-file-name user-emacs-directory)
  "Basically ~/.emacs.d but a full path.")

(defvar moon-core-dir (concat moon-emacs-d-dir "core/")
  "Where core is located.")

(defvar moon-star-dir (concat moon-emacs-d-dir "star/")
  "Where stars shine.")

(defvar moon-local-dir (concat moon-emacs-d-dir ".local/")
  "Where package and other stuff goes. For files that are useful across sessions.")

(defvar moon-cache-dir (concat moon-emacs-d-dir ".cache/")
  "Where tmp files rest. For files that are dedicated to each session.")

;;;; Misc

(defvar moon-init-time nil
  "How long it takes for Emacs to start.")

(defvar lunary-version "1.2"
  "Current version of lunarymacs.")

(defvar moon-phase :startup
  "'startup or 'setup.")

(defvar moon-debug-on-startup nil
  "Enter debugger or ignore error.")

;;;; Package & loading

(defvar moon-package-dir (concat moon-local-dir "package/")
  "Where melpa and elpa packages are installed.")

(defvar moon-package-list '(use-package bind-key)
  "A list of packages to install. Packages are represented by symbols.")

(defvar moon-site-lisp-dir (concat moon-emacs-d-dir "site-lisp/"))

(defvar moon-quelpa-package-list ()
  "A list of packages to install by quelpa. Packages are represented by recipe list.")

(defvar moon-package-load-path ()
  "The load path of package libraries installed via ELPA and QUELPA.")

(defvar moon-star-path-list ()
  "The path to each stars.")

(defvar moon-autoload-file (concat moon-local-dir "autoload.el")
  "The path of autoload file which has all the autoload functions.")

(fset 'moon-grand-use-package-call
      '(lambda ()
         "A bunch of (use-package blah blah blah) collected by use-package| macro from each config file of stars."))

(defvar green-check "\033[00;32m✔\033[0m")
(defvar green-OK "\033[00;32mOK\033[0m")

(defvar red-cross "\033[00;31mX\033[0m")
(defvar red-ERROR "\033[00;31mERROR\033[0m")

(defvar moon-package-sub-dir-white-list (list "magit/lisp$")
  "A list of sub-dirs' regexp (of package) that are included in `load-path'.
For example, magit/lisp.")

;;
;;; Func
;;

(defun moon-safe-load (file &rest args)
  "Load FILE, if there is an error, print it an go on.
ARGS same as in `load'."
  (condition-case err
      (apply #'load file args)
    (error (message "error in file %s: %s" file err))))

(defun moon-directory-list (dir)
  "Return a list of directories under DIR. Return absolute path."
  (cl-remove-if (lambda (path) (not (file-directory-p path)))
                (directory-files dir t directory-files-no-dot-files-regexp)))

(defun moon-subdir-list (dir)
  "Return a list of sub-dirs that are included in `moon-package-sub-dir-white-list'.
Return absolute path."
  (cl-remove-if-not (lambda (path) (and (file-directory-p path)
                                        (catch 'match
                                          (dolist (regexp moon-package-sub-dir-white-list)
                                            (when (string-match regexp path)
                                              (throw 'match t))))))
                    (directory-files dir t directory-files-no-dot-files-regexp)))

(defun moon-set-load-path ()
  "Add each package to load path."
  (push moon-core-dir load-path)
  (dolist (dir (append (list moon-core-dir
                             moon-star-dir
                             moon-local-dir)
                       ;; moon-site-lisp-dir is added in Run Code section
                       ;; in this file, much earlier than this function
                       ;; would run
                       (directory-files-recursively moon-site-lisp-dir "" t)))
    (when (file-directory-p dir)
      (push dir load-path)))
  (dolist (package-dir (moon-directory-list moon-package-dir))
    (setq load-path
          (append load-path (moon-subdir-list package-dir)
                  (list package-dir)))))

(defun moon-load-star ()
  "Prepare each star in `moon-star-list'.
Then they can be loaded by `moon-load-star'."
  (load moon-autoload-file t)
  (moon-load-config moon-star-path-list)
  (moon-set-load-path)
  (timeit| "use-package"
    (require 'use-package)
    (moon-grand-use-package-call)
    ;; (read (find-file (concat moon-local-dir "startup.el")))
    ))

;; TEST
;; (defvar moon-star-dir "/Users/yuan/.emacs.d/star/")
;; (setq moon-star-path-list ())

(defun keyword-to-name-str (keyword)
  "Remove the colon in KEYWORD symbol and turn it into string.

i.e. :keyword to \"keyword\"."
  (substring (symbol-name keyword) 1))


(defun moon-load-config (path-list)
  "Load config.el in each star in PATH-LIST."
  (dolist (star-path path-list)
    (let ((path (concat star-path "config.el")))
      (if (file-exists-p path)
          (moon-safe-load path)
        (message (format "%s does not exist!" path))))))

(defun moon-display-benchmark ()
  "Display the total time of loading lunarymacs."
  (message "loaded %s packages across %d stars in %.03fs"
           (length moon-package-list)
           (length moon-star-path-list)
           (float-time (time-subtract (current-time) before-init-time))))

;;
;;; Macro
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
       (add-to-list 'moon-quelpa-package-list package))))

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
                             (add-to-list 'moon-star-path-list star-path t))))))

(defmacro post-config| (package &rest body)
  "Add expressions in BODY to be called after (use-package PACKAGE :config)."
  (declare (indent defun))
  (let ((func-symbol (intern (format "post-config-%s" (symbol-name package)))))
    `(progn (unless (fboundp #',func-symbol)
              (fset #',func-symbol '(lambda ())))
            (fset #',func-symbol (append (symbol-function #',func-symbol) ',body)))))

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
  (let ((post-func-symb (intern (format "post-config-%s" (symbol-name (get-package-symbol| package))))))
    `(progn
       (add-to-list 'moon-package-list ',package)
       (unless moon-setup
         (fset
          'moon-grand-use-package-call
          (append
           (symbol-function 'moon-grand-use-package-call)
           '((use-package
               ,(get-package-symbol| package)
               ,@rest-list
               :config
               (when (fboundp #',post-func-symb)
                 (funcall #',post-func-symb))))))))))


(defmacro customize| (&rest exp-list)
  "Set some customization in init.el.

Accepts expressions EXP-LIST, they will be run in `moon-startup-hook-1'.
Expressions will be appended."
  `(unless moon-setup
     (add-hook 'moon-startup-hook-1
               (lambda () ,@exp-list) t)))


(defmacro silent| (&rest form)
  "Run expressions in FORM without displaying message."
  `(let ((inhibit-message t))
    ,@form))

(defmacro timeit| (message &rest rest)
  "Time the execution of forms (REST) and print MESSAGE."
  (declare (indent 1))
  `(let ((start-time (current-time)))
     ,@rest
     (message (format "%s time: %.03f" ,message (float-time (time-subtract (current-time) start-time))))))

(defmacro this-dir| (var form)
  "Evaluate FORM with this directory binded to VAR."
  `(let ((,var (file-name-directory (or load-file-name buffer-file-name))))
     ,form))

(defun bootstrap ()
  "Bootstrap cowboy."
  (message "Load cowboy")
  (push moon-core-dir load-path)
  (require 'cowboy))


;;; Run code

(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-user-dir moon-package-dir)

;; Add site-lisp to load-path
;; so dash.el s.el and f.el are ready
(push moon-site-lisp-dir load-path)

(provide 'core-package)

;;; core-package.el ends here

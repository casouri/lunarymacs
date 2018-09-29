;;; cowboy.el --- Package mannager      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;; 

;;; Code:
;;

(require 'f)

;;; Variable

(defvar cowboy-package-dir package-user-dir
  "The directory where cowboy downloads packages to.")

(defvar cowboy-package-list nil
  "The package list.")

(defvar cowboy-recipe-alist '((isolate . (:repo "casouri/isolate"))
                              (aweshell . (:repo "manateelazycat/aweshell"))
                              (olivetti . (:repo "rnkn/olivetti"))
                              (ws-butler . (:repo "lewang/ws-butler"))
                              (use-package . (:repo "jwiegley/use-package"))
                              (bind-key . (:pseudo t :dependenc (use-package)))
                              (general . (:repo "noctuid/general.el"))
                              (which-key . (:repo "justbur/emacs-which-key"))
                              (hydra . (:repo abo-abo/hydra))
                              (rainbow-delimiters . (:repo "Fanael/rainbow-delimiters"))
                              (highlight-parentheses . (:repo "tsdh/highlight-parentheses.el"))
                              (minions . (:repo "tarsius/minions"))
                              (magit . (:repo "magit/magit" :dependency (with-editor magit-popup ghub async)))
                              (ghub . (:repo "magit/ghub" :dependency (graphql treepy)))
                              (treepy . (:repo "volrath/treepy.el"))
                              (graphql . (:repo "vermiculus/graphql.el"))
                              (async . (:repo "jwiegley/emacs-async"))
                              (magit-popup . (:repo "magit/magit-popup"))
                              (with-editor . (:repo "magit/with-editor"))
                              (moody . (:repo "tarsius/moody"))
                              (nyan-mode . (:repo "TeMPOraL/nyan-mode"))
                              (hl-todo . (:repo "tarsius/hl-todo"))
                              (form-feed . (:repo "wasamasa/form-feed"))
                              (buffer-move . (:repo "lukhas/buffer-move"))
                              (eyebrowse . (:repo "wasamasa/eyebrowse"))
                              (diff-hl . (:repo "dgutov/diff-hl"))
                              (expand-region . (:repo "magnars/expand-region.el"))
                              (avy . (:repo "abo-abo/avy"))
                              (minimap . (:repo "dengste/minimap"))
                              (outshine . (:repo "tj64/outshine" :dependency (outorg)))
                              (outorg . (:repo "alphapapa/outorg"))
                              (projectile . (:repo "bbatsov/projectile"))
                              (counsel-projectile . (:repo "ericdanan/counsel-projectile"))
                              (ivy . (:repo "abo-abo/swiper"))
                              (counsel . (:pseudo t :dependency (ivy)))
                              (swiper . (:pseudo t :depedec (ivy)))
                              (company . (:repo "company-mode/company-mode"))
                              (yasnippet . (:repo "joaotavora/yasnippet"))
                              (auto-yasnippet . (:repo "abo-abo/auto-yasnippet"))
                              (latex-preview-pane . (:repo "jsinglet/latex-preview-pane"))
                              (neotree . (:repo "jaypei/emacs-neotree"))
                              (awesome-tab . (:repo "manateelazycat/awesome-tab"))
                              (ranger . (:repo "ralesi/ranger.el"))
                              (dired-narrow . (:repo "Fuco1/dired-hacks"))
                              (toc-org . (:repo "snosov1/toc-org"))
                              (htmlize . (:repo "hniksic/emacs-htmlize"))
                              (flycheck . (:repo "flycheck/flycheck"))
                              (flyspell-correct-ivy . (:repo "d12frosted/flyspell-correct"))
                              (langtool . (:repo "mhayashi1120/Emacs-langtool"))
                              (sly . (:repo "joaotavora/sly"))
                              (lsp-mode . (:repo "emacs-lsp/lsp-mode"))
                              (lsp-python . (:repo "emacs-lsp/lsp-python"))
                              (pyvenv . (:repo "jorgenschaefer/pyvenv"))
                              (aggressive-indent . (:repo "Malabarba/aggressive-indent-mode"))
                              (lsp-typescript . (:repo "emacs-lsp/lsp-javascript"))
                              (web-mode . (:repo "fxbois/web-mode"))
                              (lua-mode . (:repo "immerrr/lua-mode"))
                              (doom-themes . (:repo "hlissner/emacs-doom-themes"))
                              (atom-one-dark-theme . (:repo "jonathanchu/atom-one-dark-theme"))
                              (lsp-ui . (:repo "emacs-lsp/lsp-ui"))
                              (company-lsp . (:repo "tigersoldier/company-lsp"))
                              (markdown-mode . (:repo "jrblevin/markdown-mode"))
                              (ivy-filthy-rich . (:repo "casouri/ivy-filthy-rich"))
                              (hungry-delete . (:repo "nflath/hungry-delete"))
                              (magit-todos . (:repo "alphapapa/magit-todos" :dependency (pcre2el)))
                              (pcre2el . (:repo "joddie/pcre2el"))
                              (jump-char . (:repo "lewang/jump-char"))
                              (typescript-mode . (:repo "ananthakumaran/typescript.el"))
                              (wanderlust . (:repo "wanderlust/wanderlust" :dependency (semi flim apel)))
                              (semi . (:repo "wanderlust/semi"))
                              (flim . (:repo "wanderlust/flim"))
                              (apel . (:repo "wanderlust/apel")))
  "Contains the recopies for each package.
This is an alist of form: ((package . properties)).

package is a symbol, properties is a plist.
Avaliable keywords: :fetcher, :repo, :dependency.

:fetcher is a symbol representing the source, avaliable options are 'github.
If none specified, default to 'github.

:repo is a string representing a repository from github, it shuold be like \"user/repo\".

TODO :branch fetch a particular branch of repo.

:dependency is a list of symbols of packages thar this package depends on.

:pseudo is for pseudo packages. for example, ivy, counsel & swiper are in one repo,
they you only need one recipe. The other two can be pseudo packages.

TODO :load-path is for additional load-path entries. By default cowboy adds package dir
and subdir under that into load-path, if the package needs to add subdirs that are deeper
to load-path, use this key to specify a relative path to package-dir. No preceeding slash or dot.")

;;; Function

(defun cowboy-add-load-path ()
  "Add packages to `load-path.'"
  (dolist (package-dir-path (f-directories cowboy-package-dir))
    (add-to-list 'load-path package-dir-path)
    (dolist (package-subdir-path (f-directories package-dir-path))
      (add-to-list 'load-path package-subdir-path))))

(defun cowboy-install (package &optional full-clone)
  "Install PACKAGE (a symbol) by cloning it down. Do nothing else.
By default use shadow-clone, if FULL-CLONE is t, use full clone."
  (cowboy--with-recipe
   (if (plist-get recipe :pseudo)
       t ; return with success immediately 
     (let ((dependency-list (plist-get recipe :dependency)))
       (when dependency-list
         (mapcar #'cowboy-install dependency-list)))
     (if (eq 0 (funcall (intern (format "cowboy--%s-clone"
                                        (symbol-name (or (plist-get :fetcher recipe) 'github))))
                        package-symbol (plist-get recipe :repo) full-clone))
         ;; exit code 0 means success, any other code means failure
         t
       nil))))

(defun cowboy--package-symbol (package)
  "PACKAGE can be a recipe, a symbol or a dir. Return package symbol."
  (pcase package
    ((pred symbolp) package)
    ((pred stringp) (intern (file-name-base package)))
    ((pred listp) (car package))
    (_ (error "Cannot make into package symbol: %s" package))))

(defmacro cowboy--with-recipe (&rest body)
  "With package recipe, eval BODY. Return nil if none found.
If PACKAGE is a symbol or list, treat as package,
if it is a string, treate as dir.

Variable PACKAGE should be defined prior to this macro,
inside the macro you get variable PACKAGE-SYMBOL and RECIPE."
  `(let* ((package-symbol (cowboy--package-symbol package))
          (recipe (if (listp package) ; in-place recipe always override recipe in cowboy-recipe-alist
                      (cdr package)
                    (alist-get package-symbol cowboy-recipe-alist))))
     (if recipe
         ,@body
       (message "Cannot find recipe for %s" (symbol-name package-symbol))
       nil)))

(defmacro cowboy--command (command dir &rest args)
  "Call process with COMMAND and ARGS in DIR."
  `(let ((default-directory ,dir))
     (call-process ,command nil "*COWBOY*" nil
                   ,@args)))

(defun cowboy--github-clone (package repo &optional full-clone)
  "Clone a REPO down and name it PACKAGE (symbol).
Shadow clone if FULL-CLONE nil. REPO is of form \"user/repo\"."
  (cowboy--command "git" cowboy-package-dir "clone" (unless full-clone "--depth")
                   (unless full-clone "1")
                   (format "https://github.com/%s.git" repo)
                   (symbol-name package)))

(defun cowboy-update (package)
  "Update PACKAGE from upstream.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir."
  (cowboy--with-recipe
   (if (eq 0 (funcall (intern (format "cowboy--%s-pull"
                                      (symbol-name
                                       (or
                                        (plist-get recipe :fetcher)
                                        'github))))
                      package))
       t
     nil)))

(defun cowboy--github-pull (package)
  "Pull PACKAGE from upstream.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir."
  (cowboy--command "git" (if (stringp package)
                             package
                           (concat cowboy-package-dir (symbol-name package) "/"))
                   "pull" "--rebase"))

(defun cowboy-delete (package)
  "Delete PACKAGE.
If PACKAGE is a symbol, treat as a package, if a string, treat as a dir."
  (delete-directory
   (if (stringp package)
       package
     (concat cowboy-package-dir (symbol-name (cowboy--package-symbol package)) "/"))
   t t)
  t)

(defun cowboy-reinstall (package)
  "Reinstall PACKAGE."
  (cowboy-delete package)
  (cowboy-install package))

(provide 'cowboy)

;;; cowboy.el ends here

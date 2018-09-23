;;; cowboy.el --- Package mannager      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

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
                              (ivy-filthy-rich . (:repo "casouri/ivy-filthy-rich"))
                              (hungry-delete . (:repo "nflath/hungry-delete"))
                              (magit-todos . (:repo "alphapapa/magit-todos" :dependency (pcre2el)))
                              (pcre2el . (:repo "joddie/pcre2el"))
                              (jump-char . (:repo "lewang/jump-char")))
  "Contains the recopies for each package.
This is an alist of form: ((package . properties)).

package is a symbol, properties is a plist.
Avaliable keywords: :fetcher, :repo, :dependency.

:fetcher is a symbol representing the source, avaliable options are 'github.
If none specified, default to 'github.

:repo is a string representing a repository from github, it shuold be like \"user/repo\".

:dependency is a list of symbols of packages thar this package depends on.")

;;; Function

(defun cowboy-initialize ()
  "Dress up cowboy. Scans `cowboy-package-dir' and update `cowboy-package-list'."
  (dolist (package-dir-path (f-directories cowboy-package-dir))
    (push (file-name-base (directory-file-name package-dir-path)) cowboy-package-list)))

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
                        package (plist-get recipe :repo) full-clone))
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
if it is a string, treate as dir."
  `(let* ((package-symbol (cowboy--package-symbol package))
          (recipe (if (listp package)
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

(provide 'cowboy)

;;; cowboy.el ends here

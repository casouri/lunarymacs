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
                              (smex . (:repo "nonsequitur/smex"))
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
                              (awesome-tab . (:repo "manateelazycat/awesome-tab" :dependency (tabbar)))
                              (tabbar . (:repo "dholm/tabbar"))
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
                              (apel . (:repo "wanderlust/apel"))
                              (quickrun . (:repo "syohex/emacs-quickrun"))
                              (visual-regexp . (:repo "benma/visual-regexp.el"))
                              (multiple-cursors . (:repo "magnars/multiple-cursors.el"))
                              (eglot . (:repo "joaotavora/eglot" :dependency (jsonrpc)))
                              (nyan-lite . (:repo "casouri/nyan-lite"))
                              (zone-nyan . (:repo "wasamasa/zone-nyan" :dependency (esxml)))
                              (esxml . (:repo "tali713/esxml"))
                              (helpful . (:repo "Wilfred/helpful" :dependency (elisp-refs shut-up)))
                              (elisp-refs . (:repo "Wilfred/elisp-refs" :dependency (loop)))
                              (loop . (:repo "Wilfred/loop.el"))
                              (shut-up . (:repo "cask/shut-up"))
                              (pp+ . (:fetcher url :url "https://www.emacswiki.org/emacs/download/pp%2b.el"))
                              (rainbow-mode . (:fetcher url :url "https://raw.githubusercontent.com/emacsmirror/rainbow-mode/master/rainbow-mode.el"))
                              (json-rpc . (:fetcher url :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/jsonrpc.el"))
                              (undo-tree . (:fetcher url :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/undo-tree/undo-tree.el"))
                              )
  "Contains the recopies for each package.
This is an alist of form: ((package . properties)).

package is a symbol, properties is a plist.
Avaliable keywords: :fetcher, :repo, :dependency.

:fetcher is a symbol representing the source, avaliable options are 'github.
If none specified, default to 'github.

:repo is a string representing a repository from github, it shuold be like \"user/repo\".

TODO :branch fetch a particular branch of repo.

:dependency is a list of symbols of packages thar this package depends on.
p
:pseudo is for pseudo packages. for example, ivy, counsel & swiper are in one repo,
they you only need one recipe. The other two can be pseudo packages.

TODO :load-path is for additional load-path entries. By default cowboy adds package dir
and subdir under that into load-path, if the package needs to add subdirs that are deeper
to load-path, use this key to specify a relative path to package-dir. No preceeding slash or dot.")

;;; Function

;;;; Userland

(defun cowboy-install (package &optional full-clone error)
  "Install PACKAGE (a symbol, a recipe or a directory) by cloning it down.
Do nothing else.
By default use shallow clone, if FULL-CLONE is t, use full clone.

If package is a directory string,
the directory file name will be used as package name.

ERROR is passes to `cowboy--error' as FUNC."
  (cowboy--with-recipe
   (if (plist-get recipe :pseudo)
       t ; return with success immediately
     ;; handle dependency
     (let ((dependency-list (plist-get recipe :dependency)))
       (when dependency-list
         (mapcar (lambda (package) (cowboy-install package full-clone error)) dependency-list)))
     ;; install, return t if success, nil if fail
     (funcall (intern (format "cowboy--%s-install"
                              (symbol-name (or (plist-get recipe :fetcher) 'github))))
              package-symbol recipe full-clone))))

(defun cowboy-update (package &optional error)
  "Update PACKAGE from upstream.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir.

ERROR is passes to `cowboy--error' as FUNC."
  (cowboy--with-recipe
   ;; handle dependency
   (let ((dependency-list (plist-get recipe :dependency)))
     (when dependency-list
       (mapcar (lambda (package) (cowboy-update package error)) dependency-list)))
   ;; return t if success, nil if fail
   (funcall (intern (format "cowboy--%s-update"
                            (symbol-name
                             (or
                              (plist-get recipe :fetcher)
                              'github))))
            package)))

(defun cowboy-delete (package &optional error)
  "Delete PACKAGE.
If PACKAGE is a symbol, treat as a package, if a string, treat as a dir.

ERROR is passes to `cowboy--error' as FUNC."
  ;; TODO revise
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

(defun cowboy-compile ()
  "Compile all packages."
  ;; cpmpile all file but only when .elc file is older than .el file
  (byte-recompile-directory cowboy-package-dir 0))

(defun cowboy-add-load-path ()
  "Add packages to `load-path'."
  (dolist (package-dir-path (f-directories cowboy-package-dir))
    (add-to-list 'load-path package-dir-path)
    (dolist (package-subdir-path (f-directories package-dir-path))
      (add-to-list 'load-path package-subdir-path))))

;;;; Backstage

;;;;; Helpers

(defun cowboy--package-symbol (package)
  "PACKAGE can be a recipe, a symbol or a dir. Return package symbol."
  (pcase package
    ((pred symbolp) package)
    ((pred stringp) (intern (file-name-base (directory-file-name package))))
    ((pred listp) (car package))
    ;; TODO rephrase
    (_ (error "Cannot make into package symbol: %s" package))))

(defmacro cowboy--with-recipe (&rest body)
  "Process package.
With package recipe, eval BODY. Return nil if none found.
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

(defvar cowboy--default-error-func (lambda (err) (apply #'message (cdr err)))
  "The default error handling function used by `cowboy--error'.")

(defmacro cowboy--handle-error (form &optional func)
  "Eval FORM. Use function FUNC to handle error.

If FUNC is nil, use `cowboy--default-error-func'.

Return t if success, nil if fail."
  `(condition-case err (progn ,form t)
     (error (funcall (or ,func cowboy--default-error-func) err)
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

(defun cowboy--github-install (package recipe &optional full-clone)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol).
Shadow clone if FULL-CLONE nil. REPO is of form \"user/repo\". Return 0 if success."
  (cowboy--handle-error
   (cowboy--command "git" cowboy-package-dir "clone" (unless full-clone "--depth")
                    (unless full-clone "1")
                    (format "https://github.com/%s.git" (plist-get recipe :repo))
                    (symbol-name package))))





(defun cowboy--github-update (package)
  "Pull PACKAGE from upstream.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir."
  (cowboy--handle-error
   (cowboy--command "git" (if (stringp package)
                              package
                            (concat cowboy-package-dir (symbol-name package) "/"))
                    "pull" "--debase")))



;;;;;; URL

(defun cowboy--url-install (package recipe &optional _)
  "Download the PACKAGE (file) directly from URL. Return 0 is success."
  (with-current-buffer (url-retrieve-synchronously
                        (plist-get recipe :url) t nil 10)
    (let ((file-content (buffer-substring (point-min) (point-max)))
          (dir (format "%s%s/" cowboy-package-dir package)))
      (unless (file-exists-p dir) (mkdir dir))
      (find-file (format "%s%s/%s.el" cowboy-package-dir package package))
      (insert file-content)
      (save-buffer)
      0)
    ;; (let ((redirection (plist-get status :redirect)))
    ;;   (if redirection
    ;;       (cowboy--http-clone package (plist-put recipe 'url redirection))
    ;;     ;; current buffer is retrieved data
    ;;     ))
    ))

(defun cowboy--url-update (package)
  "Download PACKAGE again.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir."
  ;; TODO
  (cowboy-delete package)
  (cowboy-install package))


(provide 'cowboy)

;;; cowboy.el ends here

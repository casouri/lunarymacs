;;; cowboy.el --- Package manager      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;;;; Conventions:
;;
;; 1. Use `cowboy--handle-error' in fetchers.
;;    This guarantees fetchers to return t when success
;;    and handles error with `cowboy--default-error-func'.
;;;; Notes:
;;
;; 1. You can use a recipe when installing package, e.g. (cowboy-install (color-rg :fetcher github :repo "manateelazycat/color-rg"))
;;    But you can't update it because cowboy doesn't have the recipe of it. So this method is not recommended.
;;; Code:
;;


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
                              (bind-key . (:pseudo t :dependency (use-package)))
                              (general . (:repo "noctuid/general.el"))
                              (which-key . (:repo "justbur/emacs-which-key"))
                              (hydra . (:repo "abo-abo/hydra"))
                              (rainbow-delimiters . (:repo "Fanael/rainbow-delimiters"))
                              (highlight-parentheses . (:repo "tsdh/highlight-parentheses.el"))
                              (minions . (:repo "tarsius/minions" :dependency (dash)))
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
                              (flycheck . (:repo "flycheck/flycheck" :dependency (dash)))
                              (flyspell-correct-ivy . (:repo "d12frosted/flyspell-correct"))
                              (langtool . (:repo "mhayashi1120/Emacs-langtool"))
                              (sly . (:repo "joaotavora/sly"))
                              (lsp . (:repo "emacs-lsp/lsp-mode" :dependency (ht f spinner)))
                              (company-lsp . (:repo "tigersoldier/company-lsp"))
                              (company-box . (:repo "sebastiencs/company-box"))
                              (lsp-ui . (:repo "emacs-lsp/lsp-ui"))
                              (spinner . (:repo "Malabarba/spinner.el"))
                              (dash . (:repo "magnars/dash.el"))
                              (f . (:repo "rejeep/f.el" :dependency (dash s)))
                              (s . (:repo "magnars/s.el"))
                              (ht . (:repo "Wilfred/ht.el" :dependency (dash)))
                              (lsp-python . (:repo "emacs-lsp/lsp-python"))
                              (pyvenv . (:repo "jorgenschaefer/pyvenv"))
                              (aggressive-indent . (:repo "Malabarba/aggressive-indent-mode"))
                              (lsp-typescript . (:repo "emacs-lsp/lsp-javascript"))
                              (web-mode . (:repo "fxbois/web-mode"))
                              (lua-mode . (:repo "immerrr/lua-mode"))
                              (doom-themes . (:repo "hlissner/emacs-doom-themes"))
                              (atom-one-dark-theme . (:repo "jonathanchu/atom-one-dark-theme"))
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
                              (jsonrpc . (:fetcher url :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/jsonrpc.el"))
                              (undo-tree . (:fetcher url :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/undo-tree/undo-tree.el"))
                              (objed . (:repo "clemera/objed"))
                              (helm . (:repo "emacs-helm/helm" :dependency (async popup-el)))
                              (popup-el . (:repo "auto-complete/popup-el"))
                              (helm-swoop . (:repo "ShingoFukuyama/helm-swoop" :dependency (helm)))
                              (ox-rss . (:pseudo t))
                              (dired+ . (:fetcher url :url "https://www.emacswiki.org/emacs/download/dired%2b.el"))
                              (dired-explore . (:repo "zk-phi/dired-explore"))
                              (org-download . (:repo "abo-abo/org-download" :dependency (async)))
                              (chinese-word-at-point . (:repo "xuchunyang/chinese-word-at-point.el"))
                              (helm-c-yasnippet . (:repo "emacs-jp/helm-c-yasnippet"))
                              (color-rg . (:repo "manateelazycat/color-rg"))
                              (ccls . (:repo "MaskRay/emacs-ccls"))
                              (writegood-mode . (:repo "bnbeckwith/writegood-mode"))
                              (eldoc-box . (:repo "casouri/eldoc-box")))
  "Contains the recopies for each package.
This is an alist of form: ((package . properties)).

package is a symbol, properties is a plist.
Avaliable keywords: :fetcher, :repo, :dependency, :pseudo.

:fetcher is a symbol representing the source, available options are 'github, 'url.
If none specified, default to 'github.

:repo is a string representing a repository from github, it should be like \"user/repo\".

TODO :branch fetch a particular branch of repo.

:dependency is a list of symbols of packages thar this package depends on.
p
:pseudo is for pseudo packages. for example, ivy, counsel & swiper are in one repo,
then you only need one recipe. The other two can be configured as pseudo packages.

TODO :load-path is for additional load-path entries. By default cowboy adds package dir
and subdir under that into load-path, if the package needs to add subdirs that are deeper
to load-path, use this key to specify a relative path to package-dir. No preceeding slash or dot.")

;;; Function

;;;; Userland

(defun cowboy-install (package &optional full-clone error)
  "Install PACKAGE (a symbol, a recipe or a directory) by cloning it down.
Do nothing else (no autoload, no byte compile). Return t if success, nil if fail.
By default use shallow clone, if FULL-CLONE is t, use full clone.

If package is a directory string,
the directory file name will be used as package name.

ERROR is passes to `cowboy--handle-error' as FUNC."
  (cowboy--only-with-recipe
   (if (plist-get recipe :pseudo)
       t ; return with success immediately
     ;; handle dependency
     (let ((dependency-list (plist-get recipe :dependency)))
       (when dependency-list
         (mapcar (lambda (package) (unless (cowboy-installedp package)
                                     (cowboy-install package full-clone error)))
                 dependency-list)))
     ;; install, return t if success, nil if fail
     (funcall (intern (format "cowboy--%s-install"
                              (symbol-name (or (plist-get recipe :fetcher) 'github))))
              package-symbol recipe full-clone))))

(defun cowboy-update (package &optional error)
  "Update PACKAGE from upstream. Return t if success, nil if fail.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir.

ERROR is passes to `cowboy--handle-error' as FUNC."
  (cowboy--only-with-recipe
   (if (plist-get recipe :pseudo)
       t
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
              package-symbol recipe))))

(defun cowboy-delete (package &optional error)
  "Delete PACKAGE.  Return t if success, nil if fail.
If PACKAGE is a symbol, treat as a package, if a string, treat as a dir.

ERROR is passes to `cowboy--handle-error' as FUNC."
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
  (let ((inhibit-message t))
    (byte-recompile-directory cowboy-package-dir 0)))

(defun cowboy-add-load-path ()
  "Add packages to `load-path'."
  (dolist (package-dir-path (cowboy--directory-list cowboy-package-dir))
    (add-to-list 'load-path package-dir-path)
    (dolist (package-subdir-path (cowboy--directory-list package-dir-path))
      (add-to-list 'load-path package-subdir-path))))

;;;; Backstage

;;;;; Helpers

(defun cowboy--directory-list (dir)
  "Return a list of directories under DIR. Return absolute path."
  (cl-remove-if (lambda (path) (not (file-directory-p path)))
                (directory-files dir t directory-files-no-dot-files-regexp)))

(defvar cowboy--all-file-in-load-path nil
  "All the base file names in file path.")

(defvar cowboy--old-load-path load-path
  "If this doesn't equal to `load-path', update `cowboy--all-file-in-load-path'.")

(defvar cowboy-ignore-package-list nil
  "A list of symbols of ignored system packages.")

(defun cowboy--all-file-in-load-path ()
  "Return a list of base file names of all files in load path."
  (if (and (equal cowboy--old-load-path load-path)
           cowboy--all-file-in-load-path)
      cowboy--all-file-in-load-path
    (setq cowboy--old-load-path load-path)
    (setq cowboy--all-file-in-load-path ; setq and return
          (append (mapcar #'file-name-base (mapcan (lambda (dir) (directory-files-recursively dir "\\.el$")) load-path))
                  (mapcar #'file-name-base (cowboy--directory-list cowboy-package-dir))))))

(defun cowboy-installedp (package)
  "Return t if PACKAGE (symbol, recipe, dir string) is installed, nil if not."
  (cowboy--with-recipe
   (if (or (plist-get recipe :system)
           (member package-symbol cowboy-ignore-package-list)
           (member (symbol-name package-symbol) (cowboy--all-file-in-load-path)))
       t
     nil)))

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
  `(cowboy--with-recipe
    (if recipe
        ,@body
      (message "Cannot find recipe for %s" (symbol-name package-symbol))
      nil)))

(defmacro cowboy--with-recipe (&rest body)
  "Process package and evaluate BODY.
If PACKAGE is a symbol or list, treat as package,
if it is a string, treate as dir.

Variable PACKAGE should be defined prior to this macro,
inside the macro you get variable `package-symbol' and `recipe'."
  `(let* ((package-symbol (cowboy--package-symbol package))
          (recipe (if (listp package) ; in-place recipe always override recipe in cowboy-recipe-alist
                      (cdr package)
                    (alist-get package-symbol cowboy-recipe-alist))))
     ,@body))

(defvar cowboy--default-error-func (lambda (err) (message (error-message-string err)))
  "The default error handling function used by `cowboy--handle-error'.")

(defmacro cowboy--handle-error (form &optional func)
  "Eval FORM. Use function FUNC to handle error.

If FUNC is nil, use `cowboy--default-error-func'.

Return t if success, nil if fail."
  `(condition-case err (progn ,form t)
     ((debug error) (funcall (or ,func cowboy--default-error-func) err)
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
Shadow clone if FULL-CLONE nil. REPO is of form \"user/repo\". Return 0 if success.
Return t if success, nil if fail."
  (cowboy--handle-error
   (cowboy--command "git" cowboy-package-dir "clone" (unless full-clone "--depth")
                    (unless full-clone "1")
                    (format "https://github.com/%s.git" (plist-get recipe :repo))
                    (symbol-name package))))

(defun cowboy--github-shallowp (package)
  "Return t if PACKAGE (a symbol, a recipe or a directory) is shallow cloned, nil if not."
  (let ((default-directory (format "%s%s/" cowboy-package-dir (symbol-name (cowboy--package-symbol package)))))
    (with-temp-buffer
      (if (eq 0 (funcall #'call-process "git" nil t nil
                         "rev-parse" "--is-shallow-repository"))
          ;; return t if true (shallow), nil if false (not shallow)
          (if (search-backward "true" nil t) t nil)
        nil))))

(defun cowboy--github-update (package recipe)
  "Pull PACKAGE with RECIPE from upstream. Return t if success, nil if fail.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir."
  (cowboy--handle-error
   (if (cowboy--github-shallowp package)
       ;; simply reinstall
       (progn (cowboy-delete package)
              (cowboy--github-install package recipe))
     (cowboy--command "git" (if (stringp package)
                                package
                              (concat cowboy-package-dir (symbol-name package) "/"))
                      "fetch"))))



;;;;;; URL

(defun cowboy--url-install (package recipe &optional _)
  "Download the PACKAGE (file) directly from URL.
Return t if success, nil if fail."
  (cowboy--handle-error
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
     )))

(defun cowboy--url-update (package recipe)
  "Download PACKAGE with RECIPE again.
Return t if success, nil if fail.
If PACKAGE is a symbol, treate as a package, if it is a string, treat as a dir."
  ;; TODO
  (cowboy-delete package)
  (cowboy--url-install package recipe))


(provide 'cowboy)

;;; cowboy.el ends here

;;; -*- lexical-binding: t -*-
;;
;; Variables, functions and macros used by for config files

;;; UI

(require 'lunary-ui)

;;; Variables

(defvar luna-autoload-file (expand-file-name "autoload.el" user-emacs-directory)
  "The path of autoload file which has all the autoload functions.")

(defvar luna-star-enable-alist nil
  "((star1 . t) (star2 . nil)), then start1 is enabled and star2 is not.")

(defvar luna-cache-dir (expand-file-name "cache" user-emacs-directory)
  "The dir for cache files.")

(defvar luna-package-list nil
  "List of package symbols. Added by ‘load-package’.")

(defvar luna-dumped nil
  "non-nil when a dump file is loaded (because dump.el sets this variable).")

(defvar luna-dumped-load-path nil
  "By default dump files doesn’t save ‘load-path’.
We need to manually save and restore it. See manual for more info.")

(defvar luna-dump-file (expand-file-name "emacs.pdmp" luna-cache-dir)
  "Location of dump file.")

;;; Loading functions

(defun luna-safe-load (file &rest args)
  "Load FILE and don’t error out.
If FILE doesn’t exist, create it.
ARGS is as same as in `load'."
  (if (file-exists-p file)
      (condition-case err
          (apply #'load file args)
        ((debug error) (warn "Error when loading %s: %s" file
                             (error-message-string err))))
    ;; Create file.
    (write-region "" nil file)))

(defun luna-load-relative (file &rest args)
  "Load FILE relative to user-emacs-directory.
ARGS are applied to ‘load'."
  (apply #'luna-safe-load
         (expand-file-name file user-emacs-directory) args))

;;; Load package

(require 'luna-load-package)

;;; Package functions

(defun luna-install-all ()
  "Install all required packages."
  (interactive)
  (dolist (package luna-package-list)
    (unless (cowgirl-installed-p package)
      (cowgirl-install package))))

;;; Convenience macros

(defmacro luna-lsp/eglot (lsp eglot)
  "Run LSP or EGLOT based on `luna-lsp'."
  `(pcase luna-lsp
     ('lsp ,lsp)
     ('eglot ,eglot)))

(defmacro luna-when-mac (&rest body)
  "Evaluate BODY when in a Mac system."
  `(when (eq system-type 'darwin)
     ,@body))

(defmacro luna-when-linux (&rest body)
  "Evaluate BODY when in a GNU/Linux system."
  `(when (eq system-type 'gnu/linux)
     ,@body))

(defmacro luna-if-dump (then &rest else)
  "Evaluate THEN if running with a dump file, else evaluate ELSE."
  (declare (indent 1))
  `(if luna-dumped
       ,then
     ,@else))

;;; Dump

(defun luna-dump ()
  "Dump Emacs."
  (interactive)
  (let ((buf "*dump process*"))
    (delete-file "/Applications/Emacs.app/Contents/MacOS/Emacs.pdmp")
    (make-process
     :name "dump"
     :buffer buf
     :command (list "/Applications/Emacs.app/Contents/MacOS/Emacs"
                    "--batch" "-Q"
                    "-l" (luna-f-join user-emacs-directory
                                      "dump.el")))
    (display-buffer buf)))

;;; Format on save

(defvar luna-smart-format-alist ()
  "Alist of format functions of each major mode.
Each element should be a con cell of major mode symbol and function symbol.
For example, '(python-mode . format-python)")

(defvar-local luna-format-on-save nil
  "Whether to format on save.")

(defun luna-smart-format-buffer ()
  "Only format buffer when `luna-format-on-save' is non-nil."
  (interactive)
  (when luna-format-on-save
    (let ((format-func (alist-get major-mode luna-smart-format-alist)))
      (when format-func
        (funcall format-func)))))

(add-hook 'after-save-hook #'luna-smart-format-buffer)

;;; buffer ordering

(defvar luna-buffer-bottom-list nil
  "Buffer name patterns that stays at the bottom of buffer list in helm.
Each pattern is the beginning of the buffer name, e.g., *Flymake, magit:, etc.")

(provide 'lunary)

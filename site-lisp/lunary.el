;;; lunary.el --- Helpers for config files      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Variables, functions and macros used by config files.

;;; Code:
;;

(require 'lunary-ui)
(require 'cowboy)

;;; Variables

(defvar luna-data-dir (expand-file-name "var" user-emacs-directory))

(defvar luna-package-list nil
  "List of package symbols. Added by ‘load-package’.")

(defvar luna-external-program-list nil
  "List of external programs needed. Added by ‘load-package’.
A list of notes, really.")

(defvar luna-dumped nil
  "non-nil when a dump file is loaded.
(Because dump.el sets this variable to t.)")

(defvar luna-dumped-load-path nil
  "By default dump files doesn’t save ‘load-path’.
We need to manually save and restore it. See manual for more info.")

(defvar luna-dump-location-alist
  '((Emacs "/Users/yuan/emacs-head/nextstep/Emacs.app/Contents/MacOS/Emacs"
           "/Users/yuan/emacs-head/nextstep/Emacs.app/Contents/MacOS/Emacs.pdmp"
           "/Users/yuan/emacs-head/nextstep/Emacs.app/Contents/MacOS/libexec/Emacs.pdmp")
    (Emacs-27 "/Applications/Emacs 27.app/Contents/MacOS/Emacs"
              "/Applications/Emacs 27.app/Contents/MacOS/Emacs.pdmp"
              "/Applications/Emacs 27.app/Contents/MacOS/libexec/Emacs.pdmp"))
  "An alist of (Name . LOCATION-LIST).
LOCATION-LIST is (BINARY-PATH DUMP-PATH). BINARY-PATH is the path
to the Emacs binary, DUMP-PATH is the path to the dump file, it
cannot be “emacs.pdmp” because that’s the original dump’s name.")

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

(eval-when-compile
  (require 'luna-load-package))

;;; Define key

(eval-when-compile
  (require 'luna-key))

;;; Package functions

(defun luna-install-all ()
  "Install all required packages."
  (interactive)
  (display-buffer "*Messages*")
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

(defmacro luna-on (host &rest body)
  "Evaluate BODY when running on HOST.
HOST can be a string or a list of strings.
You can see your host name by

    $ hostname

and change it with

    $ hostname <new name>

To make the change persist reboot, use

    $ scutil --set HostName <new name>"
  (declare (indent 1))
  `(when (if (stringp ,host)
             (equal ,host (system-name))
           (member (system-name) ,host))
     ,@body))

;;; Dump

(defun luna-dump (emacs-location dump-location orig-dump-location)
  "Dump Emacs.
Run Emacs at EMACS-LOCATION and dump to DUMP-LOCATION.
ORIG-DUMP-LOCATION is location of the original pre-built dump."
  (interactive
   (alist-get (intern (completing-read
                       "Location: "
                       (mapcar (lambda (elt)
                                 (symbol-name (car elt)))
                               luna-dump-location-alist)))
              luna-dump-location-alist))
  (let ((buf "*dump process*"))
    (delete-file dump-location)
    (make-process
     :name "dump"
     :buffer buf
     :command
     (list emacs-location
           "--batch" "-Q"
           "--dump-file" orig-dump-location
           "--eval"
           ;; Don’t add quote around!
           (format "(setq luna-dump-file \"%s\")" dump-location)
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

;;; External program

(defun luna-check-external-program ()
  "Check if external programs are available."
  (interactive)
  (pop-to-buffer (get-buffer-create "*external program*"))
  (erase-buffer)
  (let* ((programs (mapcar (lambda (p)
                             (car (split-string p)))
                           luna-external-program-list))
         (notes (mapcar (lambda (p)
                          (string-join (cdr (split-string p)) " "))
                        luna-external-program-list))
         (max-len (apply #'max (mapcar #'length programs)))
         (align (propertize
                 " " 'display `(space :align-to ,(+ max-len 2)))))
    (cl-loop for program in programs
             for note in notes
             if (not (or (executable-find program)
                         (file-exists-p program)))
             do (progn
                  (insert program align "is not available")
                  (if (equal note "")
                      (insert "\n")
                    (insert ", it has a note: " note "\n"))))
    (when (eq (point) (point-min))
      (insert "All good\n"))
    (special-mode)))

(provide 'lunary)

;;; lunary.el ends here

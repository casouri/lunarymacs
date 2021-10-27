;;; lunary.el --- Helpers for config files      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Variables, functions and macros used by config files.

;;; Code:

(require 'cowboy)
(require 'luna-load-package)
(require 'luna-key)

;;; Variables

(defvar luna-data-dir (expand-file-name "var" user-emacs-directory))

(defvar luna-package-list nil
  "List of package symbols. Added by ‘load-package’.")

(defvar luna-external-program-list nil
  "List of external programs needed. Added by ‘load-package’.
Each element is a file path or program name (string).")

(defvar luna-external-program-notes nil
  "An alist of ((PROGRAM . DISTRIBUTION) . NOTES).
PROGRAM is a string representing the command line program.
DISTRIBUTION is a symbol representing the package distribution
system where PROGRAM can be retrieved. It can be guix, macports,
debian, etc. NOTES is a string containing the notes. It must not
start or end with a newline.")

(defvar luna-dumped nil
  "non-nil when a dump file is loaded.
(Because dump.el sets this variable to t.)")

(defvar luna-dumped-load-path nil
  "By default dump files doesn’t save ‘load-path’.
We need to manually save and restore it. See manual for more info.")

(defvar luna-dump-location-alist
  '((Emacs
     "/Users/yuan/emacs-head/nextstep/Emacs.app/Contents/MacOS/Emacs"
     "/Users/yuan/emacs-head/nextstep/Emacs.app/Contents/MacOS/Emacs.pdmp"
     "/Users/yuan/emacs-head/nextstep/Emacs.app/Contents/MacOS/libexec/Emacs.pdmp")
    (Emacs-27
     "/Applications/Emacs 27.app/Contents/MacOS/Emacs"
     "/Applications/Emacs 27.app/Contents/MacOS/Emacs.pdmp"
     "/Applications/Emacs 27.app/Contents/MacOS/libexec/Emacs.pdmp"))
  "An alist of (Name . LOCATION-LIST).
LOCATION-LIST is (BINARY-PATH ORIGINAL-DUMP-PATH DUMP-PATH).
BINARY-PATH is the path to the Emacs binary, DUMP-PATH is the
path to the dump file, ORIGINAL-DUMP-PATH is the path to the
original dump file generated by the compile process.")

(defvar luna-font-settings nil
  "A list of (FACE . FONT-NAME).
FONT-NAMEs are keys in ‘luna-font-alist’.")

(defvar luna-cjk-rescale-alist
  '(("Source Han Serif SC" . 1.3)
    ("Source Han Sans SC" . 1.3)
    ("FZFW ZhuZi MinchoS" . 1.3))
  "A list of font names that should be rescaled.")

(defvar luna-font-alist
  `(("SF Mono" . ("SF Mono" "Source Han Serif SC"))
    ("IBM Plex Mono" . ("IBM Plex Mono" "Source Han Serif SC"))
    ("SF Pro Text" . ("SF Pro Text" "Source Han Serif SC"))
    ("IBM Plex Sans" . ("IBM Plex Sans" "Source Han Serif SC"))

    ("方正fW筑紫明朝" . (nil "FZFW ZhuZi MinchoS"))
    ("Source Han Serif" . (nil "Source Han Serif SC"))
    ("Source Han Sans" . (nil "Source Han Sans SC"))

    ("Charter 13" . ("Charter" nil :size 13))
    ("GNU Unifont 15" . ("Unifont" nil :size 15))
    ("SF Mono Light 13" . ("SF Mono" nil :size 13 :weight light))
    ("PragmataPro 13" . ("PragmataPro Mono" nil :size 13))
    ("Iosevka 13" . ("Iosevka" nil :size 14))
    ("JetBrains Mono 12" . ("JetBrains Mono" nil :size 12))
    ("Roboto Mono 12" . ("Roboto Mono" nil :size 12 :weight light)))
  "An alist of all the fonts you can switch between by `luna-load-font'.
Each element is like (FONT-NAME . FONT-DEF). FONT-DEF can be a
fontset name, or a list of font specs that ‘font-spec’ accepts.")

(defvar luna-load-theme-hook nil
  "Hook run after ‘luna-load-theme’.")

;;; Package functions

(defun luna-install-all ()
  "Install all required packages."
  (interactive)
  (display-buffer "*Messages*")
  (dolist (package luna-package-list)
    (unless (cowgirl-installed-p package)
      (cowgirl-install package))))

;;; Config helpers

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

;;; External program

(defun luna-check-external-program (distribution)
  "Check if external programs are available.
Distribution is the current package distribution (symbol), it can be
guix, macports, debian, etc."
  (interactive (list (intern (completing-read "Distribution: "
                                              '(guix macports debian)))))
  (pop-to-buffer (get-buffer-create "*external program*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (cl-loop for program in luna-external-program-list
             if (not (or (executable-find program)
                         (file-exists-p program)))
             do (let ((note (alist-get (cons program distribution)
                                       luna-external-program-notes
                                       nil nil #'equal)))
                  (insert program " is not available")
                  (if note
                      (insert ", it has a note:\n\t"
                              (string-join (split-string note "\n")
                                           "\n\t")
                              "\n")
                    (insert "\n"))))
    (when (eq (point) (point-min))
      (insert "All good\n"))
    (special-mode)))

(defun luna-note-extern (program distribution notes)
  "Set the note for (PROGRAM . DISTRIBUTION) to NOTES.
See ‘luna-external-program-notes’."
  (declare (indent 2))
  (setf (alist-get (cons program distribution)
                   luna-external-program-notes
                   nil nil #'equal)
        notes))

;;; Theme

(defun luna-load-theme (theme)
  "Load THEME or `luna-theme'."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (if (featurep (intern (format "%s-theme" theme)))
      ;; We can save a lot of time by only enabling the theme.
      (enable-theme theme)
    (load-theme theme t))
  (custom-set-variables
   `(luna-theme ,theme nil nil
                "Automatically saved by ‘luna-load-theme’"))
  (custom-save-all)
  (run-hooks 'luna-load-theme-hook))

;;; Font

(defun luna-create-fontset (ascii-spec cjk-spec)
  "Create a fontset NAME with ASCII-SPEC and CJK-SPEC font."
  (let* ((fontset-name
          (concat "fontset-" (downcase (plist-get ascii-spec :family))))
         ;; ASCII font.
         (fontset
          (create-fontset-from-fontset-spec
           (font-xlfd-name
            (apply #'font-spec :registry fontset-name ascii-spec)))))
    ;; CJK font.
    (dolist (charset '(kana han cjk-misc))
      (set-fontset-font fontset charset (apply #'font-spec cjk-spec)))
    fontset))

(defun luna-load-font (face font-name size &rest attrs)
  "Set font for FACE to FONT-NAME.
If FONT-NAME is nil, use the first font in ‘luna-font-alist’.
SIZE is the font size in pt. Add additional face attributes in
ATTRS.

If called interactively, the setting is saved to the custom file
and can be reloaded by ‘luna-load-saved-font’."
  (interactive
   (list (intern (completing-read
                  "Face: " (face-list)))
         (completing-read
          "Font: " (mapcar #'car luna-font-alist))
         (string-to-number (completing-read
                            "Size: " nil nil nil nil nil "13"))))
  (let* ((font-spec (if (null font-name)
                        (cdar luna-font-alist)
                      (alist-get font-name luna-font-alist
                                 nil nil #'equal)))
         (ascii-family (car font-spec))
         (cjk-family (cadr font-spec))
         (rest-spec (append (cddr font-spec) attrs))
         ;; (rest-spec (setf (plist-get rest-spec :size) size))
         (rest-spec (append `(:size ,size) rest-spec))
         (ascii-spec (and ascii-family
                          `(:family ,ascii-family ,@rest-spec)))
         (cjk-spec (and cjk-family `(:family ,cjk-family ,@rest-spec)))
         (fontset (luna-create-fontset ascii-spec cjk-spec)))
    ;; Emacs has a bug that prevents us from setting a fontset for the
    ;; default face, so we have to use ‘set-frame-parameter’. One of
    ;; the reason why we create fontsets on-the-fly (by
    ;; ‘luna-create-fontset’) is because we cannot set default face
    ;; font and frame parameter in the same time, one always overrides
    ;; another. With default face we cannot use fontset, with frame
    ;; parameter we cannot set size dynamically... Oh, and we need to
    ;; add the form to ‘window-setup-hook’ because if this function
    ;; runs before that (in init.el, for example), it doesn’t always
    ;; work properly.
    (if (and (eq face 'default))
        (progn (set-frame-parameter nil 'font fontset)
               (add-hook 'window-setup-hook
                         `(lambda ()
                            "Automatically inserted by ‘luna-load-font’."
                            (set-frame-parameter nil 'font ,fontset))))
      (apply #'set-face-attribute face nil
             :font fontset
             :fontset fontset
             attrs))
    ;; Save the settings.
    (setf (alist-get face luna-font-settings) (list font-name size))
    (when (called-interactively-p 'any)
      (custom-set-variables
	   `(luna-font-settings
	     ',luna-font-settings
	     nil nil "Automatically saved by ‘luna-load-font’"))
      (custom-save-all))))

(defun luna-load-saved-font ()
  "Load font settings saved in ‘luna-font-settings’."
  (dolist (setting luna-font-settings)
	(let ((face (car setting))
		  (font-name (cadr setting))
          (size (caddr setting)))
	  (luna-load-font face font-name size))))

(define-minor-mode luna-scale-cjk-mode
  "Scale CJK font to align CJK font and ASCII font."
  :lighter ""
  :global t
  :group 'luna
  (dolist (setting luna-cjk-rescale-alist)
	(setf (alist-get (car setting)
                     face-font-rescale-alist nil nil #'equal)
		  (if luna-scale-cjk-mode (cdr setting) nil))))

(defun luna-enable-apple-emoji ()
  "Enable Apple emoji display."
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                    nil 'prepend))


(provide 'lunary)

;;; lunary.el ends here

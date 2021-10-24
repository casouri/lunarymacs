;;; lunary-ui.el --- UI helpers      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Provides some system-wide UI helpers.

;;; Code:
;;

(require 'cl-lib)
(require 'cus-edit)

;;; Theme

(defvar luna-theme nil
  "The current theme.")

(defvar luna-load-theme-hook ()
  "Hook run after ‘luna-load-theme’.")

(defun luna-load-theme (&optional theme)
  "Load THEME or `luna-theme'."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (when-let ((theme (or theme luna-theme (car luna-toggle-theme-list))))
    (if (featurep (intern (format "%s-theme" theme)))
        ;; We can save a lot of time by only enabling the theme.
        (enable-theme theme)
      (load-theme theme t))
    (custom-set-variables
	 `(luna-theme ,theme nil nil
				  "Automatically saved by ‘luna-load-theme’"))
	(custom-save-all)))

;;; Utilities

(defun luna-quit-window ()
  "Quit from current window.
If this window used to display another buffer with different
major mode as the current one, switch to that buffer; if not,
delete the window."
  (interactive)
  (cl-loop for buffer-info in (window-prev-buffers)
           for buffer = (car buffer-info)
           ;; If the buffer has different major mode, switch to it.
           if (not (eq (buffer-local-value 'major-mode buffer)
                       major-mode))
           do (switch-to-buffer buffer)
           and return nil
           finally (delete-window)))

(defun luna-window-sibling-list (&optional window)
  "Return all siblings of WINDOW or selected window."
  (let* ((parent (window-parent window))
         (win (window-child parent))
         lst)
    (while win
      (push win lst)
      (setq win (window-next-sibling win)))
    (remove (or window (selected-window)) lst)))

(defun luna-expand-window ()
  "Delete all sibling windows."
  (interactive)
  (mapc #'delete-window (luna-window-sibling-list)))

;;; Font

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
    ;; parameter we cannot set size dynamically...
    (if (and (eq face 'default))
        (set-frame-parameter nil 'font fontset)
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

;;; Color

(defvar luna-color-alist '((doom-blue . "#56B0EC")
                           (doom-purple . "#D86FD9")
                           (spacemacs-yellow . "DarkGoldenrod2")
                           (spacemacs-light-purple . "plum3")
                           (spacemacs-dark-purple . "#5D4E79")
                           (spacemacs-gray . "#3E3D31")
                           (spacemacs-green . "chartreuse3")
                           (lunary-white . "#DEDDE3")
                           (lunary-light-purple . "#61526E")
                           (lunary-dark-yellow . "#F3B700")
                           (lunary-yellow . "#FFD256")
                           (lunary-pink . "#E8739F")
                           (lunary-dark-pink . "#E83077")
                           (powerline-blue . "#289BEC")
                           (powerline-green . "#AAC306")
                           (powerline-yellow . "#DCA809")
                           (mac-red . "#FA5754")
                           (mac-green . "#36CF4C")
                           (mac-yellow . "#FEC041"))

  "Colors.")

;;; Display buffer

(defun luna-display-temp-buffer (buffer alist)
  "Display BUFFER in a small window below.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

But, this function ignores all specifications in the ALIST."
  (ignore alist)
  (let* ((max-height (/ (window-height) 3))
         (window (split-window-below (- max-height))))
    (set-window-buffer window buffer)
    (select-window window)
    (fit-window-to-buffer window max-height 0)))

;;; Configs

(advice-add #'enable-theme :after
            (lambda (&rest _)
              ;; Otherwise title bar’s text’s color doesn’t look right.
              (when (featurep 'ns)
                (set-frame-parameter
                 nil 'ns-appearance
                 (frame-parameter nil 'background-mode)))))

(define-key special-mode-map [remap quit-window] #'luna-quit-window)

(provide 'lunary-ui)

;;; lunary-ui.el ends here

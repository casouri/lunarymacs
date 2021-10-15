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

(defun luna-set-fontset-cjk-font (fontset font-spec)
  "Make FONTSET use FONT-SPEC for CJK characters."
  (dolist (charset '(kana han cjk-misc))
    (set-fontset-font fontset charset font-spec)))

(defun luna-create-fontset (&rest spec)
  "Create a fontset from SPEC.
SPEC is the same as the arguments for ‘font-spec’. A :registry
attribute is mandatory and should look like \"fontset-xxx\",
where xxx is a unique name."
  (create-fontset-from-fontset-spec
   (font-xlfd-name
	(apply #'font-spec spec))))

(defun luna-define-fontset (ascii cjk name)
  "Create a fontset NAME with ASCII and CJK font."
  (let ((fontset name))
	(luna-create-fontset :family ascii :registry fontset)
	(luna-set-fontset-cjk-font fontset (font-spec :family cjk))
	fontset))

(defvar luna-font-settings nil
  "A list of (FACE . FONT-NAME).
FONT-NAMEs are keys in ‘luna-font-alist’.")

(defvar luna-cjk-rescale-alist
  '(("Source Han Serif SC" . 1.3)
    ("Source Han Sans SC" . 1.3)
    ("FZFW ZhuZi MinchoS" . 1.3))
  "A list of font names that should be rescaled.")

(defvar luna-font-alist
  `(("SF Mono" .
     ,(luna-define-fontset
       "SF Mono" "Source Han Serif SC" "fontset-sf mono"))
    ("IBM Plex Mono" .
     ,(luna-define-fontset
       "IBM Plex Mono" "Source Han Serif SC" "fontset-ibm mono"))
    ("SF Pro Text" .
     ,(luna-define-fontset
       "SF Pro Text" "Source Han Serif SC" "fontset-sf text"))
    ("IBM Plex Sans" .
     ,(luna-define-fontset
       "IBM Plex Sans " "Source Han Serif SC" "fontset-ibm sans"))
    
    ("GNU Unifont 15" . (:family "Unifont" :size 15))
    ("SF Mono Light 13" . (:family "SF Mono" :size 13 :weight light))
    ("PragmataPro 13" . (:family "PragmataPro Mono" :size 13))
    ("Iosevka 13" . (:family "Iosevka" :size 14))
    ("JetBrains Mono 12" . (:family "JetBrains Mono" :size 12))
    ("Roboto Mono 12" . (:family "Roboto Mono" :size 12 :weight light))
    ("Charter 13" . (:family "Charter" :size 13)))
  "An alist of all the fonts you can switch between by `luna-load-font'.
Each element is like (FONT-NAME . FONT-DEF). FONT-DEF can be a
fontset name, or a list of font specs that ‘font-spec’ accepts.")

(defvar luna-cjk-font-alist
  ;; We don’t set font size, so the font size changes with default
  ;; font.
  '(("Source Han Serif 13" . (:family "Source Han Serif SC" :size 13))
	("Source Han Sans 13" . (:family "Source Han Sans SC" :size 13))
	("方正fW筑紫明朝 13" . (:family "FZFW ZhuZi MinchoS" :size 13))
    ("GNU Unifont 13" . (:family "Unifont" :size 13)))
  "Similar to `luna-font-alist' but used for CJK scripts.
Use `luna-load-cjk-font' to load them.")

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
  (let* ((font (if (null font-name)
                   (cdar luna-font-alist)
                 (alist-get font-name luna-font-alist
                            nil nil #'equal)))
         (font-def (if (stringp font) font
                     (apply #'font-spec font)))
         (font-name (or font-name (caar luna-font-alist))))
    ;; Emacs has a bug that prevents us from setting a fontset for the
    ;; default face. Also, this needs to go before the
    ;; ‘set-face-attribute’ form, otherwise attributes like :height
    ;; are nullified.
    (if (eq face 'default)
        (set-frame-parameter nil 'font font-def))
    (if (stringp font-def)
        (apply #'set-face-attribute face nil
               :font font-def
               :fontset font-def
               :height (* 10 size)
               attrs)
      (apply #'set-face-attribute face nil
             :font font-def
             :height (* 10 size)
             attrs))
    (setf (alist-get face luna-font-settings) (list font-name size))
    (when (called-interactively-p 'any)
      (custom-set-variables
	   `(luna-font-settings
	     ',luna-font-settings
	     nil nil "Automatically saved by ‘luna-load-font’"))
      (custom-save-all))))

;; (defun luna-load-cjk-font (font-name fontset)
;;   "Prompt for a font and set it for FONTSET.
;; If FONT-NAME is nil, use the first font in ‘luna-cjk-font-alist’."
;;   (interactive
;;    (list (completing-read
;;           "Choose a font: "
;;           (mapcar #'car luna-cjk-font-alist))
;; 		 (completing-read
;; 		  "Choose a fontset: " luna-fontset-list)))
;;   (let* ((font-name (or font-name (caar luna-cjk-font-alist)))
;; 		 (font-spec (apply #'font-spec
;;                            (if font-name
;;                                (alist-get font-name luna-cjk-font-alist
;;                                           nil nil #'equal)
;; 							 ;; If both ‘font-name’ and
;; 							 ;; ‘luna-cjk-font’ are nil, default to
;; 							 ;; the first font in
;; 							 ;; ‘luna-cjk-font-alist’.
;;                              (cdar luna-cjk-font-alist)))))
;;     (luna-set-fontset-cjk-font fontset font-spec)
;; 	(setf (cadr (alist-get fontset luna-font-settings nil nil #'equal))
;; 		  font-name)
;; 	(custom-set-variables
;; 	 `(luna-font-settings
;; 	   ',luna-font-settings
;; 	   nil nil "Automatically saved by ‘luna-load-font’"))
;; 	(custom-save-all)))

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

;;; Serif/sans serif

(define-minor-mode luna-sans-mode
  "Use sans-serif font for variable-pitch face."
  :lighter ""
  :global t
  :group 'convenience
  (if luna-sans-mode
      (set-face-attribute 'variable-pitch nil :family "Helvetica")
    (set-face-attribute 'variable-pitch nil :family "Charter")))

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

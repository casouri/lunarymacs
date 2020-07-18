;;; -*- lexical-binding: t -*-

(require 'luna-local)

;;; Theme

(defvar luna-toggle-theme-list ()
  "Themes that you can toggle bwtween by `luna-switch-theme'.")

(defvar luna-theme nil
  "The theme applied on startup (by ‘luna-load-theme’ in init.el).")

(defvar luna-load-theme-hook nil
  "Hook run after Emacs loads a theme.")

(advice-add #'enable-theme :after
            (lambda (&rest _)
              (run-hooks 'luna-load-theme-hook)
              ;; Otherwise title bar’s text’s color doesn’t look right
              (when (featurep 'ns)
                (set-frame-parameter
                 nil 'ns-appearance
                 (frame-parameter nil 'background-mode)))))

(defun luna-load-theme (&optional theme)
  "Disable `luna-currnt-theme' and load THEME.
For NO-CONFIRM and NO-ENABLE see ‘load-theme’."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))))
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (let ((theme (or theme luna-theme (car luna-toggle-theme-list))))
    ;; We can save a lot of time by enabling the theme instead of
    ;; loading it
    (if (featurep (intern-soft (format "%s-theme" theme)))
        (enable-theme theme)
      (load-theme theme t))
    (luna-local-set 'luna-theme theme)))

(defun luna-quit-window (arg)
  "Quit current window and bury it's buffer.
Unlike `quit-window', this function deletes the window no matter what.
If run with prefix argument (ARG), kill buffer."
  (interactive "p")
  (if (equal major-mode 'dired-mode)
      (while (equal major-mode 'dired-mode)
        (kill-buffer))
    (if (eq arg 4) ; with C-u
        (kill-buffer)
      (bury-buffer)))
  (ignore-errors (delete-window)))

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

(defun luna-switch-theme ()
  "Switch between themes in `luna-toggle-theme-list'"
  (interactive)
  ;; move the fist element to last
  (luna-load-theme (car luna-toggle-theme-list))
  (setq luna-toggle-theme-list
        (append (cdr luna-toggle-theme-list)
                (list (car luna-toggle-theme-list)))))

;;; Font

(defmacro luna-set-font (&rest config-list)
  "Set font. Accepts `font-spec' arguments.

e.g. :family :weight :size etc."
  `(set-frame-font (font-spec ,@config-list) nil t))

(defvar luna-font nil
  "Font name (string) used by me. Cached by ‘luna-local’.")

(defvar luna-cjk-font nil
  "CJK font name (string) used by me. Cached by ‘luna-local’.")

(defvar luna-cjk-font-scale 1.1
  "The scale for CJK font. Used in ‘luna-scale-cjk’.")

(defvar luna-font-alist
  '(("SF Mono 13" . (:family "SF Mono" :size 13))
    ("SF Mono 14" . (:family "SF Mono" :size 14))
    ("GNU Unifont 15" . (:family "Unifont" :size 15))
    ("SF Mono Light 13" . (:family "SF Mono" :size 13 :weight light))
    ("PragmataPro 13" . (:family "Essential PragmataPro" :size 13))
    ("PragmataPro 14" . (:family "Essential PragmataPro" :size 14))
    ("Iosevka 13" . (:family "Iosevka" :size 14)))
  "An alist of all the fonts you can switch between by `luna-load-font'.
Key is a symbol as the name, value is a plist specifying the font spec.
More info about spec in `font-spec'.")

(defvar luna-cjk-font-alist
  ;; We don’t set font size, so the font size changes with default
  ;; font.
  '(("Srouce Han Serif" . (:family "Source Han Serif SC"))
    ("GNU Unifont" . (:family "Unifont")))
  "Similar to `luna-font-alist' but used for CJK scripts.
Use `luna-load-cjk-font' to load them.")

(defun luna-load-font (&optional font-name)
  "Prompt for a font and set it.
Fonts are specified in `luna-font-alist'. If FONT-NAME non-nil,
use it instead. The font is saved to local file (see
luna-local.el)."
  (interactive
   (list (completing-read
          "Choose a font: "
          (mapcar #'car luna-font-alist))))
  (let* ((font-name (or font-name luna-font))
         (font (apply #'font-spec
                      (if font-name
                          (alist-get font-name luna-font-alist
                                     nil nil #'equal)
                        ;; If font-name is nil (loading from local
                        ;; file and don’t have it saved), use first
                        ;; font spec.
                        (cdar luna-font-alist)))))
    (set-frame-font font nil t)
    ;; seems that there isn't a good way to get font-object directly
    (add-to-list 'default-frame-alist
                 `(font . ,(face-attribute 'default :font)))
    (luna-local-set 'luna-font font-name)))

(defun luna-load-cjk-font (&optional font-name)
  "Prompt for a font and set it.
Fonts are specified in `luna-cjk-font-alist'. If FONT-NAME non-nil,
use it instead. The font is saved to local file (see
luna-local.el)."
  (interactive
   (list (completing-read
          "Choose a font: "
          (mapcar #'car luna-cjk-font-alist))))
  (let* ((font-name (or font-name luna-cjk-font))
         (font-spec (apply #'font-spec
                           (if font-name
                               ;; If font-name is nil (loading from
                               ;; local file and don’t have it saved),
                               ;; use first font spec.
                               (alist-get font-name luna-cjk-font-alist
                                          nil nil #'equal)
                             (cdar luna-cjk-font-alist)))))
    (dolist (charset '(kana han cjk-misc))
      (set-fontset-font t charset font-spec))
    (luna-local-set 'luna-cjk-font font-name)))

(define-minor-mode luna-scale-cjk-mode
  "Scale CJK font to align CJK font and ASCII font."
  :lighter ""
  (if luna-scale-cjk-mode
      (progn
        (make-local-variable 'face-font-rescale-alist)
        (when luna-font
          (setf (alist-get (plist-get (alist-get (intern luna-cjk-font)
                                                 luna-cjk-font-alist)
                                      :family)
                           face-font-rescale-alist)
                luna-cjk-font-scale)))
    (kill-local-variable 'face-font-rescale-alist)))

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

(provide 'lunary-ui)

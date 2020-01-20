;;; -*- lexical-binding: t -*-

;;; Theme

(defvar luna-load-theme-hook ()
  "Hook ran after `load-theme'.")

(defvar luna-current-theme nil
  "The last loaded theme (symbol) in string.")

(defvar luna-toggle-theme-list ()
  "Themes that you can toggle bwtween by `luna-switch-theme'.")

(defcustom luna-theme nil
  "The theme used on startup.
This way luanrymacs remembers the theme.
You need to load `luna-theme' somewhere (after loading custom.el)."
  :type 'symbol
  :group 'convenience)

(defcustom luna-bg nil
  "Save bg to avoid flicker on startup."
  :type 'string
  :group 'convenience)

(defun luna-set-current-theme (theme &rest _)
  "Adveiced before `load-theme', set `luna-current-theme' to THEME."
  (setq luna-current-theme theme))

(defun luna-run-load-theme-hook (&rest _)
  "Run `luna-load-theme-hook'."
  (condition-case err
      (run-hook-with-args 'luna-load-theme-hook)
    ((error (message (error-message-string err))))))

(advice-add #'load-theme :after #'luna-run-load-theme-hook)

(advice-add #'load-theme :before #'luna-set-current-theme)

(defun luna-load-theme (&optional theme)
  "Disable `luna-currnt-theme' and load THEME.
For NO-CONFIRM and NO-ENABLE see ‘load-theme’."
  (disable-theme luna-current-theme)
  (let ((theme (or theme luna-theme (car luna-toggle-theme-list))))
    (if (featurep (intern-soft (format "%s-theme" theme)))
        (enable-theme theme)
      (load-theme theme t))
    (setq luna-current-theme theme)
    (customize-set-variable 'luna-theme theme)))

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
  (let ((index (or (cl-position luna-current-theme luna-toggle-theme-list)
                   (progn (message "`luna-current-theme' is not in `luna-toggle-theme-list', default to the first one") 0)))
        (len (length luna-toggle-theme-list)))
    (luna-load-theme (nth (% (1+ index) len) luna-toggle-theme-list))))

;;; Font

(defmacro luna-set-font (&rest config-list)
  "Set font. Accepts `font-spec' arguments.

e.g. :family :weight :size etc."
  `(set-frame-font (font-spec ,@config-list) nil t))

(defcustom luna-font nil
  "Like `luna-theme', used to cache configuration across sessions."
  :type 'string
  :group 'convenience)

(defcustom luna-cjk-font nil
  "Like `luna-font'."
  :type 'string
  :group 'convenience)

(defvar luna-cjk-font-scale 1.1
  "The scale for CJK font. Used in ‘luna-scale-cjk’.")

(defvar luna-font-alist
  '((sf-mono-13 . (:family "SF Mono" :size 13)))
  "An alist of all the fonts you can switch between by `luna-load-font'.
Key is a symbol as the name, value is a plist specifying the font spec.
More info about spec in `font-spec'.")

(defvar luna-cjk-font-alist
  '((soure-han-serif-13 . (:family "Source Han Serif SC"
                                   :size 13)))
  "Similar to `luna-font-alist' but used for CJK scripts.
Use `luna-load-cjk-font' to load them.")

(defun luna-load-font (&optional font-name)
  "Prompt for a font and set it.
Fonts are specified in `luna-font-alist'.

Changes are saved to custom.el in a idle timer."
  (interactive (list
                (completing-read "Choose a font: "
                                 (mapcar (lambda (cons) (symbol-name (car cons)))
                                         luna-font-alist))))

  (let* ((arg font-name)
         (font-name (or font-name luna-font))
         (font (apply #'font-spec
                      (if font-name (alist-get (intern font-name)
                                               luna-font-alist)
                        (cdar luna-font-alist)))))
    (set-frame-font font nil t)
    ;; seems that there isn't a good way to get font-object directly
    (add-to-list 'default-frame-alist `(font . ,(face-attribute 'default :font)))
    (when (or arg (not (custom-variable-p 'luna-font)))
      (customize-set-variable 'luna-font font-name))))

(defun luna-load-cjk-font (&optional font-name)
  "Prompt for a font and set it.
Fonts are specified in `luna-font-alist'.

Changes are saved to custom.el in a idle timer."
  (interactive (list
                (completing-read "Choose a font: "
                                 (mapcar (lambda (cons) (symbol-name (car cons)))
                                         luna-cjk-font-alist))))
  (let* ((arg font-name)
         (font-name (or font-name luna-cjk-font))
         (font-spec (apply #'font-spec
                           (if font-name
                               (alist-get (intern font-name)
                                          luna-cjk-font-alist)
                             (cdar luna-cjk-font-alist)))))
    (dolist (charset '(kana han cjk-misc))
      (set-fontset-font t charset font-spec))
    (when (or arg (not (custom-variable-p 'luna-cjk-font)))
      (customize-set-variable 'luna-cjk-font font-name))))

(defun luna-scale-cjk ()
  "Rescale CJK font to align CJK font and ASCII font."
  (make-local-variable 'face-font-rescale-alist)
  (when luna-font
    (setf (alist-get (plist-get (alist-get (intern luna-cjk-font)
                                           luna-cjk-font-alist)
                                :family)
                     face-font-rescale-alist)
          luna-cjk-font-scale)))

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

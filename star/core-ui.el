;;; -*- lexical-binding: t -*-


;;; Homepage

(setq inhibit-startup-screen t)

(defvar luna-homepage-buffer "HOME"
  "The buffer name of the homepage")

;;; Theme

(defvar luna-load-theme-hook ()
  "Hook ran after `load-theme'")

(defvar luna-current-theme nil
  "The last loaded theme (symbol) in string.")

(defvar luna-toggle-theme-list ()
  "Themes that you can toggle bwtween by `luna-switch-theme'")

(defvar luna-theme-book '(spacemacs-dark spacemacs-light)
  "A list of themes that you can load with `luna-load-theme'.")

(defcustom luna-theme nil
  "The theme used on startup.
This way luanrymacs remembers the theme.
You need to load `luna-theme' somewhere (after loading custom.el)."
  :type 'symbol
  :group 'convenience)

(defcustom luna-font nil
  "Like `luna-theme', used to cache configuration across sessions."
  :type 'string
  :group 'convenience)

(defcustom luna-cjk-font nil
  "Like `luna-font'."
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

(defun luna-load-theme (&optional theme no-confirm no-enable)
  "Disable `luna-currnt-theme' and oad THEME.
Set `luna-theme' to THEME."
  (disable-theme luna-current-theme)
  (load-theme (or theme luna-theme (car luna-toggle-theme-list)) no-confirm no-enable)
  (when theme
    (customize-set-variable 'luna-theme theme)
    (customize-save-customized)))

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
    (when arg
      (customize-set-variable 'luna-font font-name)
      (message "Change will be saved in idle time (5 seconds)")
      (run-with-idle-timer 5 nil #'customize-save-customized))))

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
    (when arg
      (customize-set-variable 'luna-cjk-font font-name)
      (message "Change will be saved in idle time (5 seconds)")
      (run-with-idle-timer 5 nil #'customize-save-customized))))

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

(defun luna-switch-theme ()
  "Switch between themes in `luna-toggle-theme-list'"
  (interactive)
  ;; move the fist element to last
  (let ((index (or (cl-position luna-current-theme luna-toggle-theme-list)
                   (progn (message "`luna-current-theme' is not in `luna-toggle-theme-list', default to the first one") 0)))
        (len (length luna-toggle-theme-list)))
    (luna-load-theme (nth (% (1+ index) len) luna-toggle-theme-list) t)))

(defmacro luna-set-font (&rest config-list)
  "Set font. Accepts `font-spec' arguments.

e.g. :family :weight :size etc."
  `(set-frame-font (font-spec ,@config-list) nil t))


;;; Font

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


;;; Rmove GUI elements

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(unless (eq window-system 'ns)
  (menu-bar-mode -1))

;;; Color

(defvar luna-color-book
  '(doom-blue spacemacs-yellow lunary-white
              lunary-dark-pink lunary-pink
              lunary-dark-yellow lunary-yellow
              spacemacs-gray spacemacs-green
              spacemacs-light-purple
              spacemacs-dark-purple
              powerline-blue poweline-green
              poweline-yellow mac-red mac-green
              mac-yellow
              )
  "All prededined colors.")

(defvar doom-blue "#56B0EC"
  "Blue color of doom-emacs.")

(defvar doom-purple "#D86FD9"
  "Blue color of doom-emacs.")

(defvar spacemacs-yellow "DarkGoldenrod2"
  "Yellow color of spacemacs.")

(defvar spacemacs-light-purple "plum3"
  "A light purple used in spacemacs.")

(defvar spacemacs-dark-purple "#5D4E79"
  "Spacemacs purple.")

(defvar spacemacs-gray "#3E3D31"
  "A dark gray.")

(defvar spacemacs-green "chartreuse3"
  "A bright green.")

(defvar lunary-white "#DEDDE3"
  "White color of luna.")

(defvar lunary-light-purple "#61526E"
  "Light purple color of luna.

Can be uesed for hightlight region.")

(defvar lunary-dark-yellow "#F3B700"
  "Dark yellow color of luna.")

(defvar lunary-yellow "#FFD256"
  "Yellow color of luna.")

(defvar lunary-pink "#E8739F"
  "Pink(?) color of luna.")

(defvar lunary-dark-pink "#E83077"
  "Dark pink(?) color of luna.")

(defvar powerline-blue "#289BEC"
  "Bright blue.")

(defvar powerline-green "#AAC306"
  "Bright green.")

(defvar powerline-yellow "#DCA809"
  "Brigh yellow/orange.")

(defvar mac-red "#FA5754"
  "Red color on mac titlebar.")

(defvar mac-green "#36CF4C"
  "Green color on mac titlebar.")

(defvar mac-yellow "#FEC041"
  "Yellow color on mac titlebar.")

(provide 'core-ui)

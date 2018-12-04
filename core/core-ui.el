;; -*- lexical-binding: t -*-

;;
;;; Homepage
;;

(setq inhibit-startup-screen t)
(setq initial-buffer-choice (lambda () (get-buffer-create moon-homepage-buffer)))

(defvar moon-homepage-buffer "HOME"
  "The buffer name of the homepage")

;;
;;; Theme
;;

(defvar moon-load-theme-hook ()
  "Hook ran after `load-theme'")

(defvar moon-current-theme nil
  "The last loaded theme (symbol) in string.")

(defvar moon-toggle-theme-list ()
  "Themes that you can toggle bwtween by `moon/switch-theme'")

(defvar moon-theme-book '(spacemacs-dark spacemacs-light)
  "A list of themes that you can load with `moon/load-theme'.")

(defun moon-set-current-theme (theme &rest _)
  "Adveiced before `load-theme', set `moon-current-theme' to THEME."
  (setq moon-current-theme theme))

(defun moon-run-load-theme-hook (&rest _)
  "Run `moon-load-theme-hook'."
  (run-hook-with-args 'moon-load-theme-hook))

(advice-add #'load-theme :after #'moon-run-load-theme-hook)

(advice-add #'load-theme :before #'moon-set-current-theme)

(defun moon-load-theme (theme &optional no-confirm no-enable)
  "Disable `moon-currnt-theme' and oad THEME."
  (disable-theme moon-current-theme)
  (load-theme theme no-confirm no-enable))

;;
;;; Font
;;

(defvar moon-magic-font-book
  '(
    ("Source Code Pro" . (moon-set-font| :family "Source Code Pro"
                                         :weight 'light
                                         :size 14))
    ("SF Mono" . (moon-set-font| :family "SF Mono" :weight 'light :size 14))
    ("Source Code Pro for Powerline" . (moon-set-font| :family "Source Code Pro for Powerline" :weight 'light :size 14))
    )

  "All the fonts you can switch between by `moon/load-font'
It is an alist with the form
((name . (moon-set-font| configuration))
 (name . (moon-set-font| :family \"family\" :weight 'weight)))

I have to do it this way because apply flattens a list ignoring quotes.
So '(:family \"SF Mono\" :weight 'light) will become
(:family \"SF Mono\" :weight quote light).
And such list cannot be passed into a `font-spec'.")


;;
;;; Rmove GUI elements
;;

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(unless (eq window-system 'ns)
  (menu-bar-mode -1))

;;
;;; Color
;;

(defvar moon-color-book
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
  "White color of moon.")

(defvar lunary-light-purple "#61526E"
  "Light purple color of moon.

Can be uesed for hightlight region.")

(defvar lunary-dark-yellow "#F3B700"
  "Dark yellow color of moon.")

(defvar lunary-yellow "#FFD256"
  "Yellow color of moon.")

(defvar lunary-pink "#E8739F"
  "Pink(?) color of moon.")

(defvar lunary-dark-pink "#E83077"
  "Dark pink(?) color of moon.")

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

;;
;;; Function
;;

(defun change-by-theme (config-list)
  "Evaluate diffrent form based on what is the current theme.

CONFIG-LIST is a list of (theme . form).

For example:
  (change-by-theme 
    '((spacemacs-dark . (progn 
                         (setq hl-paren-colors '(\"green\")) 
                         (hl-paren-color-update)))
      (spacemacs-light . (progn 
                         (setq hl-paren-colors '(\"red\")) 
                         (hl-paren-color-update)))))"
  (add-hook
    'moon-load-theme-hook
    (lambda ()
      (dolist (config config-list)
        (let ((theme (symbol-name (car config)))
              (form (cdr config)))
          (when (equal moon-current-theme theme)
            (eval form)))))))
(defun moon-quit-window ()
  "Quit current window and bury it's buffer.
Unlike `quit-window', this function deletes the window no matter what."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (while (equal major-mode 'dired-mode)
        (kill-buffer))
    (bury-buffer))
  (ignore-errors (delete-window)))


(provide 'core-ui)


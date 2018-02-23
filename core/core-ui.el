;;; -*- lexical-binding: t -*-

;;
;; Homepage
;;

;;
;; Var

(defvar moon-homepage-buffer "HOME"
  "The buffer name of the homepage")

;;
;; Config

(setq inhibit-startup-screen t)
(setq initial-buffer-choice (lambda () (get-buffer-create moon-homepage-buffer)))

;;
;; Theme
;;

;;
;; Var

(defvar moon-load-theme-hook ()
  "Hook ran after `load-theme'")

(defvar moon-current-theme ""
  "The last loaded theme name in string.")

(defvar moon-toggle-theme-list
  '(spacemacs-dark spacemacs-light)
  "Themes that you can toggle bwtween by `moon/switch-theme'")

;;
;; Func
(defun moon-set-current-theme (&rest form)
  "Adveiced before `load-theme', set `moon-current-theme'."
  (setq moon-current-theme (symbol-name (car form))))

;;
;; Config

(defadvice load-theme (after run-load-theme-hook activate)
  (run-hook-with-args 'moon-load-theme-hook))

(advice-add #'load-theme :before #'moon-set-current-theme)

;;
;; Font
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
;; Color
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
              ))

(defvar doom-blue "#56B0EC"
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

(provide 'core-ui)

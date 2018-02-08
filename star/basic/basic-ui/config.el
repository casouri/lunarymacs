;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar moon-banner "███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
████╗ ████║██╔═══██╗██╔═══██╗████╗  ██║    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
██╔████╔██║██║   ██║██║   ██║██╔██╗ ██║    █████╗  ██╔████╔██║███████║██║     ███████╗
██║╚██╔╝██║██║   ██║██║   ██║██║╚██╗██║    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
██║ ╚═╝ ██║╚██████╔╝╚██████╔╝██║ ╚████║    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
                                                                                      ")

(defvar moon-short-banner "███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗
████╗ ████║██╔═══██╗██╔═══██╗████╗  ██║
██╔████╔██║██║   ██║██║   ██║██╔██╗ ██║
██║╚██╔╝██║██║   ██║██║   ██║██║╚██╗██║
██║ ╚═╝ ██║╚██████╔╝╚██████╔╝██║ ╚████║
╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝
                                       ")

;;
;; Color
;;

(defvar moon-color-book
  '(doom-blue spacemacs-yellow lunary-white
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

(defvar lunary-white "#FFFFFF"
  "White color of moon. （壮哉我大FFF团）")

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
;; Cursor Color
;;


(defvar moon-normal-state-cursor-color lunary-white
  "Cursor color in normal state.")

(defvar moon-insert-state-cursor-color spacemacs-yellow
  "Cursor color in insert state")

(defvar moon-maximize-on-startup nil
  "Whether to maximize screen on startup.")

;;
;; Func
;;


(defun moon/draw-homepage ()
  (interactive)
  (let (
	(banner-list (if (>= (window-width) 86)
                         (split-string moon-banner "\n")
                       (split-string moon-short-banner "\n")
                       )
                     )
	)
    (let (
	  (space-to-insert
	   (make-string
	    (/ (- (window-width) (length (car banner-list))) 2) ?\s
	    )
	   )
	  )
      (insert (make-string 10 ?\n))
      (dolist (line banner-list)
	(insert space-to-insert)
	(insert line)
	(insert "\n")
	)
      )
    )
  )

;;
;; Config
;;

;; Homepage
(add-hook 'moon-post-init-hook #'moon/draw-homepage t)
(add-hook 'moon-post-init-hook #'moon-display-benchmark)

(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq-default
 use-dialog-box nil
 visible-cursor nil
 use-dialog-box nil
 ring-bell-function #'ignore
 visible-bell nil
 frame-title-format '("%f") ;; current file name
 display-line-numbers-width 3
 )

;; font
(setq moon-font (font-spec :family "Source Code Pro" :weight 'light :size 14))
(set-frame-font moon-font nil t)
(set-face-attribute 'fixed-pitch nil :font moon-font)

;; cursor
(add-hook 'evil-normal-state-entry-hook
          (lambda ()
            (set-face-attribute 'cursor nil :background moon-normal-state-cursor-color)
            ))

(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (set-face-attribute 'cursor nil :background moon-insert-state-cursor-color)
            ))


;; max screen on startup (or not)
(when moon-maximize-on-startup
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )



;;
;; Package
;;

(use-package| spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))


(use-package| delight
  :defer t
  :config
  (delight 'eldoc-mode nil "eldoc"))

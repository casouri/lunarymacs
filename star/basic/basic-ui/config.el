;;
;; Var
;;

(defvar moon-banner "
███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
████╗ ████║██╔═══██╗██╔═══██╗████╗  ██║    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
██╔████╔██║██║   ██║██║   ██║██╔██╗ ██║    █████╗  ██╔████╔██║███████║██║     ███████╗
██║╚██╔╝██║██║   ██║██║   ██║██║╚██╗██║    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
██║ ╚═╝ ██║╚██████╔╝╚██████╔╝██║ ╚████║    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
                                                                                      ")

(defvar moon-short-banner "
███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗
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

(defvar lunary-white "#DEDDE3"
  "White color of moon.")

(defvar lunary-dark-yellow "#F3B700"
  "Dark yellow color of moon.")

(defvar lunary-yellow "#FFD256"
  "Yellow color of moon.")

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
  "Cursor color in insert state.")

(defvar moon-maximize-on-startup nil
  "Whether to maximize screen on startup.")

;;
;; Config
;;

;; popup window to right!
(setq split-height-threshold nil
      split-width-threshold 80)

;;
;; Func
;;


(defun moon/draw-homepage ()
  "Draw MOON EMACS or MOON on the middle of current buffer.

MOON is used when buffer's width is less than 86."
  (interactive)
  (let (
	(banner-list (if (>= (window-width) 86)
                         (split-string moon-banner "\n")
                       (split-string moon-short-banner "\n")))
	)
    (let (
	  (space-to-insert
	   (make-string
	    (/ (- (window-width)
                  ;; first line could be empty, use second line for length
                  (length (nth 1 banner-list))) 2) ?\s))
	  )
      (insert (make-string 10 ?\n))
      (dolist (line banner-list)
	(insert space-to-insert)
	(insert line)
	(insert "\n"))
      ))
  )

;;
;; Config
;;

;; Homepage
(add-hook 'moon-init-hook #'moon/draw-homepage t)
(add-hook 'moon-post-init-hook #'moon-display-benchmark t)

(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)


;;
;; font
;;



;; (moon-set-font| :family "Source Code Pro" :weight 'light :size 14)
(moon-set-font| :family "SF Mono" :weight 'light :size 14)

;;
;; Cursor
;;

(change-cursor-on-hook| evil-normal-state-entry-hook moon-normal-state-cursor-color)
(change-cursor-on-hook| evil-insert-state-entry-hook moon-insert-state-cursor-color)


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

;; (use-package| shackle
;;   :defer t
;;   :init
;;   (setq shackle-default-rule '(:align 'right :select nil :popup t))
;;   (make-thread (lambda () (require 'shackle)) "shackle"))

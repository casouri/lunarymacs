;;
;; Init
;;

(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;
;; Func
;;

(defun moon-draw-homepage ()
  (insert "
███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
████╗ ████║██╔═══██╗██╔═══██╗████╗  ██║    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
██╔████╔██║██║   ██║██║   ██║██╔██╗ ██║    █████╗  ██╔████╔██║███████║██║     ███████╗
██║╚██╔╝██║██║   ██║██║   ██║██║╚██╗██║    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
██║ ╚═╝ ██║╚██████╔╝╚██████╔╝██║ ╚████║    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
                                                                                      "))
;;
;; Config
;;

(setq-default
 use-dialog-box nil
 visible-cursor nil
 use-dialog-box nil
 ring-bell-function #'ignore
 visible-bell nil
 frame-title-format '("%f") ;; current file name
 )

;; max screen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(add-hook 'moon-post-init-hook #'moon-draw-homepage)

;;
;; Package
;;

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(setq moon-font (font-spec :family "Source Code Pro" :weight 'light :size 14))
(set-frame-font moon-font nil t)
(set-face-attribute 'fixed-pitch nil :font moon-font)

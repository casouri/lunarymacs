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

(setq-default
 use-dialog-box nil
 visible-cursor nil
 use-dialog-box nil
 ring-bell-function #'ignore
 visible-bell nil
 frame-title-format '("%f") ;; current file name
 )

(setq moon-font (font-spec :family "Source Code Pro" :weight 'light :size 14))
(set-frame-font moon-font nil t)
(set-face-attribute 'fixed-pitch nil :font moon-font)

;; max screen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'moon-post-init-hook #'moon/draw-homepage t)

;;
;; Package
;;

(use-package| spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))


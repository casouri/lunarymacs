;;(package-initialize t)

(load (concat (expand-file-name user-emacs-directory) "core/core"))

(moon| :basic
       homepage
       key
       evil
       ui
       other
       edit
       project
       :completion
       ivy
       company
       snippet
       :os
       mac
       :utility
       dir
       git
       org
       :checker
       syntax
       spell
       :lang
       lsp
       python
       elisp
       )


;;
;; Settings evaluate befor loading any stars i.e. user-init
;;


;; max
(toggle-frame-maximized)

;; default was ", "
(setq eyebrowse-mode-line-separator " ")

;;
;;                                                                                      ~~dark~~~                               ~~light~~
(custom-set-variables '(spacemacs-theme-custom-colors ;                              GUI       TER                           GUI       TER
                        '((bg1        . (if (eq variant 'dark) (if (true-color-p) "#222226" "#262626") (if (true-color-p) "#fbf8ef" "#ffffff")))
                          (bg2        . (if (eq variant 'dark) (if (true-color-p) "#17181B" "#1c1c1c") (if (true-color-p) "#efeae9" "#e4e4e4")))
                          (comment-bg . (if (eq variant 'dark) (if (true-color-p) "#23282A" "#262626") (if (true-color-p) "#ecf3ec" "#ffffff")))
                          (highlight  . (if (eq variant 'dark) (if (true-color-p) "#61526E" "#444444") (if (true-color-p) "#d3d3e7" "#d7d7ff")))
                          (act2       . (if (eq variant 'dark) (if (true-color-p) "#643896" "#444444") (if (true-color-p) "#d3d3e7" "#d7d7ff")))
                          (border     . (if (eq variant 'dark) (if (true-color-p) "#643896" "#444444") (if (true-color-p) "#d3d3e7" "#d7d7ff")))
                          )))

;; natural title bar
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))

;; Python interpreter
(setq python-shell-interpreter "/usr/local/bin/python3")

 ;; shell
 (setq explicit-shell-file-name "/bin/zsh")
 (setq explicit-zsh-args '("--login"))
 (setenv "SHELL" "zsh")

;; split screen vertically in ediff
(setq ediff-split-window-function #'split-window-horizontally)

;; default was ", "
(setq eyebrowse-mode-line-separator " ")

;; Don't blink
(blink-cursor-mode -1)

;; relative line number
(setq moon-enable-nlinum-relative t)

;; company-yasnippet
(setq moon-enable-company-yas t)

;;
;; Settings to overwrite configs in stars i.e. user-config
;;

(customize| 

 ;; theme toggle
 ;; (setq moon-toggle-theme-list '(spacemacs-dark spacemacs-light))

 ;; font
 ;; (moon-set-font| :family "SF Mono" :weight 'light :size 14)

 ;; scroll margin
 (setq scroll-margin 8)

 ;; hide bad title when using natural title bar in Emacs 26
 (setq  frame-title-format '(""))

 (nyan-mode)
 (nyan-start-animation)

 ;; it slows down starup, disable for now
 ;; company-childframe
 ;; (load (concat moon-emacs-d-dir "star/completion/company/posframe/posframe"))
 ;; (load (concat moon-emacs-d-dir "star/completion/company/company-childframe/company-childframe"))
 ;; (company-childframe-mode 1)

 ;;
 ;;customize ends here
 )

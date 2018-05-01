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
       ;; mac
       :utility
       ;; dir
       git
       org
       ;; imagemagick
       :checker
       syntax
       ;; spell
       :lang
       ;; lsp
       ;; python
       ;; elisp
       ;; rust
       ;; javascript
       ;; web
       ;; lua
       )


;;
;; Settings evaluate befor loading any stars i.e. user-init
;;


;; max
(toggle-frame-maximized)

;; default was ", "
(setq eyebrowse-mode-line-separator " ")

;;
;; Settings to overwrite configs in stars i.e. user-config
;;

(customize| 

 ;; theme toggle
 ;; (setq moon-toggle-theme-list '(spacemacs-dark spacemacs-light))

 ;; font
 ;; (moon-set-font| :family "SF Mono" :weight 'light :size 14)

 ;;
 ;;customize ends here
 )


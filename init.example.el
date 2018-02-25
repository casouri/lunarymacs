;;(package-initialize t)

;;
;; hacks
;;

;; I have these in basic-ui star,
;; but copy them here makes startup looks a bit nicer
(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(blink-cursor-mode -1)
(toggle-frame-maximized)

;;
;; Not hacks
;;

(load (concat (expand-file-name user-emacs-directory) "core/core"))

(moon| :basic
       basic-ui
       key
       evil
       ui
       other
       edit
       :completion
       ivy
       company
       snippet
       :os
       ;; mac
       :utility
       ;; dir
       ;; git
       ;; org
       :checker
       syntax
       spell
       :lang
       ;; lsp
       ;; python
       elisp
       )



(customize| 
 ;; I have my hack
 ;; (setq moon-maximize-on-startup t)

 ;; theme toggle
 (setq moon-toggle-theme-list '(spacemacs-dark spacemacs-light))
 )

;;
;; Below are settings that evaluate befor loading any stars
;;




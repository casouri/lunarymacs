;;(package-initialize t)

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
 ;; load after stars

 ;; theme toggle
 ;; (setq moon-toggle-theme-list '(spacemacs-dark spacemacs-light))
 ;; (moon-set-font| :family "SF Mono" :weight 'light :size 14)
 )

;;
;; Settings evaluate befor loading any stars
;;


;; max
 (toggle-frame-maximized)

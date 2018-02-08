;;; -*- lexical-binding: t -*-
;;(package-initialize t)

;;
;; hacks
;;

;; I have these in basic-ui star,
;; but copy them here makes starup looks a bit nicer
(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
;; maximize on startup
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
       :autocomplete
       ivy
       company
       :casouri
       casouri
       :os
       mac
       :utility
       dir
       :checker
       syntax
       spell
       :lang
       lsp
       python
       )

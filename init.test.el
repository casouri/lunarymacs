;;(package-initialize t)

(defvar moon-setup nil)

(if moon-setup
    (load (concat (expand-file-name user-emacs-directory) "core/core-setup.el"))
  (load (concat (expand-file-name user-emacs-directory) "core/core-startup.el")))

(moon| :basic
       ;; non-evil
       homepage
       key
       ;; evil
       angel
       ui
       other
       edit
       project
       :completion
       ;; ivy
       helm
       company
       snippet
       :os
       mac
       :utility
       ;; email
       markdown
       eshell
       tex
       dir
       git
       org
       ;; imagemagick
       :checker
       syntax
       spell
       :lang
       lsp
       ;; arduino
       general
       common-lisp
       cc
       python
       elisp
       ;; rust
       javascript
       web
       lua
       )

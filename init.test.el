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
       angel ; don't enable angel and evil in the same time
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
       cc
       lsp
       python
       elisp
       ;; rust
       javascript
       web
       lua
       )

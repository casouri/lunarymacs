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
       javascript
       web
       lua
       )


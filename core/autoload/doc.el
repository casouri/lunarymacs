;;;###autoload
(defun moon/export-doc-to-wiki ()
  "Convert org files in doc/ to markdown and copy to wiki/

Deprecated."
  (interactive)
  (require 'ox-md)
  (dolist (file (directory-files (concat moon-emacs-d-dir "doc") t "org$"))
    (message file)
    (find-file file)
    (call-interactively #'org-md-export-to-markdown)
    (rename-file
     (replace-regexp-in-string "org" "md" file)
     (concat
      moon-emacs-d-dir
      "wiki/"
      (replace-regexp-in-string
       "org"
       "md"
       (file-name-nondirectory
        file)
       )
      )
     t)
    )
  )

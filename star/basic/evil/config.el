(use-package| evil
  :config
  (evil-mode 1)
  )

(post-config| general (general-define-key :states '(normal insert emacs)
                                          :prefix moon-leader
                                          "f" '(:ignore t :which-key "file")
                                          "ff" 'find-file
                                          ))

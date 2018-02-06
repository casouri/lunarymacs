;;
;; Package
;;

(use-package| general
  :config
  (setq moon-leader "SPC")
  (setq moon-non-normal-leader "M-SPC")
  (general-define-key :states '(normal insert emacs)
                      :prefix moon-leader
                      "f" '(:ignore t :which-key "file")
                      "ff" 'find-file
                      )
  )

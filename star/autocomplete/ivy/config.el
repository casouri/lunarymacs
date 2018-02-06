(use-package| ivy :config (ivy-mode 1))
(use-package| swiper :commands (ivy-mode))
(use-package| counsel :commands (ivy-mode))
(use-package| smex :commands (ivy-mode))

(post-config| general
              (general-define-key :states '(normal insert emacs)
                                  :prefix moon-leader
                                  "f" '(:ignore t :which-key "file")
                                  ;; files
                                  "ff"  #'counsel-find-file
                                  "fr"  #'counsel-recentf
                                  "fel" #'counsel-find-library
                                  "fL"  #'counsel-locate
                                  ;; help
                                  "hb"  #'counsel-descbinds
                                  "hf"  #'counsel-describe-function
                                  "hF"  #'counsel-describe-face
                                  "hm"  #'spacemacs/describe-mode
                                  "hv"  #'counsel-describe-variable
                                  "hd"  #'apropos
                                  "hi"  #'counsel-info-lookup-symbol
                                  "hR"  #'spacemacs/counsel-search-docs
                                  "?"  #'counsel-apropos
                                  ;; insert
                                  "iu"  #'counsel-unicode-char
                                  ;; register/ring
                                  "ry"  #'counsel-yank-pop
                                  "rm"  #'counsel-mark-ring
                                  ;; jumping
                                  "sj"  #'counsel-imenu
                                  ;; themes
                                  "Ts"  #'counsel-load-theme
                                  ;; external tool
                                  "sr"  #'counsel-rg
                                  ;; buffer
                                  "bb"  #'counsel-switch-buffer
                                  )
              )

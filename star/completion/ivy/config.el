;;; -*- lexical-binding: t -*-

(use-package| ivy
  :config
  (setq
   ivy-fixed-height-minibuffer t
   ivy-use-selectable-prompt t ; so I can chose what I actually typed
   ivy-initial-inputs-alist nil ; don't use ^ as initial input
   ivy-format-function 'ivy-format-function-arrow
   )
  (ivy-mode)
  )

(post-config| general
  (default-leader
    ;; other
    "SPC" #'counsel-M-x
    ;; files
    "ff"  #'counsel-find-file
    "fr"  #'counsel-recentf
    "fL"  #'counsel-locate
    ;; help
    "hb"  #'counsel-descbinds
    "hf"  #'counsel-describe-function
    "hF"  #'counsel-describe-face
    "hl"  #'counsel-find-library
    "hm"  #'spacemacs/describe-mode
    "hk"  #'describe-key
    "hv"  #'counsel-describe-variable
    "hd"  #'apropos
    "hi"  #'counsel-info-lookup-symbol
    "hR"  #'spacemacs/counsel-search-docs
    "?"   #'counsel-apropos
    ;; insert
    "iu"  #'counsel-unicode-char
    ;; register/ring
    "ry"  #'counsel-yank-pop
    "rm"  #'counsel-mark-ring
    ;; search
    "si"  #'counsel-imenu
    "ss"  #'swiper
    "sr"  #'counsel-rg
    ;; themes
    "Ts"  #'counsel-load-theme
    ;; buffer
    "bb"  #'ivy-switch-buffer
    )
  )

(use-package| swiper :commands (swiper swiper-all))
(use-package| counsel :requires ivy)
(use-package| smex
  :commands (smex smex-major-mode-commands)
  :config (setq smex-save-file (concat moon-local-dir "smex-items")))

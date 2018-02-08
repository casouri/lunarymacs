;;; -*- lexical-binding: t -*-

(use-package| ivy
  :delight (ivy-mode " ⓘ")
  :config
  (ivy-mode 1)
  (setq
   ivy-fixed-height-minibuffer t
   )
  (default-leader
    ;; other
    "SPC" #'counsel-M-x
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

(use-package| swiper :commands (ivy-mode))
(use-package| counsel :commands (ivy-mode))
(use-package| smex
	      :commands (ivy-mode)
	      :config (setq smex-save-file (concat moon-local-dir "smex-items")))

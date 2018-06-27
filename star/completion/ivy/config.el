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
    "ho"  #'describe-symbol
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
    "ss"  #'counsel-grep-or-swiper
    "sr"  #'counsel-rg
    ;; themes
    "Ts"  #'counsel-load-theme
    ;; buffer
    "bb"  #'ivy-switch-buffer
    )
  )

(global-set-key (kbd "C-x C-y") #'counsel-yank-pop)
(global-set-key (kbd "C-c C-y") #'counsel-yank-pop)
(global-set-key (kbd "C-c C-r") #'ivy-resume)

(use-package| swiper :commands (swiper swiper-all))
(use-package| counsel
  :config (counsel-mode 1)
  :commands (counsel-ag counsel-rg counsel-pt
                        counsel-apropos counsel-bookmark
                        counsel-describe-function
                        counsel-describe-variable
                        counsel-describe-face
                        counsel-M-x counsel-file-jump
                        counsel-find-file counsel-find-library
                        counsel-info-lookup-symbol
                        counsel-imenu counsel-recentf
                        counsel-yank-pop
                        counsel-descbinds counsel-org-capture
                        counsel-grep-or-swiper))
(use-package| smex
  :commands (smex smex-major-mode-commands)
  :config (setq smex-save-file (concat moon-local-dir "smex-items")))

(use-package| (ivy-filthy-rich :repo "casouri/ivy-filthy-rich" :fetcher github)
  :config (ivy-filthy-rich-mode))

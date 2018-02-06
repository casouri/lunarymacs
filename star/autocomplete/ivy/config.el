(use-package| ivy
	      :config
	      (ivy-mode 1)
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

(use-package| swiper :commands (ivy-mode))
(use-package| counsel :commands (ivy-mode))
(use-package| smex :commands (ivy-mode))

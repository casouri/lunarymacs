;;; -*- lexical-binding: t -*-


;;
;; Package
;;

(use-package| general
  :after which-key
  :init
  (defvar moon-leader "SPC")
  (defvar moon-non-normal-leader "M-SPC")
  :config
  (general-override-mode)
  (general-create-definer default-leader
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix moon-leader
    :non-normal-prefix moon-non-normal-leader)

  (global-set-key (kbd "<escape>") #'keyboard-escape-quit)

  
  (default-leader
   "f" '(:ignore t :which-key "file")
   "i" '(:ignore t :which-key "insert")
   "h" '(:ignore t :which-key "help")
   "j" '(:ignore t :which-key "jump")
   "r" '(:ignore t :which-key "register")
   "s" '(:ignore t :which-key "search")
   "T" '(:ignore t :which-key "Theme")
   "p" '(:ignore t :which-key "project")
   "w" '(:ignore t :which-key "window")
   "b" '(:ignore t :which-key "buffer")
   "w" '(:ignore t :which-key "window")
   "q" '(:ignore t :which-key "quit")
   "m" '(:ignore t :which-key "major-mode")
   "e" '(:ignore t :which-key "eval")
   "t" '(:ignore t :which-key "toggle")
   "g" '(:ignore t :which-key "git")
   "E" '(:ignore t :which-key "Error")
   ;; file
   "fR" #'moon/rename-file
   "fs" #'save-buffer
   ;; quit
   "qq" #'save-buffers-kill-emacs
   ;; buffer
   "bm" (lambda () (interactive) (switch-to-buffer "*Messages*"))
   "bd" #'kill-buffer-and-window
   "bh" #'moon/close-help
   "bo" #'moon/kill-other-buffer
   "bh" #'moon/kill-helper
   "bb" #'list-buffers
   ;; window
   "w1" #'delete-other-windows
   "w2" #'split-window-right
   "o"  #'other-window
   ;; eval
   "er" #'eval-region
   "ef" #'eval-function
   "eb" #'eval-buffer
   "`"  #'eval-expression
   ;; toggle
   "tt" #'moon/switch-theme
  )


  (general-define-key :states '(normal insert emacs)
		      "TAB" #'indent-for-tab-command)
  (general-define-key :states 'visual
		      "TAB" #'indent-region)
  (general-define-key :states 'normal
                      "RET" #'ivy-switch-buffer)
  )

(use-package| which-key
  :config (which-key-mode 1))




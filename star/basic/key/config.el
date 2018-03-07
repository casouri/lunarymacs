;;; -*- lexical-binding: t -*-


;;
;; Package
;;

(use-package| general
  :after which-key
  :init
  (defvar moon-leader "SPC")
  (defvar moon-local-leader "S-SPC")
  (defvar moon-non-normal-leader "M-SPC")
  (defvar moon-local-non-normal-leader "C-,")
  :config
  (general-override-mode)
  (general-create-definer default-leader
    :states '(normal visual insert emacs jpnb)
    :keymaps 'override
    :prefix moon-leader
    :non-normal-prefix moon-non-normal-leader)
  (general-create-definer local-leader
    :states '(normal visual insert emacs jpnb)
    :prefix moon-local-leader
    :non-normal-prefix moon-local-non-normal-leader)

  ;; One escape to escape them all.
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
   "w" '(:ignore t :which-key "workspace")
   "q" '(:ignore t :which-key "quit")
   "m" '(:ignore t :which-key "major-mode")
   "e" '(:ignore t :which-key "error")
   "t" '(:ignore t :which-key "toggle")
   "g" '(:ignore t :which-key "git")
   "p" '(:ignore t :which-key "project")
   ;; file
   "fR" #'moon/rename-file
   "fs" #'save-buffer
   "fed" #'moon/open-init-file
   "feD" #'moon/compare-init-to-example
   ;; quit
   "qq" #'save-buffers-kill-emacs
   ;; buffer
   "bm" (lambda () (interactive) (switch-to-buffer "*Messages*"))
   "bd" #'kill-buffer-and-window
   "bh" #'moon/close-help
   "bo" #'moon/kill-other-buffer
   "bh" #'moon/kill-helper
   "bb" #'list-buffers
   ;; eval
   "`"  #'eval-expression
   ;; toggle
   "tt" #'moon/switch-theme
   "tm" #'toggle-frame-maximized
   ;; error
   "ej" #'hydra-error/next-error
   "ek" #'hydra-error/previous-error
   "eh" #'hydra-error/first-error
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

(use-package hydra
  :after general
  :config
  (defhydra hydra-error ()
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))

;;; -*- lexical-binding: t -*-


;;
;; Package
;;

(use-package| general
  :after which-key
  :init
  (setq moon-leader "SPC")
  (setq moon-non-normal-leader "S-SPC")
  :config
  (general-override-mode)

  (general-create-definer default-leader
    :states '(normal visual insert emacs jpnb)
    :keymaps 'override
    :prefix moon-leader
    :non-normal-prefix moon-non-normal-leader)

  (general-create-definer default-no-leader
    :states '(normal visual insert emacs jpnb)
    :keymaps 'override)

  (general-create-definer default-g-leader
    :states '(normal visual)
    :keymaps 'override
    :prefix "g"
    )
  (general-create-definer default-cc-leader
    :state '(normal visual insert emacs jpnb)
    :keymaps 'override
    :prefix "C-c")
  
  (default-leader
   "f" '(:ignore t :which-key "file")
   "F" '(:ignore t :which-key "Frame")
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
   "u" '(:ignore t :which-key "utility")
   "o" '(:ignore t :which-key "misc")
   "c" '(:ignore t :which-key "change visual")
   ;; Frame
   "Fd" #'delete-frame
   ;; align
   "="  #'align-regexp
   ;; open
   "ot"  #'moon/open-in-iterm
   "of"  #'moon/open-in-finder
   ;; file
   "fR"  #'moon/rename-file
   "fs"  #'save-buffer
   "fed" #'moon/open-init-file
   "feD" #'moon/compare-init-to-example
   "fD"  '(:ignore t :which-key "delete file")
   "fDD" #'moon/delete-file-and-buffer
   ;; quit
   "qq"  #'save-buffers-kill-emacs
   ;; buffer
   "bm" (lambda () (interactive) (switch-to-buffer "*Messages*"))
   "bd"  #'kill-buffer-and-window
   "bh"  #'moon/close-help
   "bo"  #'moon/kill-other-buffer
   "bh"  #'moon/kill-helper
   "bb"  #'list-buffers
   "bs"  (lambda () (interactive) (switch-to-buffer "*scratch*"))
   ;; eval
   "`"   #'eval-expression
   ;; toggle
   "tt"  #'moon/switch-theme
   "tM"  #'toggle-frame-maximized
   "tf"  #'moon/toggle-format-on-save
   ;; error
   "ej"  #'hydra-error/next-error
   "ek"  #'hydra-error/previous-error
   "eh"  #'hydra-error/first-error
   ;; utilities
   ;; jump
   "jmc" #'moon/jump-to-config
   "jmp" #'moon/jump-to-package
   "jma" #'moon/jump-to-autoload
   "jmd" #'moon/jump-to-autoload-dir
   "jmr" #'moon/jump-to-readme
  )

  (general-define-key :states '(normal visual)
                      "TAB" #'indent-for-tab-command
                      "RET" #'ivy-switch-buffer
                      "C-e" #'end-of-line)
  (general-define-key
   :keymaps 'override
   "<escape>" (lambda () (interactive)
                (keyboard-escape-quit)
                (evil-force-normal-state)))
  )

(use-package| which-key
  :config (which-key-mode 1))

(use-package| hydra
  :after general
  :config
  (defhydra hydra-error ()
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))

(global-set-key (kbd "s-h") #'windmove-left)
(global-set-key (kbd "s-j") #'windmove-down)
(global-set-key (kbd "s-k") #'windmove-up)
(global-set-key (kbd "s-l") #'windmove-right)

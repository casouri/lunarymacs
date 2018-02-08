;;; -*- lexical-binding: t -*-

;;
;; Config
;;

(defvar moon-jumped nil)

;;
;; Package
;;

(use-package| general
  :after which-key
  :init
  (defvar moon-leader "SPC")
  (defvar moon-non-normal-leader "M-SPC")
  :config
  (general-create-definer default-leader
    :states '(normal visual insert emacs)
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
    ;; file
    "fs" #'save-buffer
    ;; quit
    "qq" #'save-buffers-kill-emacs
    ;; other
    "TAB" #'moon/switch-between-buffer
    ;; buffer
    "bm" (lambda () (interactive) (switch-to-buffer "*Messages*"))
    "bk" #'kill-buffer-and-window
    ;; window
    "w1" #'delete-other-windows
    "w2" #'split-window-right
    "o"  #'other-window
    ;; eval
    "er" #'eval-region
    "ef" #'eval-function
    "eb" #'eval-buffer
  )


  (general-define-key :states '(normal insert emacs)
		      "TAB" #'indent-for-tab-command)
  (general-define-key :states 'visual
		      "TAB" #'indent-region)
  )

(use-package| which-key
  :delight (which-key-mode)
  :config (which-key-mode 1))



;;
;; Package
;;

(use-package| general
              :after which-key
              :config
              (setq moon-leader "SPC")
              (setq moon-non-normal-leader "M-SPC")
              (general-define-key :states '(normal)
                                  :prefix moon-leader
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
                                  ;; file
                                  "bs" #'save-buffer
                                  )
              )

(use-package| which-key
              :config (which-key-mode 1))

(use-package| key-chord
              :config
              (key-chord-mode 1)
              (key-chord-define-global "fd" #'moon-quit-everything))

(use-package| expand-region
              :commands er/expand-region
	      :general (default-leader
		       "v" #'er/expand-region
                       ))
(use-package| winum
              :config
              (general-define-key :states '(normal visual insert emacs)
                                  :prefix moon-leader
                                  :non-normal-prefix moon-non-normal-leader
				  ;; "0..9" '(:ignore t :wk "switch window")
                                  ;; "0" #'winum-select-window-0-or-10
                                  ;; "1" #'winum-select-window-1
                                  ;; "2" #'winum-select-window-2
                                  ;; "3" #'winum-select-window-3
                                  ;; "4" #'winum-select-window-4
                                  ;; "5" #'winum-select-window-5
                                  ;; "6" #'winum-select-window-6
                                  ;; "7" #'winum-select-window-7
                                  ;; "8" #'winum-select-window-8
                                  ;; "9" #'winum-select-window-9
                                  )
              )

(use-package| undo-tree
              :config (global-undo-tree-mode)
              (setq undo-tree-visualizer-timestamps t
                    undo-tree-visualizer-diff t))

;;
;; Config
;;

(electric-pair-mode 1)

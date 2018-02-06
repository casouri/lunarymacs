(use-package expand-region
              :commands er/expand-region
	      :general (default-leader
		       "v" #'er/expand-region
                       ))

(use-package undo-tree
              :config (global-undo-tree-mode)
              (setq undo-tree-visualizer-timestamps t
                    undo-tree-visualizer-diff t))

;;
;; Config
;;

(electric-pair-mode 1)

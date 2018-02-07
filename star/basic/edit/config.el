(use-package
  expand-region
  :commands er/expand-region
  :general (default-leader
	     "v" #'er/expand-region
             )
  )

(use-package
  undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package
  recentf-ext
  :commands (recentf counsel-recentf)
  )

;;
;; Config
;;

(electric-pair-mode 1)

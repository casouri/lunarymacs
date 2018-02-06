;;
;; Config
;;

(global-hl-line-mode 1)

(use-package rainbow-delimiters
	      :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; (use-package| highlight-parentheses
;; 	      :hook prog-mode
;; 	      :config (global-highlight-parentheses-mode t))


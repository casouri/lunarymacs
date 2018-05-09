(use-package| tex-site
  :defer
  :config
  ;; suggested by auctex manual
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-syntactic-comment t)
  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

(use-package| latex-preview-pane
  :commands latex-preview-pane-mode)

(post-config| general
  (default-leader
    :keymaps 'tex-mode-map
    "tp" #'latex-preview-pane-mode))

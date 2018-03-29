(use-package| js2-mode
  :mode "\\.js\\'")

(use-package| js2-refactor
  :after js2-mode
  :config
  (js2r-add-keybindings-with-prefix "SPC u r")
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  )

(use-package| xref-js2
  :after js2-mode
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook
                              'xref-backend-functions
                              #'xref-js2-xref-backend nil t))))

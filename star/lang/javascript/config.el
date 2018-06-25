(use-package| js2-mode
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  ;; don't lint
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package| xref-js2
  :after js2-mode
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook
                              'xref-backend-functions
                              #'xref-js2-xref-backend nil t))))

;; eslint
;; (setq flycheck-eslint-rules-directories
;;      `(,(concat
;;          (file-name-directory (buffer-file-name))
;;          "eslint")))

;;
;; format


(use-package| js-format
  :after js2-mode
  :config
  (js-format-setup "standard")
  (add-to-list 'moon-smart-format-alist '(js2-mode . js-format-buffer)))

;; indent
(setq js-indent-level 2)

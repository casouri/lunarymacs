;; (use-package| js2-mode
;;   :mode "\\.js\\'")

(use-package| xref-js2
  :mode "\\.js$"
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
  :mode "\\.js$"
  :config
  (add-hook 'js2-mode-hook
            (lambda()
              (js-format-setup "standard")
              (add-hook 'after-save-hook (lambda ()
                                           (when moon-format-on-save
                                             (js-format-buffer)))))))
;; indent
(setq js-indent-level 2)

;; (use-package| js2-mode
;;   :mode "\\.js\\'")

(use-package| js2-refactor
  :config
  (js2r-add-keybindings-with-prefix "SPC u r")
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  )

(use-package| xref-js2
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

(defvar moon-javascript-format-on-save nil
  "Auto format on save.")


(use-package| js-format
  :config
  (add-hook 'js2-mode-hook
            (lambda()
              (js-format-setup "standard")
              (add-hook 'after-save-hook (lambda ()
                                           (when moon-javascript-format-on-save
                                             (js-format-buffer)))))))
;; indent
(setq js-indent-level 2)

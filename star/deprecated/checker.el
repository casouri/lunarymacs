;; Install proselint by macports.
;; (load-package flycheck
;;   :hook (text-mode-hook . flycheck-mode)
;;   :extern "proselint"
;;   :config
;;   (flycheck-define-checker proselint
;;     "A linter for prose."
;;     :command ("proselint" source-inplace)
;;     :error-patterns
;;     ((warning line-start (file-name) ":" line ":" column ": "
;; 	      (id (one-or-more (not (any " "))))
;; 	      (message) line-end))
;;     :modes (text-mode org-mode))
;;   (add-to-list 'flycheck-checkers 'proselint))

(load-package langtool
  :config
  (setq langtool-language-tool-server-jar
        "/Users/yuan/attic/LanguageTool-5.0/languagetool-server.jar")
  :extern "~/attic/LanguageTool-5.0"
  :commands
  langtool-check
  langtool-check-done
  langtool-switch-default-buffer)

;; -*- lexical-binding: t -*-

;;; Key
;;

(luna-def-key
 :keymaps 'flyspell-mode-map
 "C-;" #'flyspell-auto-correct-previous-word
 [down-mouse-3] #'flyspell-correct-word
 "C-." nil
 
 :leader
 "lcc" #'langtool-check
 "lcd" #'langtool-check-done)

;;; Packages

(load-package flymake
  :hook ((emacs-lisp-mode-hook c-mode-hook c++-mode-hook python-mode-hook)
         . flymake-mode))

(defun flymake-clean ()
  "Clean flymake temp files in current directory."
  (interactive)
  (shell-command-to-string "rm *flymake.o"))

;; Install dictionaries: http://wordlist.aspell.net
;; or by macports.
(load-package flyspell
  :hook (text-mode-hook . flyspell-mode-hook)
  :config (setq flyspell-issue-message-flag nil))

(load-package writegood-mode
  :hook (text-mode . writegood-mode))

;; Install proselint by macports.
(load-package flycheck
  :hook (text-mode-hook . flycheck-mode)
  :config
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
	      (id (one-or-more (not (any " "))))
	      (message) line-end))
    :modes (text-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

(load-package langtool
  :config
  (setq langtool-language-tool-server-jar
        "/Users/yuan/attic/LanguageTool-5.0/languagetool-server.jar")
  :commands
  langtool-check
  langtool-check-done
  langtool-switch-default-buffer)

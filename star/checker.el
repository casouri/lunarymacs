;; -*- lexical-binding: t -*-

;;; Key
;;

(luna-def-key
 :keymaps 'flyspell-mode-map
 "C-;" #'flyspell-auto-correct-previous-word
 [down-mouse-3] #'flyspell-correct-word
 "C-." nil
 "C-," nil
 "C-M-i" nil
 "C-c $" nil)

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
  :config
  (setq flyspell-issue-message-flag nil)
  ;; Add curely quotes so words like “didn’t” are properly handled.
  ;; ispell-mode overwrites ‘ispell-dictionary-alist’ every time
  ;; it is turned on so we need to modify the variable in the hook.
  (add-hook 'flyspell-mode-hook
            (lambda ()
              (add-to-list 'ispell-dictionary-alist
                           '(nil "[[:alpha:]]"
                                 "[^[:alpha:]]"
                                 "['’]" nil ("-B") nil utf-8))))
  :extern "aspell port install aspell; port install aspell-dict-en"
  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode))

;; Why no wucuo: Too many edge cases when you check buffer on a region
;; on save, and checking on post-command-hook is also more convenient.
;; For example, iimg doesn’t work with wucuo: it checks the base64
;; strings and hangs Emacs on save.

(load-package writegood-mode
  :hook (text-mode . writegood-mode))


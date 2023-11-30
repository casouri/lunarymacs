;; -*- lexical-binding: t -*-

;;; Key
;;

(luna-key-def
 :keymaps 'flyspell-mode-map
 "C-;" #'flyspell-auto-correct-previous-word
 [down-mouse-3] #'flyspell-correct-word
 "C-." nil
 "C-," nil
 "C-M-i" nil
 "C-c $" nil
 :keymaps 'flyspell-mouse-map
 [mouse-2] nil
 :---
 :leader
 ;; Mnemonics: "list errors"
 "le" #'flymake-show-buffer-diagnostics)

;;; Packages

(load-package flymake
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'flymake-mode)
  (add-hook 'c-mode-hook #'flymake-mode)
  (add-hook 'c++-mode-hook #'flymake-mode)
  :config
  (setq flymake-mode-line-lighter ""))


(defvar flyspell-skip-commands
  '( scroll-down scroll-up next-line previous-line
     luna-scroll-up-reserve-point
     luna-scroll-down-reserve-point)
  "Don’t run ‘flyspell-post-command-hook’ after these commands.")

;; Install dictionaries: http://wordlist.aspell.net
;; or by macports.
(load-package flyspell
  :extern "aspell"
  :autoload-hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  :config
  (setq flyspell-issue-message-flag nil)
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '( "-W" "3" "--sug-mode=ultra" "--lang=en_US"
                               ;; run-together allows compound words
                               ;; like "viewport".
                               "--camel-case" "--run-together"))
    ;; Check spelling for symbols too, but only if we have aspell
    ;; (which can check camelcase.)
    (add-to-list 'flyspell-prog-text-faces 'font-lock-variable-name-face)
    (add-to-list 'flyspell-prog-text-faces 'font-lock-function-name-face)
    (add-to-list 'flyspell-prog-text-faces 'font-lock-type-face)
    ;; Variable and function use don’t have faces on.
    (add-to-list 'flyspell-prog-text-faces nil))

  ;; Add curely quotes so words like “didn’t” are properly handled.
  ;; ispell-mode overwrites ‘ispell-dictionary-alist’ every time
  ;; it is turned on so we need to modify the variable in the hook.
  (add-hook 'flyspell-mode-hook
            (lambda ()
              (add-to-list 'ispell-dictionary-alist
                           '(nil "[[:alpha:]]"
                                 "[^[:alpha:]]"
                                 "['’]" nil ("-B") nil utf-8))))
  (advice-add 'flyspell-post-command-hook :around
              (lambda (oldfn &rest args)
                (unless (memq this-command flyspell-skip-commands)
                  (apply oldfn args)))))

(luna-note-extern "aspell"
  "For macports:
    sudo port install aspell aspell-dict-en
For guix:
    guix install aspell aspell-dict-uk")

;; Why no wucuo: Too many edge cases when you check buffer on a region
;; on save, and checking on post-command-hook gives much better user
;; experience. I don’t want to see spell warning for words far away
;; from the point, I just want to see spell warning for the words I’m
;; typing right now. And I want to see spell warning as soon as I
;; typed a word. Wucuo sucks at both. Also, iimg doesn’t work with
;; wucuo: it checks the base64 strings and hangs Emacs on save.

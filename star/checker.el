;;; -*- lexical-binding: t -*-

;;; Key

(luna-with-eval-after-load 'key.general
  (luna-default-leader
    "ts" #'luna-toggle-spell-check
    "ef" #'flyspell-correct-previous
    "eg" #'langtool-check
    "ec" #'langtool-correct-buffer
    "is" #'synonyms))

;;; flyspell
;;
;; install dictionaries: http://wordlist.aspell.net
;;
(load-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (luna-with-eval-after-load 'key.general
    (general-define-key
     :keymaps 'flyspell-mode-map
     "C-," nil
     "C-M-i" nil
     "C-." nil
     "C-c $" nil))
  ;; right click on mac touchpad
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (setq flyspell-issue-message-flag nil)
  ;; (when (executable-find "hunspell")
  ;;   (setq-default ispell-program-name "hunspell")
  ;;   (setq ispell-really-hunspell t)
  ;;   (setq ispell-dictionary "en_US"))
  )

(load-package writegood-mode
  :hook ((fundamental-mode org-mode) . writegood-mode))

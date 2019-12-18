;;; -*- lexical-binding: t -*-

;;; Key

(with-eval-after-load 'luna-general-config
  (luna-default-leader
    "ts" #'luna-toggle-spell-check
    "ef" #'flyspell-correct-previous
    "eg" #'langtool-check
    "ec" #'langtool-correct-buffer))

;;; flymake
;;
;; otherwise litters my directory with temp files
(setq-default flymake-diagnostic-functions nil)
;; require error
;; (setq elisp-flymake-byte-compile-load-path
;;       (append elisp-flymake-byte-compile-load-path load-path))

;;; flyspell
;;
;; install dictionaries: http://wordlist.aspell.net
;;
(load-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (with-eval-after-load 'luna-general-config
    (general-define-key
     :keymaps 'flyspell-mode-map
     "C-," nil
     "C-M-i" nil
     "C-." nil
     "C-c $" nil))
  ;; right click on mac touchpad
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (setq flyspell-issue-message-flag nil))

(load-package writegood-mode
  :hook ((fundamental-mode org-mode) . writegood-mode))

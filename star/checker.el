;; -*- lexical-binding: t -*-

;;; Key
;;
;; C-; correct previous word

;;; flymake
;;
;; otherwise litters my directory with temp files
(setq-default flymake-diagnostic-functions nil)
;; require error
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

(dolist (hook '(emacs-lisp-mode-hook
                c-mode-hook
                c++-mode-hook
                python-mode-hook))
  (add-hook hook #'flymake-mode))

;;; flyspell
;;
;; install dictionaries: http://wordlist.aspell.net
;;
(load-package flyspell
  :hook ((text-mode-hook . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode))
  :config
  (with-eval-after-load 'general
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
  :hook ((fundamental-mode-hook org-mode-hook) . writegood-mode))

;;; Grammly

;; (load-package flycheck-grammarly
;;   :hook ((org-mode-hook . flycheck-mode)
;;          (text-mode . flycheck-mode)))


;;; Flycheck

(load-package flycheck
  :hook ((text-mode-hook
          org-mode-hook)
         . flycheck-mode))

;;; proselint

(with-eval-after-load flycheck-mode
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
	      (id (one-or-more (not (any " "))))
	      (message) line-end))
    :modes (text-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

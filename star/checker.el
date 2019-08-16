;;; -*- lexical-binding: t -*-

;; (load-package flycheck
;;   ;; :commands global-flycheck-mode
;;   ;; :init (add-hook-for-once| prog-mode-hook (lambda () (global-flycheck-mode 1)))
;;   :commands flycheck-mode)

;;; Key

;; (post-config| general
;;   (luna-default-leader
;;     "en" #'hydra-error/flymake-goto-next-error
;;     "ep" #'hydra-error/flymake-goto-prev-error))

(luna-with-eval-after-load 'key.general
  (luna-default-leader
    "ts" #'luna-toggle-spell-check
    "ef" #'flyspell-correct-previous
    "eg" #'langtool-check
    "ec" #'langtool-correct-buffer
    "is" #'synonyms))

;;; Package

;; (load-package flymake-diagnostic-at-point
;;   :after flymake
;;   :init (setq flymake-diagnostic-at-point-error-prefix "|")
;;   :config (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;;; Config

;; (post-config| hydra
;;   (require 'hydra)
;;   (defhydra hydra-error ()
;;     "goto-error"
;;     ("n" flymake-goto-next-error "next")
;;     ("p" flymake-goto-prev-error "prev")
;;     ("q" nil "quit")))

;;;; flymake

;; (with-eval-after-load 'emacs-lisp-mode
;;   (with-eval-after-load 'flymake
;;     (setq elisp-flymake-byte-compile-load-path
;;           (append elisp-flymake-byte-compile-load-path load-path))))

;;; flyspell

(load-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (with-eval-after-load 'key.general
    (general-define-key
     :keymaps 'flyspell-mode-map
     "C-," nil
     "C-M-i" nil
     "C-." nil
     "C-c $" nil))
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (setq flyspell-issue-message-flag nil)
  ;; (when (executable-find "hunspell")
  ;;   (setq-default ispell-program-name "hunspell")
  ;;   (setq ispell-really-hunspell t)
  ;;   (setq ispell-dictionary "en_US"))
  )

;; (load-package langtool
;;   :commands langtool-check
;;   :config
;;   (setq langtool-language-tool-server-jar "/usr/local/Cellar/languagetool/4.1/libexec/languagetool-server.jar")
;;   (setq langtool-java-bin "/usr/bin/java"))

(load-package writegood-mode
  :hook ((fundamental-mode org-mode) . writegood-mode))

;; (load-package (synonyms :fetcher url :url "https://www.emacswiki.org/emacs/download/synonyms.el")
;;   :commands synonyms)

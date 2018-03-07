;;; -*- lexical-binding: t -*-

(use-package| evil
  :config
  (evil-mode 1)
  ;; fix paste issue in evil visual mode
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  (default-leader "ij" #'evil-insert-line-below
    "ik" #'evil-insert-line-above)
  )

(post-config| evil
              (message "it works!"))

(use-package| evil-matchit
  :config (evil-matchit-mode)
  )

(use-package| evil-search-highlight-persist
  :commands (evil-search swiper))

(use-package| evil-surround
              :hook (evil-visual-state . (lambda () (setq global-evil-surround-mode)))
              :config
              (general-define-key
               :states 'visual
               "s" #'evil-surround-region
               "S" #'evil-substitute
               )
              )


(use-package| evil-nerd-commenter
  :commands evilnc-comment-operator
  :general
  (:keymaps 'visual
	    :prefix "g"
            "c" #'evilnc-comment-operator)
  )


(use-package| evil-escape
  :config (evil-escape-mode 1))

(use-package| evil-ediff
  :hook (ediff-mode . (lambda () (require 'evil-ediff))))

(use-package| evil-vimish-fold
  :after evil
  :init (setq vimish-fold-dir (concat moon-local-dir "vimish-fold"))
  :hook (prog-mode . evil-vimish-fold-mode))

;;
;; Config
;;

(post-config| evil
  (define-key evil-normal-state-map "Q" #'moon/query-relace-point)
  (define-key evil-visual-state-map "Q" #'moon/query-replace-region))

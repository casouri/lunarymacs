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

(use-package| evil-mc
  :commands (evil-mc-find-next-cursor
             evil-mc-find-prev-cursor))

(use-package| evil-escape
  :config (evil-escape-mode 1))


(use-package| evil-multiedit
  :config (moon-evil-multiedit-default-keybinds))

(defun moon-evil-multiedit-default-keybinds ()
  "Sets up the default keybindings for `evil-multiedit'."
  (define-key evil-visual-state-map "R" #'evil-multiedit-match-all)
  (define-key evil-normal-state-map (kbd "s-d") #'evil-multiedit-match-symbol-and-next)
  (define-key evil-visual-state-map (kbd "s-d") #'evil-multiedit-match-and-next)
  (define-key evil-normal-state-map (kbd "s-D") #'evil-multiedit-match-symbol-and-prev)
  (define-key evil-visual-state-map (kbd "s-D") #'evil-multiedit-match-and-prev)
  (define-key evil-insert-state-map (kbd "s-d") #'evil-multiedit-toggle-marker-here)
  (define-key evil-visual-state-map (kbd "C-s-D") #'evil-multiedit-restore)
  (define-key evil-motion-state-map (kbd "RET") #'evil-multiedit-toggle-or-restrict-region)
  (define-key evil-multiedit-state-map (kbd "RET") #'evil-multiedit-toggle-or-restrict-region)
  (define-key evil-multiedit-state-map (kbd "C-n") #'evil-multiedit-next)
  (define-key evil-multiedit-state-map (kbd "C-p") #'evil-multiedit-prev)
  (define-key evil-multiedit-insert-state-map (kbd "C-n") #'evil-multiedit-next)
  (define-key evil-multiedit-insert-state-map (kbd "C-p") #'evil-multiedit-prev)
  (evil-ex-define-cmd "ie[dit]" #'evil-multiedit-ex-match))


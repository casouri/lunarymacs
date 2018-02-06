(use-package evil
  :config
  (evil-mode 1)
  ;; fix paste issue in evil visual mode
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  (default-leader "ij" #'evil-insert-line-below
                  "ik" #'evil-insert-line-above)
  )

(use-package evil-matchit
              :hook prog-mode
              :config (global-evil-matchit-mode)
              )

(use-package evil-search-highlight-persist
              :commands (evil-search swiper))

(use-package evil-surround
	      :hook evil-viual-state-hook
	      :config
              (general-define-key :keymaps 'visual
                                  "s" #'evil-surround-region
                                  "S" #'evil-substitute
                                  )
              )


(use-package evil-nerd-commenter
              :commands evilnc-comment-operator
	      :config
	      (general-define-key :keymaps 'visual
				  :prefix "g"
                                  "c" #'evilnc-comment-operator)
	      )

(use-package evil-mc
              :commands (evil-mc-find-next-cursor
                         evil-mc-find-prev-cursor))

(use-package evil-escape :config (evil-escape-mode 1))

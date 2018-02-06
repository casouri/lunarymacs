(use-package| evil
  :config
  (evil-mode 1)
  ;; fix paste issue in evil visual mode
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  )

(use-package| evil-matchit
              :commands (evil-mode)
              :config (global-evil-matchit-mode)
              )

(use-package| evil-search-highlight-persist
              :commands (evil-search swiper))

(use-package| evil-surround
              :commands (evil-surround-region evil-substitute))

(post-config| general
              (general-define-key :keymaps 'normal
                                  "s" #'evil-surround-region
                                  "S" #'evil-substitute
                                  )
              (general-define-key :keymaps 'normal
                                  :prefix "g"
                                  "c" #'evilnc-comment-operator))

(use-package| evil-nerd-commenter
              :commands evilnc-comment-operator)

(use-package| evil-mc
              :commands (evil-mc-find-next-cursor
                         evil-mc-find-prev-cursor))

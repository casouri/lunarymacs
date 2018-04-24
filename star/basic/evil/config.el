;;; -*- lexical-binding: t -*-

(use-package| evil
  :config
  (evil-mode 1)
  ;; fix paste issue in evil visual mode
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; https://github.com/syl20bnr/spacemacs/issues/6636
  ;; setting this directly doesn't work
  ;; you have to set it through customize
  ;; (customize-set-variable evil-search-module 'evil-search)
  (setq evil-ex-substitute-global t)
  )

(post-config| evil
              (message "it works!"))

(use-package| evil-matchit
  :config (evil-matchit-mode)
  )

(use-package| evil-search-highlight-persist
  :after evil
  :config (global-evil-search-highlight-persist)
  (set-face-attribute 'evil-search-highlight-persist-highlight-face
                      nil
                      :background (face-attribute 'highlight :background)))

(use-package| evil-surround
  :after evil
  :config (global-evil-surround-mode 1)
  (evil-define-key 'visual 'global "s" 'evil-surround-region))


(use-package| evil-nerd-commenter
  :commands evilnc-comment-operator
  :general
  (:keymaps 'visual
	    :prefix "g"
            "c" #'evilnc-comment-operator)
  )


(use-package| evil-escape
  :config (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "<escape>"))

(use-package| evil-ediff
  :hook (ediff-mode . (lambda () (require 'evil-ediff))))

(use-package| evil-vimish-fold
  :after evil
  :init (setq vimish-fold-dir (concat moon-local-dir "vimish-fold"))
  :hook (prog-mode . evil-vimish-fold-mode))

(after-load| term
  (require 'evil-collection-term)
  (evil-collection-term-setup)
  (add-hook 'term-mode-hook (lambda ()
                              (setq-local evil-insert-state-cursor 'box)))
  )


;;
;; Config
;;

;;
;; Replace some keys

(post-config| evil
  (define-key evil-normal-state-map "Q" #'moon/query-relace-point)
  (define-key evil-visual-state-map "Q" #'moon/query-replace-region))

(post-config| general
  (general-define-key :states 'insert
                      "M-n" #'next-line
                      "M-p" #'previous-line
                      )
  (default-g-leader "s" #'save-buffer)
  (default-leader
    "sc" #'moon/clear-evil-search)
  "ij" #'evil-insert-line-below
  "ik" #'evil-insert-line-above
  (default-leader
    :keymaps 'term-mode-map
    "c" '((lambda ()
            (interactive)
            (term-char-mode)
            (evil-insert-state)) :which-key "char-mode")
    "l" #'term-line-mode))

;; This way "/" respects the current region
;; https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs
(defun moon-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'moon-isearch-with-region)

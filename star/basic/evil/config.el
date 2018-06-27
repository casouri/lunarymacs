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

(after-load| evil-search
  ;; . in visual mode
  ;; binded below by general.el
  (defun moon/make-region-search-history ()
    "Make region a histroy so I can use cgn."
    (interactive)
    (let ((region (strip-text-properties (funcall region-extract-function nil))))
      (push region evil-ex-search-history)
      (setq evil-ex-search-pattern (evil-ex-make-search-pattern region))
      (deactivate-mark)))


  ;; / in visual mode will start search immediatly
  (defun moon-evil-ex-start-search-with-region-string ()
    (let ((selection (with-current-buffer (other-buffer (current-buffer) 1)
                       (when (evil-visual-state-p)
                         (let ((selection (buffer-substring-no-properties (region-beginning)
                                                                          (1+ (region-end)))))
                           (evil-normal-state)
                           selection)))))
      (when selection
        (evil-ex-remove-default)
        (insert selection)
        (evil-ex-search-activate-highlight (list selection
                                                 evil-ex-search-count
                                                 evil-ex-search-direction)))))

  (advice-add #'evil-ex-search-setup :after #'moon-evil-ex-start-search-with-region-string)

  ;; # in visual mode
  (defun moon-evil-ex-search-word-backward-advice (old-func count &optional symbol)
    (if (evil-visual-state-p)
        (let ((region (buffer-substring-no-properties
                       (region-beginning) (1+ (region-end)))))
          (setq evil-ex-search-pattern region)
          (deactivate-mark)
          (evil-ex-search-full-pattern region count 'backward))
      (apply old-func count symbol)))

  ;; * in visual mode
  (defun moon-evil-ex-search-word-forward-advice (old-func count &optional symbol)
    (if (evil-visual-state-p)
        (let ((region (buffer-substring-no-properties
                       (region-beginning) (1+ (region-end)))))
          (setq evil-ex-search-pattern region)
          (deactivate-mark)
          (evil-ex-search-full-pattern region count 'forward))
      (apply old-func count symbol)))

  (advice-add #'evil-ex-search-word-backward :around #'moon-evil-ex-search-word-backward-advice)
  (advice-add #'evil-ex-search-word-forward :around #'moon-evil-ex-search-word-forward-advice))

(use-package| evil-matchit
  :after evil
  :defer 2
  :config (evil-matchit-mode)
  )

;; (use-package| evil-surround
;;   :after evil
;;   :config (global-evil-surround-mode 1)
;;   (evil-define-key 'visual 'global "s" 'evil-surround-region))

(use-package| evil-nerd-commenter
  :commands evilnc-comment-operator)

(post-config| general
  (general-define-key
   :keymaps 'visual
   :prefix "g"
   "c" #'evilnc-comment-operator))

;; (use-package| evil-escape
;;   :config (evil-escape-mode 1)
;;   (setq-default evil-escape-key-sequence "<escape>"))

(use-package| evil-ediff
  :after evil
  :defer 2
  :hook (ediff-mode . (lambda () (require 'evil-ediff))))

(use-package| evil-vimish-fold
  :after evil
  :defer 2
  :init (setq vimish-fold-dir (concat moon-local-dir "vimish-fold")))


(use-package| evil-embrace
  ;; doesn't need defer, embrace is autoloaded
  :after embrace evil
  :config (evil-embrace-enable-evil-surround-integration))


;;
;; Config
;;

;;
;; Replace some keys

(post-config| general
  (after-load| evil
    (general-define-key
     :states 'normal
     "c" (general-key-dispatch 'evil-change "s" #'embrace-change)
     "d" (general-key-dispatch 'evil-delete "s" #'embrace-delete))
    
    (general-define-key
     :states 'visual
     ;; `evil-change' is not bound in `evil-visual-state-map' by default but
     ;; inherited from `evil-normal-state-map'
     ;; if you don't want "c" to be affected in visual state, you should add this
     "c" #'evil-change
     "d" #'evil-delete
     "s" #'embrace-add
     "x" #'exchange-point-and-mark ; for expand-region
     "." #'moon/make-region-search-history
     )

    (general-define-key
     :states 'insert
     "M-n" #'next-line
     "M-p" #'previous-line
     "C-a" #'evil-beginning-of-line
     "C-e" #'evil-end-of-line)

    (general-define-key
     :states 'normal
     "H"   #'evil-beginning-of-line
     "L"   #'evil-end-of-line
     "P"   #'evil-paste-from-register
     "U"   #'undo-tree-redo
     "M-d" #'evil-scroll-up)
    
    (default-g-leader "s" #'embrace-commander)

    (default-leader
      "sc" #'moon/clear-evil-search
      "ij" #'evil-insert-line-below
      "ik" #'evil-insert-line-above)

    (default-leader
      :keymaps 'term-mode-map
      "c" '((lambda ()
              (interactive)
              (term-char-mode)
              (evil-insert-state)) :which-key "char-mode")
      "l" #'term-line-mode
      "bl" #'evil-switch-to-windows-last-buffer)))

;; This way "/" respects the current region
;; but not when you use 'evil-search as evil-search-module
;; https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs
(defun moon-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'moon-isearch-with-region)

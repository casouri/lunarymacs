;; -*- lexical-binding: t -*-

(require 'luna-f)

;;; Key

(with-eval-after-load 'luna-general-config
  (luna-default-leader
    "tl" #'luna-toggle-left-margin
    "sr" #'color-rg-search-input
    "C-o" #'outline-toggle-children)

  (general-define-key
   ;; Hungry delete
   [remap backward-delete-char-untabify] #'luna-hungry-delete
   ;; helpful
   "C-h C-h" #'ghelp-describe
   "C-h k" #'helpful-key
   "C-h e" #'ghelp-describe-as-in-emacs-lisp-mode
   "C-x i" #'luna-insert-special-symbol
   "C-c '" #'separedit
   "C-s-i" #'color-outline-toggle-all
   "C-/"   #'undo-only
   "C-."   #'undo-redo)

  (general-define-key
   :keymaps '(c-mode c++-mode)
   "M-RET" #'srefactor-refactor-at-point))

;;; Config

;; (which-function-mode)

;;; Package

(load-package ws-butler
  ;; global mode interferes with magit
  :hook (prog-mode . ws-butler-mode))


(load-package expand-region
  :config
  ;; it interferes angel.el's region transient map
  ;; specifically, the region-deactive-hook
  ;; doesn't run right after the region highlight is off
  (setq expand-region-fast-keys-enabled nil)
  :commands
  er/expand-region
  er/mark-defun
  er/mark-word
  er/mark-symbol
  er/mark-inside-quotes
  er/mark-outside-quotes
  er/mark-inside-pairs
  er/mark-outside-pairs
  er/contract-region)


(load-package isolate
  :commands (isolate-quick-add
             isolate-quick-change
             isolate-quick-delete
             isolate-long-add
             isolate-long-change
             isolate-long-delete))


;; (load-package undo-tree
;;   :config (global-undo-tree-mode)
;;   (setq undo-tree-visualizer-timestamps t
;;         undo-tree-enable-undo-in-region nil
;;         undo-tree-visualizer-diff t
;;         undo-tree-auto-save-history t
;;         undo-tree-history-directory-alist
;;         `(("." . ,(luna-f-join luna-cache-dir "undo-tree")))))


(load-package diff-hl
  :hook (prog-mode-hook . diff-hl-mode)
  :config (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(load-package separedit
  :commands separedit
  :config
  (setq separedit-default-mode 'text-mode
        separedit-remove-trailing-spaces-in-comment t))


(load-package yasnippet
  :config (yas-global-mode))


(load-package color-rg
  :init
  (define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg)
  :config
  (let ((map color-rg-mode-map))
    (define-key map "p" #'color-rg-jump-prev-keyword)
    (define-key map "n" #'color-rg-jump-next-keyword)
    (define-key map "f" #'color-rg-jump-next-file)
    (define-key map "b" #'color-rg-jump-prev-file))
  (let ((map color-rg-mode-edit-map))
    (define-key map (kbd "C-c C-f") #'color-rg-jump-next-file)
    (define-key map (kbd "C-c C-n") #'color-rg-jump-next-keyword)
    (define-key map (kbd "C-c C-p") #'color-rg-jump-prev-keyword)
    (define-key map (kbd "C-c C-b") #'color-rg-jump-prev-file)
    (define-key map (kbd "C-c C-k") #'color-rg-switch-to-view-mode))
  :commands (isearch-toggle-color-rg
             color-rg-search-input
             color-rg-search-symbol
             color-rg-search-project
             color-rg-search-project-rails))


(load-package color-outline
  :hook (prog-mode-hook . color-outline-mode)
  :config (define-key hi-lock-map (kbd "C-x w b") nil))


(load-package visual-regexp
  :commands (vr/replace
             vr/query-replace
             vr/mc-mark))


(load-package srefactor
  :hook ((c-mode-hook . semantic-mode)
         (c++-mode-hook . semantic-mode)))


(add-to-list 'luna-package-list 'helpful)

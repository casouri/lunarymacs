;;; -*- lexical-binding: t -*-

(use-package| expand-region
  :commands er/expand-region)

(post-config| general
  (default-leader
    "v" #'er/expand-region))

(use-package| embrace
  :commands (embrace-add embrace-delete embrace-change embrace-commander))


(use-package| undo-tree
  :config (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package| recentf-ext
  :commands (recentf counsel-recentf)
  )

(use-package| avy
  :defer 2
  :config
  (setq avy-background t)
  (setq avy-all-windows nil))

(post-config| general
  (default-leader
    "k" #'avy-goto-char-timer))

(use-package| minimap
  :config
  (setq
   minimap-width-fraction 0.1
   minimap-window-location 'right
   minimap-update-delay 0)
  (custom-theme-set-faces 'user
                          '(minimap-active-region-background
                            ((((background dark)) (:background "#61526E"))
                             (t (:background "#d3d3e7")))))
  :commands minimap-mode)

(post-config| general
  (default-leader "tm" #'minimap-mode))

;; used for chinese editiing on macOS
(load| switch-input-mode)


(use-package| hungry-delete
  :defer 2
  :config
  (global-set-key (kbd "M-DEL") #'hungry-delete-backward))

(use-package| (bicycle :repo "tarsius/bicycle" :fetcher github)
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global))
  :init
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;;
;; Config
;;

(electric-pair-mode 1)

;; smooth scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(post-config| general
  (general-define-key
   :states 'insert
   "<C-return>" #'moon/jump-newline-below
   "<C-S-return>" #'moon/jump-newline-above
   "C-;" #'moon/insert-semi-at-eol
   )
  (general-define-key
   :states 'normal
   "J" #'moon/scroll-down-reserve-point
   "K" #'moon/scroll-up-reserve-point)) 

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defun open-next-line (arg)
      "Move to the next line and then opens a line.
    See also `newline-and-indent'."
      (interactive "p")
      (end-of-line)
      (open-line arg)
      (next-line 1)
      (when newline-and-indent
        (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)

;; never tested
;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ivy-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


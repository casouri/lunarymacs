;;; -*- lexical-binding: t -*-

;;
;; Config
;;

(global-hl-line-mode 1)

;;
;; Cursor Color
;;


(defun moon-normal-state-cursor-color ()
  "Cursor color in normal state."
  (cond
   ((equal moon-current-theme "spacemacs-dark")
    lunary-yellow)
   ((equal moon-current-theme "spacemacs-light")
    spacemacs-light-purple)
   (t doom-blue)
   ))

(defun moon-insert-state-cursor-color ()
  "Cursor color in insert state."
  lunary-pink)

(change-cursor-on-hook| evil-normal-state-entry-hook moon-normal-state-cursor-color)
;; secure cursor color after changing theme
(change-cursor-on-hook| moon-load-theme-hook moon-normal-state-cursor-color)
(change-cursor-on-hook| evil-insert-state-entry-hook moon-insert-state-cursor-color)

;;
;; Font
;;

;; (moon-set-font| :family "Source Code Pro" :weight 'light :size 14)
(moon-set-font| :family "SF Mono" :weight 'light :size 14)

;;
;; Package
;;

;; spacemacs-theme
(add-to-list 'custom-theme-load-path (car (directory-files (concat moon-package-dir "elpa/") t "spacemacs-theme.+")) t)
(load-theme 'spacemacs-dark t)

(use-package| rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package| rainbow-mode
  :commands rainbow-mode)

(use-package| highlight-parentheses
  :init
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (global-highlight-parentheses-mode 1)
  ;;highlight only the most inner pair
  (setq hl-paren-colors '("green"))
  ;; red in light theme, green in dark
  (change-by-theme '((spacemacs-dark . (progn (setq hl-paren-colors '("green")) (hl-paren-color-update)))
                     (spacemacs-light . (progn (setq hl-paren-colors '("red")) (hl-paren-color-update))))))


;;
;; Mode-line

(defun moon-load-powerline ()
  (require 'powerline)
  (setq powerline-default-separator 'slant)
  (setq powerline-image-apple-rgb t)
  (setq powerline-height 28)
  (load (concat moon-star-dir "basic/ui/lunaryline/lunaryline"))
  (lunaryline-default-theme)
  ;; fix different separator color after changing theme
  (add-hook 'moon-load-theme-hook #'powerline-reset)
  (remove-hook 'after-change-major-mode-hook #'moon-load-powerline))

(add-hook 'after-change-major-mode-hook #'moon-load-powerline)


;;
;; Line number

(use-package| nlinum
  :init 
  (add-hook 'moon-load-theme-hook #'moon/sync-nlinum-face)
  (add-hook 'moon-load-theme-hook #'moon/sync-nlinum-highlight-face)
  (setq nlinum-highlight-current-line t)
  :config
  (global-nlinum-mode)
  (moon/sync-nlinum-face)
  (moon/sync-nlinum-highlight-face))

(use-package| nlinum-relative
  :config
  (add-hook 'moon-load-theme-hook #'moon/sync-nlinum-relative-current-line-face)
  (add-hook 'nlinum-relative-mode-hook #'moon/sync-nlinum-relative-current-line-face)
  (when moon-enable-nlinum-relative
    (nlinum-relative-setup-evil)
    (global-nlinum-relative-mode 1)
    (moon/sync-nlinum-relative-current-line-face)
    ;; minimun delay makes sure
    ;; line number doesn't update when scrolling
    (setq nlinum-relative-redisplay-delay 0.1)))

;; absolute line number
(global-display-line-numbers-mode)

(use-package| nyan-mode
  :init (setq nyan-wavy-trail t))

(use-package| hl-todo
  :init (global-hl-todo-mode))

(defvar moon-enable-nlinum-relative nil
  "Whether to enable relative line number.")


(post-config| general
  (default-leader
    "tr" #'nlinum-mode ; toggle relative linum
    "tl" #'display-line-numbers-mode))

;; I don't show minor mode
;; in modeline anymore

;; (use-package| dim
;;   :after powerline
;;   :config
;;   (dim-minor-names
;;    '((eldoc-mode "" eldoc)
;;      (auto-revert-mode "" autorevert)
;;      (visual-line-mode "" simple)
;;      (evil-escape-mode "" evil-escape)
;;      (undo-tree-mode "" undo-tree)
;;      (which-key-mode "" which-key)
;;      (company-mode " Ⓒ" company)
;;      (flycheck-mode " ⓕ" flycheck)
;;      (ivy-mode " ⓘ" ivy)
;;      (lsp-mode " Ⓛ" lsp)
;;      (lispyville-mode " ⓟ" lispyville)
;;      (highlight-parentheses-mode "")
;;      (counsel-mode "" counsel)
;;      (flyspell-mode " Ⓢ" flyspell)
;;      ))
;;   )

(use-package| eyebrowse
  :config
  (eyebrowse-mode 1))

(post-config| general
  (default-leader
    "ww" #'delete-other-windows
    "w1" #'eyebrowse-switch-to-window-config-1
    "w2" #'eyebrowse-switch-to-window-config-2
    "w3" #'eyebrowse-switch-to-window-config-3
    "w4" #'eyebrowse-switch-to-window-config-4
    "w5" #'eyebrowse-switch-to-window-config-5
    "w6" #'eyebrowse-switch-to-window-config-6
    "wd" #'eyebrowse-close-window-config))


(use-package| winum
  :config (winum-mode 1))

(post-config| general
  (default-leader
    "1" #'moon/switch-to-window-1
    "2" #'moon/switch-to-window-2
    "3" #'moon/switch-to-window-3
    "4" #'moon/switch-to-window-4
    "5" #'moon/switch-to-window-5
    "6" #'moon/switch-to-window-6
    "7" #'moon/switch-to-window-7
    "8" #'moon/switch-to-window-8
    "9" #'moon/switch-to-window-9
    "wk1" #'moon/kill-buffer-in-window-1
    "wk2" #'moon/kill-buffer-in-window-2
    "wk3" #'moon/kill-buffer-in-window-3
    "wk4" #'moon/kill-buffer-in-window-4
    "wk5" #'moon/kill-buffer-in-window-5
    "wk6" #'moon/kill-buffer-in-window-6
    "wk7" #'moon/kill-buffer-in-window-7
    "wk8" #'moon/kill-buffer-in-window-8
    "wk9" #'moon/kill-buffer-in-window-9
    ))


(post-config| which-key
  ;; create a fake key to represent all ten winum function
  (push '(("\\(.*\\) 1" . "moon/switch-to-window-1") . ("\\1 1..9" . "window 1..9")) which-key-replacement-alist)
  ;; hide other keys
  (push '((nil . "moon/switch-to-window-[2-9]") . t) which-key-replacement-alist)

  ;; create a fake key to represent all ten eyebrowse function
  (push '(("\\(.*\\) 1" . "eyebrowse-switch-to-window-config-1") . ("\\1 1..9" . "workspace 1..9")) which-key-replacement-alist)
  ;; hide other keys
  (push '((nil . "eyebrowse-switch-to-window-config-[2-9]") . t) which-key-replacement-alist)

  ;; create a fake key to represent all ten eyebrowse function
  (push '(("\\(.*\\) 1" . "moon/kill-buffer-in-window-[1-9]") . ("\\1 1..9" . "kill buffer in window 1..9")) which-key-replacement-alist)
  ;; hide other keys
  (push '((nil . "moon/kill-buffer-in-window-[2-9]") . t) which-key-replacement-alist)
  )


;;
;; Desktop
;;

;; (add-hook 'moon-post-init-hook #'desktop-save-mode)

(post-config| general
  (default-leader
    "wr" #'moon/desktop-read))

(add-hook 'moon-post-init-hook #'moon-setup-save-session)

;; copied from
;; https://gist.github.com/syl20bnr/4425094
(defun moon-setup-save-session ()
  "Setup desktop-save-mode.

Don't bother me with annoying prompts when reading
and saveing desktop."
  ;; (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet

    (desktop-save-mode 1) ; activate desktop mode
    (setq desktop-save t) ; always save
    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)
    ;; Set the location to save/load default desktop
    (setq desktop-dirname moon-local-dir)

    ;; Make sure that even if emacs or OS crashed, emacs
    ;; still have last opened files.
    (add-hook 'find-file-hook
     (lambda ()
       (run-with-timer 5 nil
          (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save moon-local-dir)))))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook
              (lambda ()
                ;; Reset desktop modification time so the user is not bothered
                (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
    ;; )
)

;;
;; mission control
;;

(load (concat moon-star-dir "basic/ui/emacs-mission-control/mission-control"))
(setq mcon-c-tab-initial-selection-offset 1
      mcon-frame-name "mcon")
(mcon-c-tab-setup-binding)
(post-config| general
  (default-leader
    "bc" #'mcon-switch))


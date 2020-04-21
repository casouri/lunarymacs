;; -*- lexical-binding: t -*-

;;; Key

(with-eval-after-load 'luna-general-config
  (general-define-key
   "s-B" #'winner-undo
   "s-F" #'winner-redo

   "s-y" #'luna-toggle-console
   "s-Y" #'luna-toggle-console-window

   "s-K" #'buf-move-up
   "s-J" #'buf-move-down
   "s-H" #'buf-move-left
   "s-L" #'buf-move-right

   "s-h" #'windmove-left
   "s-j" #'windmove-down
   "s-k" #'windmove-up
   "s-l" #'windmove-right
   "s-s" #'save-buffer
   "s-w" #'delete-frame)

  (luna-default-leader
    "wr" #'luna-desktop-read
    "ww" #'windman-select-wilndow))

;;; Package

(load-package windman
  :commands windman-select-window)

(add-to-list 'luna-toggle-theme-list 'light)
(add-to-list 'luna-toggle-theme-list 'cyberpunk)
(setq custom-theme-directory
      (expand-file-name "site-lisp" user-emacs-directory))

(load-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config (with-current-buffer (get-buffer-create "*scratch*")
            (rainbow-delimiters-mode)))

(load-package rainbow-mode
  :commands rainbow-mode)

(load-package highlight-parentheses
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (setq hl-paren-colors
        (list (face-attribute
               'hl-paren-face :foreground)))
  (global-highlight-parentheses-mode)
  ;; highlight only the most inner pair
  (add-hook 'luna-load-theme-hook
            (lambda ()
              (setq hl-paren-colors
                    (list (face-attribute
                           'hl-paren-face :foreground)))
              (global-highlight-parentheses-mode -1)
              (global-highlight-parentheses-mode))))

(load-package nyan-lite
  :init (setq nyan-lite-add-mode-line nil
              nyan-lite-progress-bar nil
              nyan-lite-animate t)
  :commands nyan-lite-mode)

(load-package hl-todo
  :config
  (push 'org-mode hl-todo-text-modes)
  (push 'fundamental-mode hl-todo-text-modes)
  (setq hl-todo-keyword-faces ; override
        (append '(("FAIL" . "red1")
                  ("TOTEST" . "#d0bf8f")
                  ("UNSURE" . "#FE6266")
                  ("TRY" . "#FFF100")
                  ("GOOD" . "#52DEA1"))
                hl-todo-keyword-faces))
  (global-hl-todo-mode))

;; interfere with 'display text property
;;
;; (load-package form-feed
;;   :config (add-hook 'prog-mode-hook #'form-feed-mode))

;;;; Desktop, Windows & buffer

(load-package buffer-move
  :commands
  (buf-move-up
   buf-move-dowan
   buf-move-left
   buf-move-right))

(load-package eyebrowse
  :commands
  (eyebrowse-switch-to-window-config-1
   eyebrowse-switch-to-window-config-2
   eyebrowse-switch-to-window-config-3
   eyebrowse-switch-to-window-config-4
   eyebrowse-switch-to-window-config-5
   eyebrowse-switch-to-window-config-6)
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-x C-w"))
  :config
  (eyebrowse-mode)
  (setq eyebrowse-mode-line-separator " "))

(use-package console-buffer
  :commands (luna-toggle-console
             luna-toggle-console-window))

;;; Desktop resume

(add-hook 'after-init-hook #'luna-setup-save-session)

;; copied from
;; https://gist.github.com/syl20bnr/4425094
(defun luna-setup-save-session ()
  "Setup desktop-save-mode.

Don't bother me with annoying prompts when reading
and saveing desktop."
  ;; (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet

  (desktop-save-mode 1) ; activate desktop mode
  (setq desktop-save t) ; always save
  ;; The default desktop is loaded anyway if it is locked
  (setq desktop-load-locked-desktop t)
  ;; Set the location to save/load default desktop
  (setq desktop-dirname luna-cache-dir)

  ;; Make sure that even if emacs or OS crashed, emacs
  ;; still have last opened files.
  (add-hook 'find-file-hook
            (lambda ()
              (run-with-timer 5 nil
                              (lambda ()
                                ;; Reset desktop modification time so the user is not bothered
                                (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
                                (desktop-save luna-cache-dir)))))

  ;; Add a hook when emacs is closed to we reset the desktop
  ;; modification time (in this way the user does not get a warning
  ;; message about desktop modifications)
  (add-hook 'kill-emacs-hook
            (lambda ()
              ;; Reset desktop modification time so the user is not bothered
              (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
  ;; )
  )

;;; Misc functions

(defun chunyang-alpha-param-adjust ()
  "调节当前 Frame 的透明度."
  (interactive)
  (let ((alpha (or (frame-parameter nil 'alpha) 100)))
    (while (pcase (read-key (format "%d%%, use +,-,0 for further adjustment" alpha))
             ((or ?+ ?=) (setq alpha (1+ alpha)))
             (?- (setq alpha (1- alpha)))
             (?0 (setq alpha 100))
             (_  nil))
      (cond ((> alpha 100) (setq alpha 100))
            ((< alpha 0) (setq alpha 0)))
      (modify-frame-parameters nil `((alpha . ,alpha))))))


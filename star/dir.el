;; -*- lexical-binding: t -*-

;;; Key

(with-eval-after-load 'luna-general-config
  (general-define-key
   :keymaps 'dired-mode-map
   "b" #'dired-up-directory
   "q" #'luna-quit-window
   "C-c C-s" #'dired-narrow
   "C-c C-o" #'luna-dired-open-file-at-point))

;; (with-eval-after-load 'general
;;   (luna-default-leader
;;     "tn" #'neotree-toggle
;;     "tr" #'ranger
;;     "th" #'luna-toggle-hidden-file)
;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'neotree-mode-map
;;    "TAB" #'neotree-enter
;;    "SPC" #'neotree-quick-look
;;    "q" #'neotree-hide
;;    "RET" #'neotree-enter))

;;; Package

;; (load-package neotree
;;   :commands neotree-toggle
;;   :init
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;   (setq neo-smart-open t)
;;   (setq projectile-switch-project-action 'neotree-projectile-action)
;;   (setq neo-show-hidden-files t)
;;   :config
;;   (require 'all-the-icons))


;; (load-package ranger
;;   :init (setq ranger-show-hidden t
;;               ;; otherwise ranger binds ranger-key (C-p)
;;               ;; to `deer-from-dired' in dired mode
;;               ranger-key nil)
;;   :commands ranger)

(load-package dired-narrow
  :commands dired-narrow)

;;; Config

(add-hook 'dired-mode-hook #'auto-revert-mode)
(setq dired-dwim-target t)

;;; Function

(defun luna-dired-open-file-at-point ()
  (interactive)
  (if-let ((file (dired-file-name-at-point)))
      (shell-command (format "open %s" file))
    (message "Not file found at point")))


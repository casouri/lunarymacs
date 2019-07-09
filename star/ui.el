;;; -*- lexical-binding: t -*-

;;; Key

(luna-with-eval-after-load 'key.general
  (general-define-key
   "s-b" #'winner-undo
   "s-f" #'winner-redo

   "s-y" #'luna-toggle-console

   "s-K" #'buf-move-up
   "s-J" #'buf-move-down
   "s-H" #'buf-move-left
   "s-L" #'buf-move-right
   )
  (luna-default-leader
    "tb" #'awesome-tab-mode
    "wr" #'luna-desktop-read
    "ah" #'luna-highlight-symbol))

;;; Config

(global-hl-line-mode)

(add-hook 'prog-mode-hook #'hs-minor-mode)

;;;; Display buffer alist

(setq display-buffer-alist
      (append display-buffer-alist
              '(("^\\*tex-shell\\*$" . (luna-display-buffer-in-shell-window . nil))
                ("^\\*MATLAB\\*$" . (luna-display-buffer-in-shell-window . nil))
                ("^\\*sly-mrepl for sbcl\\*$" . (luna-display-buffer-in-shell-window . nil))
                )))

;;; Package

(load-package doom-themes
  :config
  ;; (add-to-list 'luna-toggle-theme-list 'doom-one)
  (setq doom-cyberpunk-bg 'light))

(add-to-list 'luna-toggle-theme-list 'doom-one-light t)
(add-to-list 'luna-toggle-theme-list 'doom-cyberpunk)


(load-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config (with-current-buffer (get-buffer-create "*scratch*")
            (rainbow-delimiters-mode)))

(load-package rainbow-mode
  :commands rainbow-mode)

(load-package highlight-parentheses
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (global-highlight-parentheses-mode)
  ;; highlight only the most inner pair
  (setq hl-paren-colors (lambda () (pcase luna-current-theme
                                     ('doom-cyberpunk '("green"))
                                     ('doom-one-light '("red"))
                                     (_ '("cyan")))))
  ;; to reflect changes in parentheses color
  (add-hook 'luna-load-theme-hook (lambda ()
                                    (global-highlight-parentheses-mode -1)
                                    (global-highlight-parentheses-mode))))

;;;; icons

;; (load-package icons-in-terminal
;;   :commands icons-in-terminal)

;;;; Mode-line

(load-package minions
  :config
  (minions-mode)
  (add-hook 'luna-load-theme-hook
            (lambda () (minions-mode -1) (minions-mode))))

(defun luna-edit-lighter ()
  (if (buffer-modified-p)
      "| "
    "  "))

(defun luna-root-lighter ()
  (if (equal user-login-name "root")
      "ROOT "
    ""))

(defun make-lighter (str empty-value empty-return)
  "Make a ligher for mode-line.

If STR equal to EMPTY-VALUE(nil, \"\"), return EMPTY-RETURN,
else return STR."
  (if (equal str empty-value)
      empty-return
    str))

(defvar luna-flymake-mode-line-map (let ((map (make-sparse-keymap)))
                                     (define-key map (vector 'mode-line
                                                             mouse-wheel-up-event) #'flymake-goto-prev-error)
                                     (define-key map (vector 'mode-line
                                                             mouse-wheel-down-event) #'flymake-goto-next-error)
                                     map))

(defun luna-dedicated-window-mode-line ()
  (if (window-dedicated-p)
      "LOCK"
    ""))

(defun luna-flymake-mode-line ()
  (require 'subr-x)
  (let* ((known (hash-table-keys flymake--backend-state))
         (running (flymake-running-backends))
         (disabled (flymake-disabled-backends))
         (reported (flymake-reporting-backends))
         (diags-by-type (make-hash-table))
         (all-disabled (and disabled (null running)))
         (some-waiting (cl-set-difference running reported)))
    (maphash (lambda (_b state)
               (mapc (lambda (diag)
                       (push diag
                             (gethash (flymake--diag-type diag)
                                      diags-by-type)))
                     (flymake--backend-state-diags state)))
             flymake--backend-state)
    (apply #'concat
           (mapcar (lambda (args)
                     (apply (lambda (num str face pad)
                              (concat (propertize
                                       (format str num)
                                       'face face
                                       'keymap luna-flymake-mode-line-map
                                       'help-echo (format "%d running backens\nScroll up/down: previous/next diagnose"
                                                          (length running)))
                                      (propertize pad 'face '(:foreground "gray"))))
                            args))
                   `((,(length (gethash :error diags-by-type)) "%d " error "|")
                     (,(length (gethash :warning diags-by-type)) " %d " warning "|")
                     (,(length (gethash :note diags-by-type)) " %d" success ""))))))

(defvar luna-many-space "    ")

(setq-default mode-line-format '((:eval (luna-root-lighter))
                                 (:eval (luna-dedicated-window-mode-line))
                                 ;; (:eval (luna-edit-lighter))
                                 vc-mode
                                 luna-many-space
                                 ;; (:eval (if (bound-and-true-p eyebrowse-mode) (eyebrowse-mode-line-indicator) ""))
                                 "%b"
                                 luna-many-space
                                 mode-line-modes
                                 luna-many-space
                                 (:eval (if (bound-and-true-p flymake-mode) (luna-flymake-mode-line) "OK"))
                                 luna-many-space
                                 ;; misc info beg
                                 mode-line-misc-info
                                 ;; misc info end
                                 luna-many-space
                                 "%I"
                                 luna-many-space
                                 (:eval (if (bound-and-true-p nyan-lite-mode) (nyan-lite-mode-line) "ฅ Φ ω Φ ฅ"))
                                 luna-many-space
                                 "%p"
                                 ;; "  %l:%c"
                                 ;; makes mode line higher
                                 (:eval (propertize " "
                                                    'display '(height 1.4)))
                                 ;; makes other text in the middle
                                 (:eval (propertize " " 'display '(raise -0.3)))
                                 mode-line-end-spaces))

;;;;
;;;; Misc

(load-package nyan-lite
  :init (setq nyan-lite-add-mode-line nil
              nyan-lite-progress-bar nil
              nyan-lite-animate t)
  :commands nyan-lite-mode)

;; (load-package zone-nyan
;;   :defer 5
;;   :config
;;   (require 'zone)
;;   ;; (setq zone-programs [zone-nyan])
;;   (zone-when-idle 120))

(load-package hl-todo
  :defer 5
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

;; form feed
(load-package form-feed
  :commands form-feed-mode
  :config
  (defface form-feed-line
    `((((type graphic)
        (background light)) :strike-through ,spacemacs-light-purple)
      (((type graphic)
        (background dark)) :strike-through ,doom-blue)
      (((type tty)) :inherit font-lock-comment-face :underline t))
    "Face for form-feed-mode lines."
    :group 'form-feed))


;;;;
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
  :config
  (eyebrowse-mode 1)
  ;; default was ", "
  (setq eyebrowse-mode-line-separator " "))



;;;; Desktop resume

(add-hook 'luna-startup-hook-1 #'luna-setup-save-session)

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



;;;; Tab
;;

;; don't load it, it has performance issues
;; (load-package awesome-tab
;;   :defer 2
;;   ;; to sync face
;;   ;; TODO maybe fix this
;;   :config (add-hook 'luna-load-theme-hook (lambda () (awesome-tab-mode -1) (awesome-tab-mode))))

;;;; Syntax
;;

(defun luna-highlight-symbol ()
  "Hightlight symbol at point."
  (interactive)
  (evil-ex-search-activate-highlight `(,(thing-at-point 'symbol) t t)))

;;;; VC

;; not autoloaded
(load-package diff-hl
  :config
  (unless window-system
    (diff-hl-margin-mode))
  (diff-hl-mode)
  (setq diff-hl-draw-borders nil)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; auto highlight

(defvar luna-auto-highlight nil
  "Wehther to highlight symbol at point after a delay.")

(defun luna-auto-highlight ()
  "Hightlight thing at point."
  (evil-ex-search-activate-highlight `(,(thing-at-point 'symbol) t t))
  (add-hook 'pre-command-hook #'luna-auto-highlight-hook))


(defun luna-auto-highlight-hook ()
  "Clean hightlight and remove self from `pre-command-hook'."
  (evil-ex-nohighlight)
  (remove-hook 'pre-command-hook #'luna-auto-highlight-hook))

(defvar luna-auto-highlight-timer nil
  "Idle timer of luna-auto-hightlight-mode.")

(define-minor-mode luna-auto-highlight-mode
  "Highlight symbol at point automatically after a delay."
  :global
  :lighter "AutoH"
  (if luna-auto-highlight-mode
      (setq luna-auto-highlight-timer (run-with-idle-timer 1 t #'luna-auto-highlight))
    (cancel-timer luna-auto-highlight-timer)))

;;; shell-window

(defvar luna-shell-window nil
  "Thw window at the bottom dedicated to temporary shell buffers.")

(defvar luna-shell-window-buffer nil
  "Used to recover shell window with last displayed buffer.")

(defun luna-toggle-shell-window (arg)
  "Toggle display of shell window at the bottom.
If called with ARG, maximize shell window."
  (interactive "p")
  (if (eq arg 4) ; C-u
      (luna-maximize-shell-window)
    (if (and luna-shell-window (window-live-p luna-shell-window))
        (delete-window luna-shell-window)
      (luna-display-buffer-in-shell-window
       (or luna-shell-window-buffer
           (get-buffer-create
            (completing-read
             "Choose a buffer: "
             (mapcar #'buffer-name (buffer-list)))))
       nil))))

(defun luna-maximize-shell-window ()
  "Make shell window take full screen.
To go back, use `winnner-undo'."
  (interactive)
  (when luna-shell-window-buffer
    (delete-other-windows)
    (switch-to-buffer luna-shell-window-buffer)))

(defun luna-display-buffer-in-shell-window (buffer alist)
  "Display BUFFER in `luna-shell-window'.
More on ALIST in `display-buffer-alist'."
  (setq luna-shell-window-buffer buffer)
  ;; (display-buffer-in-side-window buffer (append alist '((side . bottom) (window-height . 0.2))))
  (let ((win (display-buffer-at-bottom buffer (append alist '((window-height . 0.2))))))
    (set-window-dedicated-p win t))
  ;; set `luna-shell-window'
  (walk-windows (lambda (win) (when (eq (window-buffer win) buffer)
                                (setq luna-shell-window win)))
                'no))

;;; Console


(defvar-local luna-console-buffer-alist nil
  "An alist of (major-mode . console buffer).")

(defun luna-toggle-console ()
  "Toggle display of console buffer."
  (interactive)
  (let ((console-buffer (alist-get major-mode luna-console-buffer-alist)))
    (if (eq (current-buffer) console-buffer)
        (previous-buffer)
      (switch-to-buffer console-buffer))))

(defun luna-set-console-buffer (buffer)
  "Set current console buffer to BUFFER."
  (interactive "b")
  (setf (alist-get major-mode luna-console-buffer-alist)
        (get-buffer buffer)))

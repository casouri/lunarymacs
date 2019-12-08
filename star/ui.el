;;; -*- lexical-binding: t -*-

;;; Key

(luna-with-eval-after-load 'key.general
  (general-define-key
   "s-B" #'winner-undo
   "s-F" #'winner-redo

   "s-y" #'luna-toggle-console
   "s-Y" #'luna-toggle-console-window

   "s-K" #'buf-move-up
   "s-J" #'buf-move-down
   "s-H" #'buf-move-left
   "s-L" #'buf-move-right
   )
  (luna-default-leader
    "tb" #'awesome-tab-mode
    "wr" #'luna-desktop-read
    "ww" #'windman-select-window
    "ah" #'luna-highlight-symbol))

;;; Config

;; (global-hl-line-mode)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(winner-mode)

;;; Package

(load-package windman
  :commands windman-select-window)

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
  (defface hl-paren (let ((display t)
                          (attr '(:foreground "blue")))
                      (list (cons display attr)))
    "Foreground attribute is used for highlight paren highlight.")
  (setq hl-paren-colors
        (lambda () (list (face-attribute 'hl-paren :foreground))))
  ;; to reflect changes in parentheses color
  (add-hook 'luna-load-theme-hook
            (lambda ()
              (global-highlight-parentheses-mode -1)
              (global-highlight-parentheses-mode))))

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
  :commands form-feed-mode)


;;;;
;;;; Desktop, Windows & buffer

(load-package buffer-move
  :commands
  (buf-move-up
   buf-move-dowan
   buf-move-left
   buf-move-right))

(load-package eyebrowse
  :defer 3
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



;;;; Desktop resume

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

;;; Console-buffer


(defvar luna-console-buffer-alist '((emacs-lisp-mode . "*scratch*"))
  "An alist with element (major-mode . console buffer).")

(defvar-local luna-console-buffer-p nil
  "T if this buffer is a console buffer.")

(defun luna--get-console-buffer (major-mode)
  "Return the console buffer corresponding to MAJOR-MODE.
Return nil if none exists."
  (if-let ((console-buffer (alist-get major-mode luna-console-buffer-alist)))
      console-buffer
    (message "No console buffer, use `luna-set-console-buffer' to set one")
    nil))

(defun luna-toggle-console ()
  "Toggle display of console buffer.
When console window is live, jump between console window and previous window;
when console window is not live, switch between console buffer and previous buffer."
  (interactive)
  (if (window-live-p luna-console-window)
      ;; jump between console window and previous window
      (if luna-console-buffer-p
          (if-let ((win (window-parameter luna-console-window 'luna-console-jump-back)))
              (select-window win)
            (select-window (previous-window))
            (message "Could not find previous window, guess one"))
        (let ((old-window (selected-window)))
          (select-window luna-console-window)
          (set-window-parameter nil 'luna-console-jump-back old-window)))
    ;; switch between console buffer and previous buffer
    (if luna-console-buffer-p
        (previous-buffer)
      (switch-to-buffer (luna--get-console-buffer major-mode))
      (setq-local luna-console-buffer-p t))))

(defun luna-set-console-buffer (buffer)
  "Set current console buffer to BUFFER."
  (interactive "b")
  (setf (alist-get major-mode luna-console-buffer-alist)
        (get-buffer buffer)))


(defvar luna-console-window nil
  "A window at bottom dedicated to console buffer.")

(defun luna-toggle-console-window ()
  "Toggle display of console window."
  (interactive)
  (if (window-live-p luna-console-window)
      (delete-window luna-console-window)
    (when-let ((buf (luna--get-console-buffer major-mode)))
      (setq luna-console-window
            (display-buffer-at-bottom (get-buffer buf) '((window-height . 0.2)))))))

(luna-provide 'ui.console-buffer)

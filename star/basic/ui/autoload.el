;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/sync-nlinum-highlight-face ()
  (interactive)
  (set-face-attribute
   'nlinum-current-line nil
   :background (face-attribute 'hl-line :background)
   :foreground (face-attribute 'font-lock-keyword-face :foreground)
   :weight 'bold
   ))

;;;###autoload
(defun moon/sync-nlinum-relative-current-line-face ()
  (interactive)
  (set-face-attribute
   'nlinum-relative-current-face nil
   :background (face-attribute 'hl-line :background)
   :foreground (face-attribute 'font-lock-keyword-face :foreground)
   :weight 'bold
   ))

;;;###autoload
(defun moon/sync-nlinum-face ()
  (interactive)
  (set-face-background 'linum (face-attribute 'default :background) nil)
  )

;;;###autoload
(defun moon-switch-to-window (num)
  "Switch to sindow NUM, split window if not exist."
  (if (> num winum--window-count)
      (call-interactively #'moon/split-to)
    (winum-select-window-by-number num)))

;;;###autoload
(defun moon/split-to (direction)
  "Split window and switch to it according to DIRECTION.
Accept one character for DIRECTION.
  k     ^
h   l <   >
  j     v
"
  (interactive "cswitch to window: h/j/k/l")
  (pcase direction
    (?k (split-window-below))                 ; up
    (?j (select-window (split-window-below))) ; down
    (?h (split-window-right))                 ; left
    (?l (select-window (split-window-right))) ; right
    ))

;;;###autoload
(defun moon/switch-to-window-1 ()
  "Switch to window 1, split window if not exist."
  (interactive)
  (moon-switch-to-window 1))

;;;###autoload
(defun moon/switch-to-window-2()
  "Switch to window 2, split window if not exist."
  (interactive)
  (moon-switch-to-window 2))

;;;###autoload
(defun moon/switch-to-window-3 ()
  "Switch to window 3, split window if not exist."
  (interactive)
  (moon-switch-to-window 3))

;;;###autoload
(defun moon/switch-to-window-4 ()
  "Switch to window 4, split window if not exist."
  (interactive)
  (moon-switch-to-window 4))

;;;###autoload
(defun moon/switch-to-window-5()
  "Switch to window 5, split window if not exist."
  (interactive)
  (moon-switch-to-window 5))

;;;###autoload
(defun moon/switch-to-window-6 ()
  "Switch to window 6, split window if not exist."
  (interactive)
  (moon-switch-to-window 6))

;;;###autoload
(defun moon/switch-to-window-7 ()
  "Switch to window 7, split window if not exist."
  (interactive)
  (moon-switch-to-window 7))

;;;###autoload
(defun moon/switch-to-window-8 ()
  "Switch to window 8, split window if not exist."
  (interactive)
  (moon-switch-to-window 8))

;;;###autoload
(defun moon/switch-to-window-9 ()
  "Switch to window 9, split window if not exist."
  (interactive)
  (moon-switch-to-window 9))

;;;###autoload
(defun moon/desktop-read ()
  "Recover desktop, i.e. recover last session."
  (interactive)
  (desktop-read moon-local-dir))



;;;###autoload
(defun moon-kill-buffer-in-window (num)
  "Kill the buffer in window NUM."
  (let ((window (winum-get-window-by-number num)))
    (when window
      (let ((buffer (window-buffer window)))
        (when buffer
          (kill-buffer buffer))))))

;;;###autoload
(defun moon/kill-buffer-in-window-1 ()
  "Kill the buffer in window 1."
  (interactive)
  (moon-kill-buffer-in-window 1))

;;;###autoload
(defun moon/kill-buffer-in-window-2 ()
  "Kill the buffer in window 2."
  (interactive)
  (moon-kill-buffer-in-window 2))

;;;###autoload
(defun moon/kill-buffer-in-window-3 ()
  "Kill the buffer in window 3."
  (interactive)
  (moon-kill-buffer-in-window 3))

;;;###autoload
(defun moon/kill-buffer-in-window-4 ()
  "Kill the buffer in window 4."
  (interactive)
  (moon-kill-buffer-in-window 4))

;;;###autoload
(defun moon/kill-buffer-in-window-5 ()
  "Kill the buffer in window 5."
  (interactive)
  (moon-kill-buffer-in-window 5))

;;;###autoload
(defun moon/kill-buffer-in-window-6 ()
  "Kill the buffer in window 6."
  (interactive)
  (moon-kill-buffer-in-window 6))

;;;###autoload
(defun moon/kill-buffer-in-window-7 ()
  "Kill the buffer in window 7."
  (interactive)
  (moon-kill-buffer-in-window 7))

;;;###autoload
(defun moon/kill-buffer-in-window-8 ()
  "Kill the buffer in window 8."
  (interactive)
  (moon-kill-buffer-in-window 8))

;;;###autoload
(defun moon/kill-buffer-in-window-9 ()
  "Kill the buffer in window 9."
  (interactive)
  (moon-kill-buffer-in-window 9))


;;
;; Moody
;;

;;;###autoload
(defun flycheck-lighter (state bullet)
  "Return flycheck information for the given error type STATE."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
          (errorp (flycheck-has-current-errors-p state))
          (err (or (cdr (assq state counts)) "?"))
          (running (eq 'running flycheck-last-status-change)))
     (if (or errorp running) (format bullet err) "")))

;;;###autoload
(defun moon-edit-lighter ()
  (if (buffer-modified-p)
      "MOD "
    ""))

;;;###autoload
(defun moon-root-lighter ()
  (if (equal user-login-name "root")
      "ROOT "
    ""))

(defmacro make-lighter| (form empty-value empty-return)
  "Make a ligher for mode-line.

If FORM return EMPTY-VALUE(nil, \"\"), return EMPTY-RETURN,
else just return the form's return."
  `(let ((result ,form))
     (if (equal result ,empty-value)
         ,empty-return
       result)))

;;;###autoload
(defun moon/setup-moody ()
  "Setup mode-line using moody."
  (interactive)
  (let ((my-mode-line-format '(" "
                           (:eval (if winum-mode (winum-get-number-string) ""))
                           " "
                           (:eval (if eyebrowse-mode (eyebrowse-mode-line-indicator) ""))
                           " %I "
                           (:eval (moon-edit-lighter))
                           (:eval (moon-root-lighter))
                           ;; moody-mode-line-buffer-identification
                           (:eval (moody-tab "%b"))
                           " "
                           mode-line-modes
                           (:eval (moody-tab (make-lighter| (concat (flycheck-lighter 'error "☠%s")
                                                                    (flycheck-lighter 'warning "⚠%s")
                                                                    (flycheck-lighter 'info "𝌆%s")) "" "OK") nil 'up))
                           " "
                           (:eval (if nyan-mode (nyan-create) "%p"))
                           " "
                           ;; moody-vc-mode
                           (:eval (moody-tab (if vc-mode (substring-no-properties vc-mode 1) "NO VC")))
                           mode-line-misc-info
                           mode-line-end-spaces)))
    ;; change all current buffer's mode-line-format
    (save-excursion
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (setq mode-line-format my-mode-line-format)))
            (buffer-list)))
    ;; change default value of mode-line-format
    (setq-default mode-line-format my-mode-line-format)
    ))




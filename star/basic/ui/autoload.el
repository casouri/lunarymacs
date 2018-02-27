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
  (interactive "cswitch to window: h j k l")
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

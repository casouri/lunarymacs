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

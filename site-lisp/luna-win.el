;;; -*- lexical-binding: t; -*-

(require 'windmove)

;;; Key

(with-eval-after-load 'luna-general-config
  (luna-default-leader
    "w e" #'luna-win-expand-to
    "w 2" #'split-window-below
    "w 3" #'split-window-right
    "w d" #'delete-window))

;;; Functions

;;;; Backstage

(defun luna-win-do-window-in-direction (dir fn)
  "Apply FN to the window in direction DIR of the selected window.
Raise 'no-window-in-direction signal if no window is found."
  (let ((window (window-in-direction dir)))
    (if window
        (funcall fn window)
      (error "No window found in that direction"))))

(defun luna-win-do-window-in-direction-hjkl (hjkl fn)
  "See ‘luna-win-do-window-in-direction’."
  (luna-win-do-window-in-direction (luna-win-hjkl-to-dir hjkl) fn))

(defun luna-win-define-4-directions (fn-name docstring fn)
  (dolist (dir '(left right top bottom))
    (defun (intern (format "%s-%s" fn-name (symbol-name dir))) ()
      (format docstring dir)
      (interactive)
      (luna-win-do-window-in-direction dir fn))))

(defun luna-win-hjkl-to-dir (char)
  "Map CHAR (h/j/k/l) to symbol ('left/'bottom/'top/'right). "
  (let ((map-alist '((?h . left)
                     (?j . below)
                     (?k . above)
                     (?l . right))))
    (or (alist-get char map-alist)
        (error "CHAR has to be one of characters h/j/k/l"))))

;;;; Userland

;; (luna-win-define-4-directions
;;  "luna-win-expand-to" "Expand window to %s." #'delete-window)

(defun luna-win-expand-to (dir)
  (interactive "ch/j/k/l: ")
  (luna-win-do-window-in-direction-hjkl dir #'delete-window))

;;; luna-win.el ends here

;;;###autoload
(defun moon/switch-theme ()
  (interactive)
  (defvar moon-toggle-theme-list '(spacemacs-dark spacemacs-light))
  ;; move the fist element to last
  (add-to-list 'moon-toggle-theme-list (pop moon-toggle-theme-list) t)
  (load-theme (car moon-toggle-theme-list)))

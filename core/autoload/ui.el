;;;###autoload
(defun moon/switch-theme ()
  "Switch between themes in `moon-toggle-theme-list'"
  (interactive)
  ;; move the fist element to last
  (add-to-list 'moon-toggle-theme-list (pop moon-toggle-theme-list) t)
  (moon-load-theme (car moon-toggle-theme-list) t))

;;;###autoload
(defun moon/load-font ()
  "Prompt for a font and set it.
Fonts are specified in `moon-magic-font-book'.
Each element is an alist with the form
(name . (moon-set-font| configuration))
 (name . (moon-set-font| :family \"family\" :weight ’weight))"
  (interactive)
  (let ((font-name (completing-read "Font to load: " moon-magic-font-book)))
    (eval (cdr (assoc font-name moon-magic-font-book))) nil t
    ))

;;;###autoload
(defmacro moon-set-font| (&rest config-list)
  "Set font. Accepts `font-spec' arguments.

e.g. :family :weight :size etc."
  `(set-frame-font (font-spec ,@config-list) nil t))


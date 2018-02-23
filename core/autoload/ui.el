;;;###autoload
(defun moon/switch-theme ()
  "Switch between themes in `moon-toggle-theme-list'"
  (interactive)
  (defvar moon-toggle-theme-list '(spacemacs-dark spacemacs-light))
  ;; move the fist element to last
  (add-to-list 'moon-toggle-theme-list (pop moon-toggle-theme-list) t)
  (load-theme (car moon-toggle-theme-list)))

;;;###autoload
(defun moon/load-font ()
  "Prompt for a font and set it."
  (interactive)
  (let ((font-name (completing-read "Font to use: " moon-magic-font-book)))
    (eval (cdr (assoc font-name moon-magic-font-book))) nil t
    ))

;;;###autoload
(defmacro moon-set-font| (&rest config-list)
  "Set font. Accepts `font-spec' arguments.

e.g. :family :weight :size etc."
  `(set-frame-font (font-spec ,@config-list) nil t))

;;;###autoload
(defmacro change-cursor-on-hook| (hook color)
  "Change cursor color to COLOR when HOOK is activated.

COLOR is a function that returns a string that specify a color."
  `(add-hook ',hook
             (lambda ()
               (set-face-attribute
                'cursor nil :background (,color)))))

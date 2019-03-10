(require 'cl-lib)

;;;###autoload
(defun moon/switch-theme ()
  "Switch between themes in `moon-toggle-theme-list'"
  (interactive)
  ;; move the fist element to last
  (let ((index (or (cl-position moon-current-theme moon-toggle-theme-list)
                   (progn (error "`moon-current-theme' is not in `moon-toggle-theme-list'") 0)))
        (len (length moon-toggle-theme-list)))
    (moon-load-theme (nth (% (1+ index) len) moon-toggle-theme-list) t)))

;;;###autoload
(defmacro moon-set-font| (&rest config-list)
  "Set font. Accepts `font-spec' arguments.

e.g. :family :weight :size etc."
  `(set-frame-font (font-spec ,@config-list) nil t))


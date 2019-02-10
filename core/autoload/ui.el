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
(defun moon/load-font (font-name)
  "Prompt for a font and set it.
Fonts are specified in `moon-font-alist'."
  (interactive (list
                (completing-read "Choose a font: "
                                 (mapcar (lambda (cons) (symbol-name (car cons)))
                                         moon-font-alist))))
  (set-frame-font (apply #'font-spec
                         (alist-get (intern font-name)
                                    moon-font-alist))))

(defun moon/load-cjk-font (font-name)
  "Prompt for a font and set it.
Fonts are specified in `moon-font-alist'."
  (interactive (list
                (completing-read "Choose a font: "
                                 (mapcar (lambda (cons) (symbol-name (car cons)))
                                         moon-cjk-font-alist))))
  (let ((spec (alist-get (intern font-name)
                         moon-cjk-font-alist)))
    (dolist (charset '(kana han cjk-misc))
      (set-fontset-font (frame-parameter nil 'font)
                        charset (apply #'font-spec
                                       spec)))
    (add-to-list 'face-font-rescale-alist
                 (cons (plist-get spec :family) 1.3))))

;;;###autoload
(defmacro moon-set-font| (&rest config-list)
  "Set font. Accepts `font-spec' arguments.

e.g. :family :weight :size etc."
  `(set-frame-font (font-spec ,@config-list) nil t))


;;; Global map

(define-key tool-bar-map [new-file]
  `(menu-item "Visit New File..." find-file
              :enable (menu-bar-non-minibuffer-window-p)
              :help "Specify a new file's name, to edit the file"
              :image ,(find-image '((:type png :file "new-file.png")))))

(define-key tool-bar-map [open-file]
  `(menu-item "Open File..." menu-find-file-existing
              :enable (menu-bar-non-minibuffer-window-p)
              :help "Read an existing file into an Emacs buffer"
              :image ,(find-image '((:type png :file "open-file.png")))))

(define-key tool-bar-map [dired]
  `(menu-item "Open Directory..." dired
              :enable (menu-bar-non-minibuffer-window-p)
              :help "Read a directory, to operate on its files"
              :image ,(find-image '((:type png :file "dired.png")))))

(define-key tool-bar-map [kill-buffer]
  `(menu-item "Close" kill-this-buffer
              :enable (kill-this-buffer-enabled-p)
              :help "Discard (kill) current buffer"
              :image ,(find-image '((:type png :file "kill-buffer.png")))))
(define-key tool-bar-map [save-buffer]
  `(menu-item "Save" save-buffer
              :enable (and (buffer-modified-p)
                           (buffer-file-name)
                           (menu-bar-non-minibuffer-window-p))
              :help "Save current buffer to its file"
              :image ,(find-image '((:type png :file "save-buffer.png")))))

(define-key tool-bar-map [undo]
  `(menu-item "Undo" undo
              :enable (and (not buffer-read-only)
                           (not (eq t buffer-undo-list))
                           (if (eq last-command 'undo)
                               (listp pending-undo-list)
                             (consp buffer-undo-list)))
              :help "Undo last edits"
              :image ,(find-image '((:type png :file "undo.png")))))

(define-key tool-bar-map [cut]
  `(menu-item "Cut" kill-region
              :enable (and mark-active (not buffer-read-only))
              :help "Cut (kill) text in region between mark and current position"
              :image ,(find-image '((:type png :file "cut.png")))))

(define-key tool-bar-map [copy]
  `(menu-item "Copy" ns-copy-including-secondary
              :enable mark-active
              :help "Copy text in region between mark and current position"
              :image ,(find-image '((:type png :file "copy.png")))))

(define-key tool-bar-map [paste]
  `(menu-item "Paste" yank
              :enable t
              :help "Paste (yank) text most recently cut/copied"
              :image ,(find-image '((:type png :file "paste.png")))))

(define-key tool-bar-map [isearch-forward]
  `(menu-item "Forward String..." isearch-forward
              :enable t
              :help "Search forward for a string as you type it"
              :image ,(find-image '((:type png :file "isearch-forward.png")))))

;;; Isearch

(define-key isearch-tool-bar-map [isearch-repeat-backward]
  `(menu-item "Repeat backward" isearch-repeat-backward
              :help "Repeat search backward"
              :image ,(find-image '((:type png :file "isearch-repeat-backward.png")
                                    (:type png :file "left-arrow.png")))))
(define-key isearch-tool-bar-map [isearch-repeat-forward]
  `(menu-item "Repeat forward" isearch-repeat-forward
              :help "Repeat search forward"
              :image ,(find-image '((:type png :file "isearch-repeat-forward.png")
                                    (:type png :file "right-arrow.png")))))

(define-key isearch-tool-bar-map [isearch-cancel]
  `(menu-item "Abort" isearch-cancel
              :help "Abort search"
              :image ,(find-image '((:type png :file "isearch-cancel.png")
                                    (:type png :file "kill-buffer.png")))))

(define-key isearch-tool-bar-map [isearch-exit]
  `(menu-item "Finish" isearch-exit
              :help "Finish search leaving point where it is"
              :image ,(find-image '((:type png :file "isearch-exit.png")
                                    (:type png :file "finish.png")))))

(define-key isearch-tool-bar-map [isearch-delete-char]
  `(menu-item "Undo" isearch-delete-char
              :help "Undo last input item"
              :image ,(find-image '((:type png :file "isearch-delete-char.png")
                                    (:type png :file "undo.png")))))

(define-key isearch-tool-bar-map [isearch-query-replace]
  `(menu-item "Replace" isearch-query-replace
              :help "Replace search string"
              :image ,(find-image '((:type png :file "isearch-query-replace.png")
                                    (:type png :file "search-replace.png")))))

(define-key isearch-tool-bar-map [isearch-occur]
  `(menu-item "Replace" isearch-occur
              :help "Show each search hit"
              :image ,(find-image '((:type png :file "isearch-occur.png")
                                    (:type png :file "show.png")))))

(define-key isearch-tool-bar-map [isearch-describe-mode]
  `(menu-item "Replace" isearch-describe-mode
              :help "Get help for Isearch"
              :image ,(find-image '((:type png :file "isearch-describe-mode.png")
                                    (:type png :file "help.png")))))


;;; Org Mode

(define-key org-mode-map [tool-bar org-toggle-inline-images]
  `(menu-item "Show image" org-toggle-inline-images
              :help "Toggle inline image display"
              :image ,(find-image '((:type png :file "org-toggle-inline-images.png")
                                    (:type png :file "image.png")))))

;;; Custom

;; (define-key tool-bar-map [luna-switch-theme]
;;   `(menu-item "Toggle theme" luna-switch-theme
;;               :help "Switch between themes"
;;               :image ,(find-image '((:type png :file "luna-switch-theme.png")
;;                                     (:type png :file "color.png")))))

(define-key tool-bar-map [read-only-mode]
  `(menu-item "Read-only" read-only-mode
              :help "Toggle buffer read-only"
              :image ,(find-image '((:type png :file "read-only-mode.png")
                                    (:type png :file "lock.png")))))

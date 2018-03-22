;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/sync-peek-face ()
  (interactive)
  (set-face-attribute 'lsp-ui-peek-list nil :background (face-attribute 'hl-line :background))
  (set-face-attribute 'lsp-ui-peek-peek nil :background (face-attribute 'hl-line :background))
  (set-face-attribute 'lsp-ui-peek-selection nil :background (face-attribute 'highlight :background) :foreground (face-attribute 'default :foreground))
  (set-face-attribute 'lsp-ui-peek-filename nil :foreground (face-attribute 'font-lock-constant-face :foreground))
  (set-face-attribute 'lsp-ui-peek-highlight nil :background (face-attribute 'highlight :background) :foreground (face-attribute 'highlight :foreground) :distant-foreground (face-attribute 'highlight :foreground))
  (set-face-attribute 'lsp-ui-peek-header nil :background (face-attribute 'highlight :background) :foreground (face-attribute 'default :foreground))
)

;;;###autoload
(defun moon/smart-toggle-lsp-ui ()
  "Decide if turn on lsp-ui-doc and sideline
depending on the width of the current window.
Should be added to window adjust hook and lsp-ui-mode hook.

If `moon-smart-toggle-lsp-ui' is nil, don't do anything.

If called interactively, `moon-smart-toggle-lsp-ui'
will be set to t."
  (interactive)
  (when lsp-ui-mode
   (when (called-interactively-p 'any)
     (setq moon-smart-toggle-lsp-ui t))
   (when moon-smart-toggle-lsp-ui
     (let ((onoff (>= (window-width) moon-smart-toggle-threshold)))
       (lsp-ui-sideline-enable onoff)
       (lsp-ui-doc-enable onoff)))))

;;;###autoload
(defun moon-force-lsp-ui (&rest rest)
  "Disable smart-toggle-lsp-ui."
  (interactive)
  (when (called-interactively-p 'any)
    (setq moon-smart-toggle-lsp-ui nil)))


;;;###autoload
(define-minor-mode lsp-format-on-save-mode
  "Calls `lsp-format-buffer' on save."
  (add-hook 'before-save-hook #'lsp-format-buffer t))

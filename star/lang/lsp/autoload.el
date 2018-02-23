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
(defun moon/smart-toggle-lsp-ui (&optional enable)
  "Decide if turn on lsp-ui-doc and sideline
depending on the width of the current window.
Should be added to window adjust hook and lsp-ui-mode hook.

If `moon-smart-toggle-lsp-ui' is nil, don't do anything.

If ENABLE exists, set `moon-mart-toggle-lsp-ui' to ENABLE
before toggling."
  (when (boundp enable)
    (setq moon-smart-toggle-lsp-ui enable))
  (when moon-smart-toggle-lsp-ui
    (let ((onoff (>= (window-width) 120)))
      (lsp-ui-sideline-enable onoff)
      (lsp-ui-doc-enable onoff))))

;;; json-fold.el --- JSON folding  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'treesit)

;;; Folding & unfolding

(defconst json-fold--invisibility-spec '(json-fold . t)
  "The spec for ‘buffer-invisibility-spec’.")

(defun json-fold--thing-at-point (thing)
  "Fold THING at point.

THING can be a tree-sitter thing in ‘treesit-thing-settings’, or a thing
predicate (explained in the docstring of ‘treesit-thing-settings’)."
  (unless (memq json-fold--invisibility-spec
                buffer-invisibility-spec)
    (push json-fold--invisibility-spec buffer-invisibility-spec))
  
  (let ((node (treesit-thing-at-point thing 'nested)))
    (if (not node)
        (user-error "Nothing to fold at point")
      (let ((ov (make-overlay (1+ (treesit-node-start node))
                              (1- (treesit-node-end node))
                              nil t nil)))
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'invisible 'json-fold)))))

(defun json-fold--unfold-at-point ()
  "Unfold the fold at point."
  (dolist (ov (overlays-at (point)))
    (when (eq (overlay-get ov 'invisible) 'json-fold)
      (delete-overlay ov))))

(defun json-fold-toggle-at-point ()
  "Fold the object or array at point."
  (interactive)
  (if (eq (get-char-property (point) 'invisible) 'json-fold)
      (json-fold--unfold-at-point)
    (json-fold--thing-at-point '(or "object" "array"))))

;;; Buttons

(defconst json-fold--bracket-query
  (treesit-query-compile 'json '("{" @brace "[" @bracket))
  "Query that captures opening braces and brackets.")

(defface json-fold-button
  '((t . (:underline t)))
  "Face for the toggle button applied to braces and brackets.")

(defun json-fold-toggle-on-click (event)
  "Toggle folding at the point of click.

EVENT is the mouse down event."
  (interactive "e")
  (let* ((posn (event-end event))
         (pos (posn-point posn)))
    (goto-char (1+ pos))
    (json-fold-toggle-at-point)
    (goto-char pos)))

(defvar-keymap json-fold-button-map
  :doc "Keymap for the toggle button."
  "<down-mouse-1>" #'json-fold-toggle-on-click)

(defun json-fold--fontify-region (beg end)
  "Add fold toggling buttons between BEG and END.

Add buttons onto opening braces and brackets."
  (let ((nodes (treesit-query-capture
                (treesit-buffer-root-node 'json)
                json-fold--bracket-query beg end t)))
    ;; We use overlay directly, instead of ‘make-button’, because we
    ;; don’t want the normal button keymap, which activates the button
    ;; on RET. We want to only activate the button on mouse clicks.
    (dolist (node nodes)
      (let ((ov (make-overlay (treesit-node-start node)
                              (treesit-node-end node)
                              nil t nil)))
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'keymap json-fold-button-map)
        (overlay-put ov 'face 'json-fold-button)
        (overlay-put ov 'mouse-face 'highlight))))
  `(jit-lock-bounds ,beg . ,end))

;;; Minor mode

(define-minor-mode json-fold-minor-mode
  "Minor mode that makes opening brackets and braces into toggle buttons."
  :global nil
  :lighter ""
  (if json-fold-minor-mode
      (progn
        (add-hook 'jit-lock-functions #'json-fold--fontify-region 0 t)
        (jit-lock-refontify))
    (remove-hook 'jit-lock-functions #'json-fold--fontify-region t)))


(provide 'json-fold)

;;; json-fold.el ends here

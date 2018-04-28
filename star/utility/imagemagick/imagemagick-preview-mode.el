(defvar imagemagick-preview-mode-outfile nil
  "The output file that imagemagick-preview-mode shows.")

(defvar imagemagick-preview-mode-window nil
  "Window of preview image.")

(defvar imagemagick-preview-mode-buffer nil
  "Buffer of preview image.")

(defun imagemagic-preview-mode-set-outfile ()
  "Set `imagemagick-preview-mode-outfile' to region."
  (interactive)
  (setq imagemagick-preview-mode-outfile
        (buffer-substring-no-properties
         (region-beginning) (region-end))))


(define-minor-mode imagemagick-preview-mode
  "Preview your imagemagick script."
  :lighter "IMGPRE"
  (add-hook 'kill-buffer-hook (lambda () (remove-hook 'after-save-hook #'imagemagick-preview-mode-update)) nil t)
  (if imagemagick-preview-mode
      (add-hook 'after-save-hook #'imagemagick-preview-mode-update nil t)
    (remove-hook 'after-save-hook #'imagemagick-preview-mode-update t)))


(defun imagemagick-preview-mode-update ()
  "Update preview image."
  (interactive)
  (when imagemagick-preview-mode-outfile
    (shell-command (concat "chmod +x " (buffer-file-name)))
    (shell-command buffer-file-name)
    (let ((origin-window (selected-window)))
      (if (and imagemagick-preview-mode-window (window-live-p imagemagick-preview-mode-window))
          (select-window imagemagick-preview-mode-window)
        (select-window (setq imagemagick-preview-mode-window (split-window-right))))
      (if (and imagemagick-preview-mode-buffer (buffer-live-p imagemagick-preview-mode-buffer))
          (progn
            (switch-to-buffer imagemagick-preview-mode-buffer)
            (revert-buffer t t))
        (find-file imagemagick-preview-mode-outfile)
        (setq imagemagick-preview-mode-buffer (current-buffer)))
      (auto-revert-mode 1)
      (select-window origin-window))))





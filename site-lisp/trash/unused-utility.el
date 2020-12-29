;;; unused-utility.el --- Utilities that aren't useful anymore      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

;;; Buffer

(defun luna-find-file (&optional arg)
  "Find file. If called with ARG, find file in project."
  (interactive "p")
  (call-interactively
   (if (eq arg 4)
       #'project-find-file
     #'find-file)))

;;; Toggle dash

(defvar dash-underscore-mode-map (make-sparse-keymap))

(define-minor-mode dash-underscore-mode
  "Remaps “-” to “_”."
  :lighter " (-_)"
  :keymap 'dash-underscore-mode-map
  (if dash-underscore-mode
      ;; not sure how does remap works for swapping
      (progn  (define-key dash-underscore-mode-map "-"
                (lambda () (interactive) (insert "_")))
              (define-key dash-underscore-mode-map "_"
                (lambda () (interactive) (insert "-"))))
    (setq dash-underscore-mode-map (make-sparse-keymap))))

;;; Customize

(defun kill-emacs-no-save-customize ()
  "Kill Emacs and don’t save customization."
  (interactive)
  (remove-hook 'kill-emacs-hook #'customize-save-customized)
  (save-buffers-kill-emacs))

;;; Variable pitch font in code

(define-minor-mode global-variable-prog-mode
  "Global ‘variable-prog-mode’."
  :lighter ""
  :global t
  (if global-variable-prog-mode
      (progn (dolist (buf (buffer-list))
               (with-current-buffer buf
                 (when (derived-mode-p 'prog-mode)
                   (variable-prog-mode))))
             (add-hook 'prog-mode-hook #'variable-prog-mode))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'prog-mode)
          (variable-prog-mode -1))))
    (remove-hook 'prog-mode-hook #'variable-prog-mode)))

(define-minor-mode variable-prog-mode
  "Variable-pitch font in code."
  :lighter ""
  (if variable-prog-mode
      (progn (variable-pitch-mode)
             (font-lock-add-keywords nil '(("^ *" . 'fixed-pitch)
                                           ("[()'\"]" . 'fixed-pitch))))
    (variable-pitch-mode -1)
    (font-lock-remove-keywords nil '(("^ *" . 'fixed-pitch)
                                     ("[()'\"]" . 'fixed-pitch))))
  (font-lock-mode -1)
  (font-lock-mode))

;;; Screenshot

(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;;; Finder

(defvar finder--window-config nil
  "Window configuration for finder.")

(defvar finder--frame nil
  "Frame for finder.")

(defun finder-toggle ()
  "Open a dired frame."
  (interactive)
  (if (eq (selected-frame) finder--frame)
      ;; Disable.
      (progn
        (setq finder--window-config
              (window-state-get (frame-root-window)))
        (delete-frame finder--frame))
    ;; Enable.
    (unless (frame-live-p finder--frame)
      (setq finder--frame
            (make-frame)))
    (select-frame finder--frame)
    (if finder--window-config
        (window-state-put
         finder--window-config (frame-root-window))
      (find-file "~/"))))



(provide 'unused-utility)

;;; unused-utility.el ends here

;; 			E M A C S
;; -------------------------------------------------------
;; Welcome to GNU Emacs 28.0.50. Today is Wednesday 2020.4.22.
;;


(defvar-local selected-files nil)

(define-derived-mode select-file-mode special-mode
  "Select"
  (define-key select-file-mode-map (kbd "m")
    (lambda () (interactive)
      (let ((buffer-read-only nil))
        (push (buffer-substring
               (line-beginning-position)
               (line-end-position))
              selected-files)
        (put-text-property
         (line-beginning-position)
         (line-end-position)
         'face 'highlight))
      (forward-line)))
  (define-key select-file-mode-map (kbd "u")
    (lambda () (interactive)
      (let ((buffer-read-only nil))
        (setq selected-files
              (remove (buffer-substring
                       (line-beginning-position)
                       (line-end-position))
                      selected-files))
        (put-text-property
         (line-beginning-position)
         (line-end-position)
         'face nil))
      (forward-line)))
  (define-key select-file-mode-map (kbd "x")
    (lambda () (interactive)
      (let ((file-list selected-files))
        (kill-buffer "*select file*")
        (dolist (file file-list)
          (find-file file)))))
  (define-key select-file-mode-map (kbd "n") (kbd "C-n"))
  (define-key select-file-mode-map (kbd "p") (kbd "C-p")))

(defun select-files-open (dir)
  (interactive "DDirectory: ")
  (let ((file-list (directory-files dir t)))
    (switch-to-buffer "*select file*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (dolist (file file-list)
      (insert file "\n"))
    (goto-char (point-min))
    (setq selected-files nil)
    (select-file-mode)))

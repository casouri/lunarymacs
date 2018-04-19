;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/override-vim-binding ()
  "Replace some Vim keybindings.

Dependency:
- avy"
  (interactive)
  (define-key evil-normal-state-map "s"         #'avy-goto-char-timer)
  (define-key evil-normal-state-map "H"         #'evil-beginning-of-line)
  (define-key evil-normal-state-map "L"         #'evil-end-of-line)
  (define-key evil-normal-state-map "P"         #'evil-paste-from-register)
  (define-key evil-normal-state-map "U"         #'undo-tree-redo)
  (define-key evil-insert-state-map (kbd "C-a") #'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") #'evil-end-of-line)
  )

;;;###autoload
(defun moon/override-emacs-binding ()
  "Replace some vanilla Emacs bindings."
  (interactive)
  (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
  )


;;;###autoload
(defun moon/query-replace-region ()
  "Query replace selected region."
  (interactive)
  (evil-visual-state -1)
  (query-replace-regexp (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))
                 (completing-read "Replace to: " ())
                 ))

;;;###autoload
(defun moon/query-relace-point ()
  "Query replace thing at point."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (query-replace-regexp word
                   (completing-read (format "Replace \"%s\" to: " word) ())
                   nil (beginning-of-line))))

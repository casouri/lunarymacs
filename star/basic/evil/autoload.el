;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/override-vim-binding ()
  "Replace some Vim keybindings.

Dependency:
- avy"
  (interactive)
  (define-key evil-normal-state-map "s" #'avy-goto-char-timer)
  (define-key evil-insert-state-map "C-a" #'evil-beginning-of-line)
  (define-key evil-insert-state-map "C-e" #'evil-end-of-line)
  )

(defun moon/override-emacs-binding ()
  "Replace some vanilla Emacs bindings."
  (interactive)
  (define-key evil-normal-state-map "C-u" #'evil-scroll-up)
  (define-key evil-insert-state-map "C-u" #'evil-shift-right-line)
  )


;;;###autoload
(defun moon/query-replace-region ()
  "Query replace selected region."
  (interactive)
  (query-replace (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))
                 (completing-read "Replace to: " ())
                 ))

;;;###autoload
(defun moon/query-relace-point ()
  "Query replace thing at point."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (query-replace word
                   (completing-read (format "Replace \"%s\" to: " word) ())
                   nil (beginning-of-line))))

;;; -*- lexical-binding: t -*-


(require 'ein)
(require 'evil)

(evil-define-keymap evil-jpnb-state-map
  "Keymap for jupyter/ipython state.")

(evil-define-state jpnb
  "Jupyter/iPython notebook state that act on cells."
  :tag "<P>"
  :message "-- NOTEBOOK --"
  :enable (motion)
  )

(define-key evil-jpnb-state-map "i" #'evil-normal-state)
(define-key evil-jpnb-state-map "j" #'ein:worksheet-goto-next-input)
(define-key evil-jpnb-state-map "k" #'ein:worksheet-goto-prev-input)
(define-key evil-jpnb-state-map "J" #'ein:worksheet-move-cell-down)
(define-key evil-jpnb-state-map "K" #'ein:worksheet-move-cell-up)
(define-key evil-jpnb-state-map "<s-up>" #'ein:worksheet-move-cell-up)
(define-key evil-jpnb-state-map "<s-down>" #'ein:worksheet-move-cell-down)
(define-key evil-jpnb-state-map "b" #'ein:worksheet-toggle-output)
(define-key evil-jpnb-state-map "t" #'ein:worksheet-toggle-cell-type)
(define-key evil-jpnb-state-map "dd" #'ein:worksheet-kill-cell)
(define-key evil-jpnb-state-map "o" #'ein:worksheet-insert-cell-below)
(define-key evil-jpnb-state-map "O" #'ein:worksheet-insert-cell-above)
(define-key evil-jpnb-state-map "S-return" #'ein:worksheet-execute-cell-and-goto-next)
(define-key evil-jpnb-state-map "e" #'ein:worksheet-execute-cell-and-goto-next)
(define-key evil-jpnb-state-map "C-l" #'ein:worksheet-clear-output)
(define-key evil-jpnb-state-map "y" #'ein:worksheet-copy-cell)
(define-key evil-jpnb-state-map "p" #'ein:worksheet-yank-cell)
(define-key evil-jpnb-state-map "s" #'ein:worksheet-split-cell-at-point)
(define-key evil-jpnb-state-map "m" #'ein:worksheet-merge-cell)
(define-key evil-jpnb-state-map "zz" #'evil-scroll-line-to-center)

(provide 'jpnb-state)

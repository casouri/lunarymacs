;;; -*- lexical-binding: t -*-

(use-package| nyan-mode
  :init(setq nyan-wavy-trail t)
  :hook (prog-mode . nyan-mode)
  :config
  (nyan-start-animation)
  )

(post-config| general
  (general-define-key
   :states 'insert
   "<C-ret>" #'moon/jump-newline-below
   "<C-S-ret>" #'moon/jump-newline-above
   "C-;" #'moon/insert-semi-at-eol
   "<C-ret>" #'moon/jump-newline-below
   )
  (general-define-key
   :states 'normal
   "J" #'moon/scroll-down-reserve-point
   "K" #'moon/scroll-up-reserve-point)) 

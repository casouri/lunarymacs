;; -*- lexical-binding: t; -*-

(require 'luna-f)

;;; Note
;;
;; In ivy search session:
;;
;; C-c C-o ivy-occur : port current search to a new buffer like occur
;; M-o : additional actions
;;
;; In global map:
;; M-y : counsel-yank-pop (list kill-ring)


;;; Key

(luna-def-key
 "M-y" #'counsel-yank-pop
 :leader
 "ss" #'swiper)

;;; Package

(load-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ;; Use full path for virtual buffer.
        ivy-virtual-abbreviate 'full)
  (ivy-mode))

(load-package swiper
  :commands swiper)

(load-package counsel
  :config (counsel-mode))

;; For M-x history.
(load-package smex
  :defer)

;; ivy-prescient doesn’t support ivy anymore.

;; Use the forked version in site-lisp.
(load-package recentf-ext
  :config (recentf-mode))

(load-package ivy-xref
  :config
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


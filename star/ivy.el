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
  (ivy-mode)
  ;; Ivy uses this function to add current selection face over
  ;; candidates, it tries to blend colors together, which looks bad.
  (defun luna-ivy-add-face (str face)
    "Simply override STR’s face with FACE."
    (propertize str 'face face))
  (advice-add #'ivy--add-face :override #'luna-ivy-add-face))

(load-package swiper
  :commands swiper)

(load-package counsel
  :config
  (counsel-mode)
  (push '(counsel-M-x . nil) ivy-initial-inputs-alist))

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


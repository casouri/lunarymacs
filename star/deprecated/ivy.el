;; -*- lexical-binding: t; -*-

;;; Note
;;
;; In ivy search session:
;;
;; C-c C-o ivy-occur : port current search to a new buffer like occur
;; M-o : additional actions
;;
;; In global map:
;; M-y : counsel-yank-pop (list kill-ring)
;; C-o : counsel-mark-ring

;;; Key

(luna-def-key
 "M-y" #'counsel-yank-pop
 "C-x 8 RET" #'counsel-unicode-char
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
  (push '(counsel-M-x . nil) ivy-initial-inputs-alist)
  (push '(counsel-mark-ring . nil) ivy-sort-functions-alist))

;; For M-x history.
(load-package smex :defer)

;; Use the forked version in site-lisp.
(load-package recentf-ext
  :config (recentf-mode))

(load-package pinyinlib
  :commands pinyinlib-build-regexp-string
  :init
  (defun ivy-pinyin-re-builder (str)
    (if (string-prefix-p ";" str)
        (pinyinlib-build-regexp-string
         (ivy--regex (string-trim str ";")))
      (ivy--regex str)))
  (setf (alist-get t ivy-re-builders-alist)
        #'ivy-pinyin-re-builder))

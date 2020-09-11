;;-*- lexical-binding: t -*-

(add-hook 'emacs-startup-hook
          (let ((old-list file-name-handler-alist)
                (threshold (* 10 gc-cons-threshold))
                (percentage gc-cons-percentage))
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)
              (setq file-name-handler-alist old-list
                    gc-cons-threshold threshold
                    gc-cons-percentage percentage)
              (garbage-collect)))
          t)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

;; from centaur
(push '(vertical-scroll-bars . nil) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(when window-system
  ;; (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; BTW, you have to enable menu bar on mac port,
;; otherwise emacs lost focus.

(when (not window-system)
  (menu-bar-mode -1))

;; (when window-system
;;   (add-hook 'after-init-hook #'toggle-frame-maximized))


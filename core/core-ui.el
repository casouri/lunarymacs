;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar moon-homepage-buffer "MOON"
  "The buffer name of the homepage")

(defvar moon-load-theme-hook ()
  "Hook ran after `load-theme'")

;;
;; Config
;;

(setq inhibit-startup-screen t)
(setq initial-buffer-choice (lambda () (get-buffer-create moon-homepage-buffer)))

(defadvice load-theme (after run-load-theme-hook activate)
  (run-hook-with-args 'moon-load-theme-hook))

(provide 'core-ui)

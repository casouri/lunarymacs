;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar moon-homepage-buffer "MOON"
  "The buffer name of the homepage")

;;
;; Config
;;

(setq inhibit-startup-screen t)
(setq initial-buffer-choice (lambda () (get-buffer-create moon-homepage-buffer)))

(provide 'core-ui)

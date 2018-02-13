;;; -*- lexical-binding: t -*-

;;
;; Var
;;

(defvar moon-homepage-buffer "MOON"
  "The buffer name of the homepage")

(defvar moon-load-theme-hook ()
  "Hook ran after `load-theme'")

(defvar moon-magic-font-book
  '(
    ("Source Code Pro" . (moon-set-font| :family "Source Code Pro"
                                  :weight 'light
                                  :size 14))
    ("SF Mono" . (moon-set-font| :family "SF Mono" :weight 'light :size 14)))

  "All the fonts you can switch between by `moon/load-font'
It is an alist with the form
((name . (moon-set-font| configuration))
 (name . (moon-set-font| :family \"family\" :weight 'weight)))

I have to do it this way because apply flattens a list ignoring quotes.
So '(:family \"SF Mono\" :weight 'light) will become
(:family \"SF Mono\" :weight quote light).
And such list cannot be passed into a `font-spec'.")

(defvar moon-toggle-theme-list
  '(spacemacs-dark spacemacs-light)
  "Themes that you can toggle bwtween by `moon/switch-theme'")

;;
;; Config
;;

(setq inhibit-startup-screen t)
(setq initial-buffer-choice (lambda () (get-buffer-create moon-homepage-buffer)))

(defadvice load-theme (after run-load-theme-hook activate)
  (run-hook-with-args 'moon-load-theme-hook))

(provide 'core-ui)

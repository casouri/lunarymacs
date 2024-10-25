;;;; Font
(when (display-graphic-p)
  (luna-enable-apple-emoji)
  (luna-load-font 'default "Cascadia" 14)
  (luna-load-font 'fixed-pitch "Cascadia" 14)
  (luna-load-font 'variable-pitch "Academica" 16)
  (luna-load-font 'fixed-pitch-serif "Cascadia" 14)
  (luna-load-font 'mode-line "Arial" 12 :weight 'regular :height 125)
  (with-eval-after-load 'shortdoc
    (luna-load-font 'shortdoc-section "Arial" 13
                    :weight 'medium :height 150))
  (add-hook 'luna-load-theme-hook
            (lambda ()
              (luna-load-font 'mode-line "Arial" 12
                              :weight 'regular :height 125))))
;;;; Frame
(when (display-graphic-p)
  (set-frame-width (selected-frame) 150)
  (set-frame-height (selected-frame) 44)
  (set-frame-position nil 30 50))
;;;; Keys
(setq mac-option-modifier 'meta
      mac-command-modifier 'super
      mac-pass-command-to-system nil ; fix cmd h
      mac-system-move-file-to-trash-use-finder t)
(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-v") #'yank)
;; I sometimes hit this by accident.
(global-set-key (kbd "s-H") #'ignore)
;;;; Environment
(if luna-dumped
    (dolist (var luna-env-vars)
      (exec-path-from-shell-setenv (car var) (cdr var)))
  (exec-path-from-shell-initialize))
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
(setq source-directory (expand-file-name "~/emacs"))
(setq xref-search-program 'ripgrep)
;;;; Scrolling
(setq scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      redisplay-skip-fontification-on-input t)
(setq mouse-wheel-flip-direction t)

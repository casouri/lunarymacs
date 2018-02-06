;;
;; Var
;;

(defvar moon-homepage-buffer "MOON"
  "The buffer name of the homepage")
;;
;; Config
;;
(generate-new-buffer moon-homepage-buffer)

(setq inhibit-startup-screen t)
(setq initial-buffer-choice (lambda () (get-buffer moon-homepage-buffer)))

(provide 'core-ui)

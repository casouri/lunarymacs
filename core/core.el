(defvar moon-emacs-d-dir (expand-file-name user-emacs-directory)
  "Basically ~/.emacs.d but a full path.")

(defvar moon-core-dir (concat moon-emacs-d-dir "core/")
  "Where core is located.")

(defvar moon-star-dir (concat moon-emacs-d-dir "star/")
  "Where stars shine.")

(defvar moon-local-dir (concat moon-emacs-d-dir ".local/")
  "Where package and other stuff goes. For files that are useful across sessions.")

(defvar moon-cache-dir (concat moon-emacs-d-dir ".cache/")
  "Where tmp files rest. For files that are dedicated to each session.")


;; DEBUG
;; (setq moon-core-dir "/Users/yuan/.emacs.second/core")

(defvar moon-init-hook nil
  "A list of hooks run when Emacs is initialized, before `moon-post-init-hook'.")

(defvar moon-post-init-hook nil
  "A list of hooks run after Emacs initialization is complete, and after
`moon-init-hook'.")

;;
;; Config
;;

(setq package-enable-at-startup nil)
(setq custom-file (concat moon-local-dir "custom.el"))
(add-hook 'post-init-hook (lambda () (load custom-file)))

;;
;; Init
;;

(load (concat moon-core-dir "core-package"))
;; optimization on startup
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(defvar tmp-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)


(load| core-ui)

(defun moon-finalize ()
  (moon-initialize)
  (moon-initialize-load-path)
  (moon-initialize-star)
  (dolist (hook '(moon-init-hook moon-post-init-hook))
    (run-hook-with-args hook))

  ;; If you forget to reset this, you'll get stuttering and random freezes!
  (setq gc-cons-threshold 800000
        gc-cons-percentage 0.1
        file-name-handler-alist tmp-file-name-handler-alist
        )
  )

(add-hook 'emacs-startup-hook #'moon-finalize t)

;; load other core files

;; (add-hook 'moon-init-hook #'moon-initialize t)
;; (add-hook 'moon-init-hook #'moon-initialize-star t)
;; (moon-initialize)
;; (moon-initialize-load-path)
;; (moon-initialize-star)

(provide 'core)


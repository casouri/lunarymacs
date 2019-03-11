;; -*- lexical-binding: t -*-

(eval-when-compile (load (concat (expand-file-name user-emacs-directory) "core/core-general.el")))

(defvar moon-startup-hook-1 ()
  "Run after startup and before moon-startup-hook-2.")

(defvar moon-startup-hook-2 ()
  "Run after startup and after moon-startup-hook-1.")

(defvar moon-profile-startup nil
  "Whether start profiler on startup.")

(setq custom-file (concat moon-local-dir "custom.el"))

;; optimization on startup
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast


(defun moon-startup ()
  "The main startup function."
  (when moon-profile-startup
    (profiler-start 'cpu))
  (unless moon-setup
    (condition-case err
        (let (file-name-handler-alist
              (gc-cons-threshold 1000000000)
              (gc-cons-percentage 0.6)
              (debug-on-error moon-debug-on-startup))
          (load| core-ui)
          (load| core-edit)
          (moon-load-star)
          (moon-load-or-create custom-file)
          (run-hook-with-args 'moon-startup-hook-1)
          (run-hook-with-args 'moon-startup-hook-2))
      ((debug error) (print err))))
  (when moon-profile-startup
    (profiler-report)))

(add-hook 'emacs-startup-hook #'moon-startup t)

(provide 'core)

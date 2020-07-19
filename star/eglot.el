;; -*- lexical-binding: t; -*-

;;; Keys

(with-eval-after-load 'luna-general-config
  (luna-default-leader
    "lf" #'eglot-format-buffer
    "lR" #'eglot-rename
    "ld" #'xref-find-definitions
    "lr" #'xref-find-references
    "lk" #'eldoc-box-quit-frame
    "ls" #'eldoc-box-show-frame))

;;; Config
;; speed up IPC
(setq read-process-output-max (* 1024 1024))

;;; Packages

(load-package eglot
  :commands eglot eglot-ensure)

;; (load-package eldoc-box
;;   :commands (eldoc-box-hover-mode
;;              eldoc-box-eglot-help-at-point
;;              eldoc-box-helpful-callable
;;              eldoc-box-helpful-variable
;;              eldoc-box-helpful-key)
;;   :init
;;   ;; (add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-mode t)
;;   (setq eldoc-box-cleanup-interval 0.2
;;         eldoc-box-clear-with-C-g t
;;         eldoc-box-doc-fold-threshold 200)
;;   :config
;;   (set-face-attribute 'eldoc-box-body nil :family "SF Pro"))

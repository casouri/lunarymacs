;; -*- lexical-binding: t; -*-

(require 'subr-x)

;;; Packages

(load-package minions)

;; (load-package bottom-line
;;   :config
;;   (setq window-divider-default-places 'bottom-only
;;         window-divider-default-bottom-width 1)
;;   (window-divider-mode)
;;   (add-hook 'after-init-hook
;;             (lambda ()
;;               (set-face-attribute 'mode-line nil :background nil)))
;;   (bottom-line-mode))

;;; Functions

(defun luna-mode-line-coding-system ()
  "Display abnormal coding systems."
  (let ((coding (symbol-name buffer-file-coding-system)))
    (if (or (and (not (string-prefix-p "prefer-utf-8" coding))
                 (not (string-prefix-p "utf-8" coding))
                 (not (string-prefix-p "undecided" coding)))
            (string-suffix-p "dos" coding))
        (concat "  " coding)
      "")))

;;; Config

(setq mode-line-right-align-edge 'right-fringe)
(setq-default mode-line-format
              (let* ((spaces
                      (propertize " " 'display '(space :width 1.2)))
                     (fringe (propertize
                              " " 'display '(space :width fringe)))
                     (percentage
                      '(format
                        "[%%l] %d%%"
                        (/ (* (window-end) 100.0) (point-max)))))
                `(,fringe
                  (:eval (if (window-dedicated-p) "🚷" ""))
                  (:eval (if buffer-read-only "🔒" ""))
                  (:propertize "%[%b%]" face (:weight semi-bold))
                  (:eval (luna-mode-line-coding-system))
                  ,spaces
                  ,(if (boundp 'minions-mode-line-modes)
                       'minions-mode-line-modes
                     'mode-line-modes)
                  ,spaces
                  (:eval (if (buffer-modified-p)
                             ,(if (display-graphic-p) "ΦAΦ" "OAO")
                           ,(if (display-graphic-p) "ΦwΦ" "OwO")))
                  (flymake-mode
                   (,spaces
                    flymake-mode-line-title
                    flymake-mode-line-exception
                    flymake-mode-line-counters))
                  ,(if (display-graphic-p)
                       (list (propertize "  " 'display '(raise 0.3))
                             (propertize " " 'display '(raise -0.3)))
                     " ")
                  mode-line-misc-info
                  mode-line-format-right-align
                  (:eval (concat ,percentage "%%"))
                  ;; (:eval (concat ,spaces "(%l) " ,percentage "%%"))
                  )))

(setq-default header-line-format nil)

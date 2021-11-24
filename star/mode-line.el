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

(load-package which-func
  :config
  (which-func-mode))

;;; Functions

(defun luna-mode-line-with-padding (text)
  "Return TEXT with padding on the left.
The padding pushes TEXT to the right edge of the mode-line."
  (let* ((len (string-pixel-width text))
         (padding (propertize
                   "-" 'display
                   `(space :align-to
                           (- (+ right right-margin) (,len))))))
    (concat padding text)))

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

(setq-default bottom-line-format
              (let* ((spaces
                      (propertize " " 'display '(space :width 1.5)))
                     (fringe (propertize
                              " " 'display '(space :width fringe)))
                     (percentage
                      '(format
                        "%d%%" (/ (* (window-end) 100.0) (point-max)))))
                `(,fringe
                  (:eval (if (window-dedicated-p)
                             (concat "🔒" ,spaces) ""))
                  (:propertize "%[%b%]" face (:weight bold))
                  (:eval (luna-mode-line-coding-system))
                  ,spaces
                  ,(if (featurep 'minions)
                       'minions-mode-line-modes
                     'mode-line-modes)
                  ,spaces
                  (:eval
                   (cond (inhibit-read-only
                          ,(if (display-graphic-p) "–ω–" "-w-"))
                         ((buffer-modified-p)
                          ,(if (display-graphic-p) "ΦAΦ" "OAO"))
                         (t
                          ,(if (display-graphic-p) "ΦωΦ" "OwO"))))
                  ,spaces
                  mode-line-misc-info
                  ,(if (display-graphic-p)
                       `(:eval (concat (luna-mode-line-with-padding
                                        ,percentage) "%%"))
                     `(:eval (concat ,spaces ,percentage "%%")))
                  ;; (:eval (concat ,spaces "(%l) " ,percentage "%%"))
                  (:propertize " " display (raise 0.3))
                  (:propertize " " display (raise -0.3))
                  )))

(setq-default header-line-format nil)
(setq-default mode-line-format bottom-line-format)
(setq-default frame-title-format "⚘ %f ⚘")

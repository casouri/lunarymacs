;; -*- lexical-binding: t; -*-

(require 'subr-x)

(load-package minions)

;; When we use ‘set-face-font’ to set the font, Emacs converts the
;; font-spec is to a font-object, which we can later retrieve with
;; ‘face-attribute’ and use for calculating the text width.
(add-hook 'luna-load-theme-hook
          (lambda ()
            (set-face-font 'mode-line
                           (font-spec :family "SF Pro Text"
                                      :weight 'light
                                      :height 130))))
(add-hook 'after-init-hook (lambda () (run-hooks 'luna-load-theme-hook)))

(defun luna-mode-line-with-padding (text)
  "Return TEXT with padding on the left.
The padding pushes TEXT to the right edge of the mode-line."
  (let ((font (face-attribute 'mode-line :font)))
    (if (not (fontp font))
        " "
      (let* ((glyph-list (font-get-glyphs font 0 (length text) text))
             (len (cl-reduce (lambda (len glyph)
                               (+ len (aref glyph 4)))
                             glyph-list
                             :initial-value 0))
             (padding (propertize
                       "-" 'display
                       `(space :align-to
                               (- (+ right right-margin) (,len))))))
        (concat padding text)))))

(defun luna-mode-line-coding-system ()
  "Display abnormal coding systems."
  (let ((coding (symbol-name buffer-file-coding-system)))
    (if (or (and (not (string-prefix-p "prefer-utf-8" coding))
                 (not (string-prefix-p "utf-8" coding))
                 (not (string-prefix-p "undecided" coding)))
            (string-suffix-p "dos" coding))
        (concat "  " coding)
      "")))

(setq-default mode-line-format
              (let* ((spaces
                      (propertize " " 'display '(space :width 1.5)))
                     (fringe (propertize
                              " " 'display '(space :width fringe)))
                     (percentage
                      '(format
                        "%d%%" (/ (* (window-end) 100.0) (point-max)))))
                `(,fringe
                  (:eval (if (window-dedicated-p)
                             (concat "🔒" spaces) ""))
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
                  (:propertize " " display (raise 0.3))
                  (:propertize " " display (raise -0.3))
                  mode-line-misc-info
                  ,(if (display-graphic-p)
                       `(:eval (concat (luna-mode-line-with-padding
                                        ,percentage) "%%"))
                     `(:eval (concat ,spaces ,percentage "%%"))))))


(setq-default header-line-format nil)

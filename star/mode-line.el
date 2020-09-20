;; -*- lexical-binding: t; -*-

(require 'subr-x)

(load-package minions)

(define-minor-mode inhibit-read-only-mode
  "Inhibit read-only in this buffer."
  :lighter ""
  (if inhibit-read-only-mode
      (setq-local inhibit-read-only t)
    (setq-local inhibit-read-only nil)))

(defun mode-line-with-padding (text)
  (let* ((font (face-attribute 'mode-line :font))
         (glyph-list (font-get-glyphs font 0 (length text) text))
         (len (cl-reduce (lambda (len glyph)
                           (+ len (aref glyph 4)))
                         glyph-list
                         :initial-value 0))
         (padding (propertize "-" 'display
                              `(space :align-to (- right (,len))))))
    (concat padding text)))

(setq-default mode-line-format
              (let* ((spaces
                      (propertize " " 'display '(space :width 1.5)))
                     (percentage
                      '(format
                        "%d%%" (/ (* (point) 100.0) (point-max)))))
                `((:eval (if (window-dedicated-p)
                             "🔒"
                           ""))
                  ,spaces
                  "%b"
                  ,spaces
                  ,(if (featurep 'minions)
                       'minions-mode-line-modes
                     'mode-line-modes)
                  ,spaces
                  (:eval
                   (cond (inhibit-read-only
                          ,(if (display-graphic-p)
                               "– ω –"
                             "-w-"))
                         ((buffer-modified-p)
                          ,(if (display-graphic-p)
                               "Φ A Φ"
                             "OAO"))
                         (t
                          ,(if (display-graphic-p)
                               "Φ ω Φ"
                             "OwO"))))
                  ,spaces
                  mode-line-misc-info
                  ,(if (display-graphic-p)
                       `(:eval (concat (mode-line-with-padding
                                        ,percentage)
                                       "%%"))
                     `(:eval (concat ,spaces ,percentage "%%"))))))

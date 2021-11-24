;; -*- lexical-binding: t; -*-

(require 'svg)

;;; Tab-line

(with-eval-after-load 'tab-line
  (setq tab-line-tab-name-function
        (lambda (buffer &optional _)
          (format " %s " (buffer-name buffer))))

  (setq tab-line-new-button
        (propertize " + "
                    'display
                    (let ((svg (svg-create 9 9)))
                      (svg-path svg '((moveto ((1 . 4.5) (8 . 4.5))))
                                :stroke "black")
                      (svg-path svg '((moveto ((4.5 . 1) (4.5 . 8))))
                                :stroke "black")
                      (svg-image svg :ascent 'center))
                    'keymap tab-line-add-map
                    'mouse-face 'tab-line-highlight
                    'help-echo "Click to add tab"))

  (setq tab-line-close-button
        (propertize " x"
                    'display
                    (let ((svg (svg-create 9 9)))
                      (svg-path svg '((moveto ((1 . 1) (8 . 8))))
                                :stroke "black"
                                :stroke-width 1.5)
                      (svg-path svg '((moveto ((1 . 8) (8 . 1))))
                                :stroke "black"
                                :stroke-width 1.5)
                      (svg-image svg :ascent 'center))
                    'keymap tab-line-tab-close-map
                    'mouse-face 'tab-line-close-highlight
                    'help-echo "Click to close tab"))

  (setq tab-line-left-button
        (propertize " <"
                    'display
                    (let ((svg (svg-create 9 9)))
                      (svg-path svg '((moveto ((5 . 0) (1 . 4.5)
                                               (5 . 9))))
                                :stroke "black"
                                :stroke-width 1.5
                                :fill "transparent")
                      (svg-image svg :ascent 'center))
                    'keymap tab-line-left-map
                    'mouse-face 'tab-line-highlight
                    'help-echo "Click to scroll left"))

  (setq tab-line-right-button
        (propertize "> "
                    'display
                    (let ((svg (svg-create 9 9)))
                      (svg-path svg '((moveto ((4 . 0) (8 . 4.5)
                                               (4 . 9))))
                                :stroke "black"
                                :stroke-width 1.5
                                :fill "transparent")
                      (svg-image svg :ascent 'center))
                    'keymap tab-line-right-map
                    'mouse-face 'tab-line-highlight
                    'help-echo "Click to scroll right")))

;;; Widget

(defun highres-icon-widget-image-find (oldfn image)
  (let ((fill (face-attribute 'default :foreground)))
    (pcase image
      ((or "down-pushed" "down")
       (let ((svg (svg-create 9 9)))
         (svg-polygon svg '((0 . 0) (9 . 0) (4.5 . 9)) :fill fill)
         (svg-image svg)))
      ((or "right-pushed" "right")
       (let ((svg (svg-create 9 9)))
         (svg-polygon svg '((0 . 0) (0 . 9) (8 . 4.5)) :fill fill)
         (svg-image svg)))
      (_ (funcall oldfn image)))))

(advice-add #'widget-image-find :around #'highres-icon-widget-image-find)
;; (advice-remove 'widget-image-find 'highres-icon-widget-image-find)

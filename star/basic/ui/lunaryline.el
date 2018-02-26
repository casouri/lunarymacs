;;; -*- lexical-binding: t -*-

(require 'powerline)

;;
;; Face
;;

(defface lunaryline-yellow
  `((t (:background "DarkGoldenrod2"
        :foreground "#3E3D31"
        :weight bold
        :inherit 'mode-line)))
  "Yellow highlight face for lunaryline."
  :group 'lunaryline)

(defface lunaryline-blue
  `((t (:background ,doom-blue
        :foreground "#3E3D31"
        :weight bold
        :inherit 'mode-line)))
  "Blue highlight face for lunaryline."
  :group 'lunaryline)


(defvar lunaryline-narrow-window-threshold
  90
  "If window width is lower than this number,

modeline segments that are inside `disappear-when-narrow'
macro will disappear.")

(defmacro disappear-when-narrow| (&rest rest)
  "If window width is lower than `lunaryline-narrow-window-threshold',

modeline segment REST will disappear.
Note that This macro have to be used once for each segment,
you cannot use one `disappear-when-narrow' for multiple segment."
  `(when (> (window-width) lunaryline-narrow-window-threshold)
     ,@rest))


;;
;; Flycheck
;;

(defface lunaryline-flycheck-error
  '((t (:foreground "#FC5C94" :distant-foreground "#A20C41")))
  "Face for flycheck error feedback in the modeline."
  :group 'lunaryline)

(defface lunaryline-flycheck-warning
  '((t (:foreground "#F3EA98" :distant-foreground "#968B26")))
  "Face for flycheck warning feedback in the modeline."
  :group 'lunaryline)

(defface lunaryline-flycheck-info
  '((t (:foreground "#8DE6F7" :distant-foreground "#21889B")))
  "Face for flycheck info feedback in the modeline."
  :group 'lunaryline)

(defvar lunaryline-flycheck-bullet "•%s"
  "The bullet used for the flycheck segment.
This should be a format string with a single `%s'-expression corresponding to
the number of errors.")

(defmacro lunaryline-flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  `(let* ((counts (flycheck-count-errors flycheck-current-errors))
          (errorp (flycheck-has-current-errors-p ',state))
          (err (or (cdr (assq ',state counts)) "?"))
          (running (eq 'running flycheck-last-status-change)))
     (if (or errorp running) (format lunaryline-flycheck-bullet err))))

;; (format lunaryline-flycheck-bullet (or (cdr (assq 'warning (flycheck-count-errors flycheck-current-errors))) "?"))

(dolist (state '(error warning info))
  (let ((segment-name (intern (format "flycheck-%S" state)))
        ;; (face (intern (format "lunaryline-flycheck-%S" state)))
        )
    (eval
     `(defpowerline ,segment-name
        (when (and (bound-and-true-p flycheck-mode)
                   (or flycheck-current-errors
                       (eq 'running flycheck-last-status-change)))
          (let ((lighter (lunaryline-flycheck-lighter ,state)))
            (if lighter
                (s-trim lighter)
              "")))))))

;;
;; Winum
;;

(defpowerline lunaryline-winum
  (when (bound-and-true-p winum-mode)
    (format "%d " (winum-get-number))
    ))


;;
;; Encodeing
;;

(defpowerline lunaryline-encoding
  ;; have to use a form, variable won't work
  (format "%s" buffer-file-coding-system))

;;
;; Version control
;;
(defpowerline lunaryline-vc
  (if (and (buffer-file-name) vc-mode)
      (format "%s:%s"
              (if (featurep 'magit)
                  (magit-get-current-branch)
                "Git") ; magit is autoloaded
              (vc-state (buffer-file-name)))
    "N/A"))

;;
;; Theme
;;

(defun lunaryline-default-theme ()
  "Setup the default modeline."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (facey (if active 'lunaryline-yellow 'powerline-inactive1))
                          (faceb (if active 'lunaryline-blue 'powerline-inactive1))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          ;; left
                          (lhs (list
                                (if (buffer-modified-p)
                                    (powerline-raw (lunaryline-winum) facey 'l)
                                  (powerline-raw (lunaryline-winum) faceb 'l)
                                    )
                                ;; separator >> face0
                                ;; buffer info
                                (if (buffer-modified-p)
                                    (funcall separator-left facey face0)
                                  (funcall separator-left faceb face0)
                                    )
                                (when powerline-display-buffer-size
                                  (powerline-buffer-size face0 'l))
                                (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                (powerline-raw " " face0)
                                ;; separator >> face1
                                ;; major mode
                                (funcall separator-right face0 face1)
                                (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                  (powerline-raw erc-modified-channels-object face0 'l))
                                (powerline-major-mode face1 'l)
                                (powerline-raw " " face1)
                                ;; separator >> face0
                                ;; flycheck
                                (funcall separator-left face1 face0)
                                (flycheck-error 'lunaryline-flycheck-error 'l)
                                (flycheck-warning 'lunaryline-flycheck-warning 'l)
                                (flycheck-info 'lunaryline-flycheck-info 'l)
                                ;; (powerline-minor-modes face0 'l)
                                (powerline-narrow face0 'l)
                                (powerline-raw " " face0)
                                ;; separator >> face1
                                ;; git
                                (funcall separator-right face0 face1)
                                (lunaryline-vc face1 'l)
                                (powerline-raw " " face1)
                                ;; separator >> face0
                                ;; nyan
                                (disappear-when-narrow|
                                 (funcall separator-left face1 face0))
                                (disappear-when-narrow|
                                 (when (bound-and-true-p nyan-mode)
                                   (powerline-raw (list (nyan-create)) face0 'l)))
                                ;; separator >> face1
                                (disappear-when-narrow|
                                 (funcall separator-left face0 face1))
                                ))
                          ;; right
                          ;; use face1 instead of face2
                          (rhs (list
                                ;; whatever junk you append to it
                                (powerline-raw global-mode-string face1 'r)
                                ;; separator >> face0
                                ;; encoding
                                (funcall separator-right face1 face0)
                                (lunaryline-encoding face0 'l)
                                ;; separator >> face 1
                                ;; line:colomn number
                                (funcall separator-right face0 face1)
                                (powerline-raw "%3l" face1 'l) ; line number, reserve 3 digit
                                (powerline-raw ":" face1 'l)
                                (powerline-raw "%2c" face1 'r) ; colomn number, reserve 2 digit
                                ;; separator >> face0
                                ;; hud
                                (funcall separator-right face1 face0)
                                (powerline-raw " " face0)
                                (powerline-raw "%6p" face0 'r) ; percentage of screen
                                (when powerline-display-hud
                                  (powerline-hud facey face0))
                                (powerline-fill face0 0)
                                )))
                     (concat (powerline-render lhs)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

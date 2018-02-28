;;; lunaryline.el --- fast and minimum alternative of spaceline  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <yuan@missSilver>
;; URL: https://github.com/casouri/lunaryline
;; Version: 0.0.9
;; Keywords: convenience, extensions, powerline, spaceline, spacemacs
;; Package-Requires: ((emacs "24.4") (powerline "2.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a fast and minimum alternative to beautiful but slow spaceline.
;; Like spaceline, lunaryline is based on powerline.
;; Unlike spaceline, there is no customization functionality.
;; In another word, lunaryline is a powerline theme that looks like spaceline.

;;; Code:

(require 'powerline)
;; (require 's)


;;
;; Color
;;

(setq error-red (if (bound-and-true-p mac-red) mac-red "#FA5754"))
(setq warning-yellow (if (bound-and-true-p spacemacs-yellow) spacemacs-yellow "DarkGoldenrod2"))
(setq info-blue (if (bound-and-true-p doom-blue) doom-blue "#56B0EC"))

;;
;; Face
;;


(defface lunaryline-yellow
  `((t (:background ,warning-yellow
        :foreground "#3E3D31"
        :weight bold
        :inherit 'mode-line)))
  "Yellow highlight face for lunaryline."
  :group 'lunaryline)

(defface lunaryline-yellow-light
  `((t (:background ,warning-yellow
        :foreground "#3E3D31"
        :inherit 'mode-line)))
  "Yellow highlight face for lunaryline."
  :group 'lunaryline)

(defface lunaryline-blue
  `((t (:background ,info-blue
        :foreground "#3E3D31"
        :weight bold
        :inherit 'mode-line)))
  "Blue highlight face for lunaryline."
  :group 'lunaryline)

(defface lunaryline-blue-light
  `((t (:background ,info-blue
        :foreground "#3E3D31"
        :inherit 'mode-line)))
  "Blue highlight face for lunaryline."
  :group 'lunaryline)

(defvar lunaryline-narrow-window-threshold
  120
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

(defmacro only-in-prog-mode| (&rest rest)
  "Only evaluate expressions in REST when in modes derived from ‘prog-mode’ or ‘text-mode’."
  `(when (or (derived-mode-p 'prog-mode)
             (derived-mode-p 'text-mode))
     ,@rest))

(defmacro when-exist| (test form)
  "When TEST is bounded and true (`bound-and-true-p'), evaluate FORM."
  (declare (indent defun))
  `(when (bound-and-true-p ,test)
       ,form))

(defmacro if-exist| (test form-exist form-not-exist)
  "If `bound-and-true-p' TEST, evaluate FORM-EXIST, otherwise evaluate FORM-NOT-EXIST."
  (declare (indent defun))
  `(if (bound-and-true-p ,test)
       ,form-exist
     ,form-not-exist))

;;
;; Flycheck
;;


(defface lunaryline-flycheck-error
  `((t (:foreground ,error-red :distant-foreground "#A20C41")))
  "Face for flycheck error feedback in the modeline."
  :group 'lunaryline)

(defface lunaryline-flycheck-warning
  `((t (:foreground ,warning-yellow :distant-foreground "#968B26")))
  "Face for flycheck warning feedback in the modeline."
  :group 'lunaryline)

(defface lunaryline-flycheck-info
  `((t (:foreground ,info-blue :distant-foreground "#21889B")))
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
  (let ((segment-name (intern (format "lunaryline-flycheck-%S" state)))
        ;; (face (intern (format "lunaryline-flycheck-%S" state)))
        )
    (eval
     `(defpowerline ,segment-name
        (when (and (bound-and-true-p flycheck-mode)
                   (or flycheck-current-errors
                       (eq 'running flycheck-last-status-change)))
          (let ((lighter (lunaryline-flycheck-lighter ,state)))
            (if lighter
                ;; (s-trim lighter)
                lighter
              "")))))))


;;
;; Winum
;;

;; don't display winum in modeline
;; because lunaryline will do that
(setq winum-auto-setup-mode-line nil)

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

(defpowerline lynaryline-encoding-abbv
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

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
;; Eyebrowse
;;

(defun lunaryline-eyebrowse-mode-line-indicator (active-face inactive-face)
  "Return a string representation of the window configurations.
ACTIVE-FACE is for the number representing current workspace,
INACTIVE-FACE is for others."
  (let* ((left-delimiter (propertize eyebrowse-mode-line-left-delimiter
                                     'face inactive-face)) ; edit mark
         (right-delimiter (propertize eyebrowse-mode-line-right-delimiter
                                      'face inactive-face)) ; edit mark
         (separator (propertize eyebrowse-mode-line-separator
                                'face inactive-face)) ; edit mark
         (current-slot (eyebrowse--get 'current-slot))
         (window-configs (eyebrowse--get 'window-configs)))
    (if (and eyebrowse-mode-line-style
             (not (eq eyebrowse-mode-line-style 'hide))
             (or (and (not (eq eyebrowse-mode-line-style 'smart))
                      eyebrowse-mode-line-style)
                 (and (eq eyebrowse-mode-line-style 'smart)
                      (> (length window-configs) 1))))
        (concat
         left-delimiter
         (mapconcat
          (lambda (window-config)
            (let* ((slot (car window-config))
                   (face (if (= slot current-slot)
                             active-face ; edit mark
                           inactive-face)) ; edit mark
                   (keymap
                    (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "<mode-line><mouse-1>")
                        (lambda (e)
                          (interactive "e")
                          (eyebrowse-switch-to-window-config slot)))
                      map))
                   (help-echo "mouse-1: Switch to indicated workspace")
                   (caption (eyebrowse-format-slot window-config)))
              (propertize caption 'face face 'slot slot
                          'mouse-face 'mode-line-highlight
                          'local-map keymap
                          'help-echo help-echo)))
          window-configs separator)
         right-delimiter)
      "")))


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
                          (face0  (if active 'powerline-active0       'powerline-inactive0))
                          (face1  (if active 'powerline-active1       'powerline-inactive1))
                          (face2  (if active 'powerline-active2       'powerline-inactive2))
                          (facey  (if active 'lunaryline-yellow       'powerline-inactive1))
                          (faceyl (if active 'lunaryline-yellow-light 'powerline-inactive1))
                          (faceb  (if active 'lunaryline-blue         'powerline-inactive1))
                          (facebl (if active 'lunaryline-blue-light   'powerline-inactive1))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          ;; left
                          (lhs (list
                                ;; window number & workspace number
                                ;; faceb when not edited
                                ;; facey when edited
                                (when-exist| eyebrowse-mode
                                  (if (buffer-modified-p)
                                      (lunaryline-eyebrowse-mode-line-indicator faceyl facey)
                                    (lunaryline-eyebrowse-mode-line-indicator facebl faceb)))
                                (if-exist| winum-mode
                                  (if (buffer-modified-p)
                                      (powerline-raw (lunaryline-winum) facey 'l)
                                    (powerline-raw (lunaryline-winum) faceb 'l))
                                  (if (buffer-modified-p)
                                      (powerline-raw " " facey 'l)
                                    (powerline-raw " " faceb 'l)))
                                ;; separator >> face0
                                ;; buffer info
                                (if (buffer-modified-p)
                                    (funcall separator-left facey face0)
                                  (funcall separator-left faceb face0))
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
                                (only-in-prog-mode| (funcall separator-left face1 face0))
                                (only-in-prog-mode| (lunaryline-flycheck-error 'lunaryline-flycheck-error 'l))
                                (only-in-prog-mode| (lunaryline-flycheck-warning 'lunaryline-flycheck-warning 'l))
                                (only-in-prog-mode| (lunaryline-flycheck-info 'lunaryline-flycheck-info 'l))
                                (only-in-prog-mode| (powerline-raw " " face0))
                                ;; separator >> face1
                                ;; git
                                (only-in-prog-mode| (funcall separator-right face0 face1))
                                (only-in-prog-mode| (lunaryline-vc face1 'l))
                                (only-in-prog-mode| (powerline-raw " " face1))
                                ;; separator >> face0
                                ;; nyan
                                (disappear-when-narrow|
                                 (when-exist| nyan-mode
                                              (funcall separator-left face1 face0)))
                                (disappear-when-narrow|
                                 (when-exist| nyan-mode
                                   (powerline-raw (list (nyan-create)) face0 'l)))
                                ;; separator >> face1
                                (disappear-when-narrow|
                                 (when-exist| nyan-mode
                                   (funcall separator-left face0 face1)))
                                ))
                          ;; right
                          ;; use face1 instead of face2
                          (rhs (list
                                ;; whatever you append to it
                                ;; it's hard to explain what it does
                                ;; and I don't really know
                                ;; does anyone even use it?
                                (powerline-raw global-mode-string face1 'r)
                                ;; separator >> face0
                                ;; encoding
                                (funcall separator-right face1 face0)
                                (lunaryline-encoding face0 'l)
                                ;; separator >> face 1
                                ;; line:colomn number
                                (funcall separator-left face0 face1)
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

(provide 'lunaryline)

;;; lunaryline.el ends here

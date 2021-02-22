;;-*- lexical-binding: t -*-

(require 'luna-f)

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

;;;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;; Term mouse
(unless (display-graphic-p)
  (require 'mouse)
  (xterm-mouse-mode t)
  ;; (defun track-mouse (e))
  (setq mouse-sel-mode t))

(global-set-key (kbd "<mouse-4>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-5>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-6>") #'mwheel-scroll)
(global-set-key (kbd "<mouse-7>") #'mwheel-scroll)

;;;; "Dangerous Commands"
(put 'narrow-to-page 'disabled nil)

;;;; ERC
(with-eval-after-load 'erc
  (setq erc-nick "casouri"
        erc-nickserv-passwords
        '((freenode (("casouri" . "XF234567ic"))))
        erc-prompt-for-nickserv-password nil)
  (require 'erc-services)
  (erc-services-mode))

;;;; Customize
(defface custom-default nil "")
(add-hook 'Custom-mode-hook
          (lambda ()
            (let ((ov (make-overlay (point-min) (point-max))))
              ;; 0.2 above, 0.2 below.
              (overlay-put ov 'line-height 1.2)
              (overlay-put ov 'line-spacing 0.2))
            (buffer-face-set 'custom-default)
            (visual-line-mode)))

;;;; Littering
(load-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )

;;;; Button
;; mouse-1 to follow link
(put 'default-button 'follow-link t)

;;;; Benchmark

(load-package benchmark-init :defer)

;;;; Screencasting

(load-package keycast
  :commands
  keycast-mode
  keycast-log-mode
  :init (setq keycast-insert-after 'mode-line-misc-info))

;;-*- lexical-binding: t -*-

(require 'luna-f)

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(setq-default ;; current file name
 frame-title-format '("%f"))

;;;; minibuffer
;; keep the point out of the minibuffer
;; (setq minibuffer-prompt-properties
;;       '(read-only t point-entered minibuffer-avoid-prompt
;;                   face minibuffer-prompt))

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
            (setq-local line-spacing 0.3)
            (buffer-face-set 'custom-default)))

;;;; Littering
(load-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

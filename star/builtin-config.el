;;-*- lexical-binding: t -*-

(require 'luna-f)

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(setq-default
 backup-directory-alist
 `((".*" . ,(luna-f-join luna-cache-dir "backup")))

 auto-save-list-file-prefix
 (luna-f-join luna-cache-dir "auto-save-list/saves-")


 abbrev-file-name (expand-file-name "abbrev.el" luna-cache-dir)
 recentf-save-file (expand-file-name "recentf" luna-cache-dir)
 tramp-persistency-file-name (expand-file-name "tramp" luna-cache-dir)
 bookmark-default-file (expand-file-name "bookmarks" luna-cache-dir)
 savehist-file (expand-file-name "history" luna-cache-dir)
 project-list-file (expand-file-name "projects" luna-cache-dir)

 auto-save-file-name-transforms
 (list (list ".*" (luna-f-join luna-cache-dir "auto-save/") t)
       (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             (luna-f-join luna-cache-dir "auto-save-remote") t))

 ;; current file name
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
(unless window-system
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

;;;; hideshow
(add-hook 'prog-mode-hook #'hs-minor-mode)

;;;; ERC
;;
;; remember to enable “services” in erc-modules
(setq erc-nick "casouri"
      erc-nickserv-passwords
      '((freenode (("casouri" . "XF234567ic"))))
      erc-prompt-for-nickserv-password nil)

(with-eval-after-load 'erc
  (require 'erc-services)
  (erc-services-mode))

;;;; Customize

(defface custom-default nil "")
(add-hook 'Custom-mode-hook
          (lambda ()
            (setq-local line-spacing 0.3)
            (buffer-face-set 'custom-default)))

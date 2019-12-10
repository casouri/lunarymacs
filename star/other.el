;;; -*- lexical-binding: t -*-

(require 'luna-f)

;;; Config

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(setq-default
 ad-redefinition-action              'accept                               ; silence advised function warnings
 apropos-do-all                      t                                     ; make `apropos' more useful
 compilation-always-kill             t                                     ; kill compilation process before starting another
 compilation-ask-about-save          nil                                   ; save all buffers on `compile'
 confirm-nonexistent-file-or-buffer  t
 idle-update-delay                   2                                     ; update ui less often

 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties        '(read-only
                                       t
                                       point-entered
                                       minibuffer-avoid-prompt
                                       face
                                       minibuffer-prompt)

 create-lockfiles                    nil
 history-length                      500
 make-backup-files                   t
 auto-save-default                   t
 backup-directory-alist              `((".*" . ,(luna-f-join luna-cache-dir "backup")))
 auto-save-list-file-prefix          (luna-f-join luna-cache-dir "auto-save-list/saves-")
 auto-save-timeout                   5

 ;; files
 abbrev-file-name                    (expand-file-name "abbrev.el" luna-cache-dir)
 recentf-save-file                   (expand-file-name "recentf" luna-cache-dir)
 recentf-max-saved-items             300
 tramp-persistency-file-name         (expand-file-name "tramp" luna-cache-dir)
 bookmark-default-file               (expand-file-name "bookmarks" luna-cache-dir)
 delete-by-moving-to-trash           t
 savehist-file                       (expand-file-name "history" luna-cache-dir)
 auto-save-file-name-transforms      (list (list ".*" (luna-f-join luna-cache-dir "auto-save/") t)
                                           (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                                 (luna-f-join luna-cache-dir "auto-save-remote") t))

 ;; edit
 indent-tabs-mode                    nil
 backup-inhibited                    t
 sentence-end-double-space           nil
 kill-ring-max                       200

 ;; ui
 use-dialog-box                      nil
 visible-cursor                      nil
 use-dialog-box                      nil
 visible-bell                        nil
 frame-title-format                  '("%f")                              ; current file name
 display-line-numbers-width          3
 split-height-threshold              nil                                  ; Popup window to right
 split-width-threshold               80
 ns-pop-up-frames                    nil                                  ; no new frame when emacsclient connected

 ;; minibuffer
 enable-recursive-minibuffers        t
 )

;;;; modes
(blink-cursor-mode                   -1)

;;;; natural title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;;; "Dangerous Commands"
(put 'narrow-to-page 'disabled nil)

;;;; Save history
(savehist-mode)
(add-to-list 'savehist-additional-variables 'extended-command-history)

;;;; mirror function

(defun change-mirror (mirror)
  "Change mirror for ELPA and MELPA to MIRROR (symbol).
MIRROR can be ’emacs-china or ’default.

When called interactively, Prompt user to choose."
  (interactive (list (intern (completing-read "Choose mirror: "
                                              '("emacs-china" "default")))))
  (cond ((eq mirror 'emacs-china)
         (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                                  ("melpa" . "http://elpa.emacs-china.org/melpa/"))))
        ((eq mirror 'default)
         (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                  ("melpa" . "http://melpa.org/packages/"))))))

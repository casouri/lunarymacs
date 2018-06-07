;;; -*- lexical-binding: t -*-


;;
;; Config
;;

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(setq-default
 ad-redefinition-action              'accept                               ; silence advised function warnings
 apropos-do-all                      t                                     ; make `apropos' more useful
 compilation-always-kill             t                                     ; kill compilation process before starting another
 compilation-ask-about-save          nil                                   ; save all buffers on `compile'
 confirm-nonexistent-file-or-buffer  t
 enable-recursive-minibuffers        nil
 idle-update-delay                   2                                     ; update ui less often

 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties        '(read-only
                                       t
                                       point-entered
                                       minibuffer-avoid-prompt
                                       face
                                       minibuffer-prompt)

 ;; History & backup settings (save nothing, that's what git is for)
 create-lockfiles                    nil
 history-length                      500
 make-backup-files                   t
 auto-save-default                   t
 backup-directory-alist              `((".*" . ,moon-cache-dir))
 auto-save-file-name-transforms      `((".*" ,moon-cache-dir t))
 auto-save-list-file-name            (concat moon-cache-dir "autosave")
 auto-save-timeout                   5

 ;; files
 abbrev-file-name                    (concat moon-local-dir "abbrev.el")
 recentf-save-file                   (concat moon-local-dir "recentf")
 recentf-max-saved-items             300
 
 ;; edit
 indent-tabs-mode                    nil
 backup-inhibited                    t
 sentence-end-double-space           nil
 kill-ring-max                       200

 ;;ui
 use-dialog-box                      nil
 visible-cursor                      nil
 use-dialog-box                      nil
 ring-bell-function                  #'ignore
 visible-bell                        nil
 frame-title-format                  '("%f")                                 ; current file name
 display-line-numbers-width          3
 ;; Popup window to right!
 split-height-threshold              nil
 ;; split-width-threshold               100
 )

(blink-cursor-mode                   -1)


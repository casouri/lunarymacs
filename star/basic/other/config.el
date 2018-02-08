;;
;; Config
;;

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 apropos-do-all t                 ; make `apropos' more useful
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-ask-about-save nil   ; save all buffers on `compile'
 confirm-nonexistent-file-or-buffer t
 enable-recursive-minibuffers nil
 idle-update-delay 2              ; update ui less often
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 ;; files
 abbrev-file-name             (concat moon-local-dir "abbrev.el")
 auto-save-list-file-name     (concat moon-cache-dir "autosave")
 recentf-save-file (concat moon-local-dir "recentf")
 ;; edit
 indent-tabs-mode nil
 backup-inhibited t
 auto-save-default nil
 make-backup-files nil
 )


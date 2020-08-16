;; -*- lexical-binding: t; -*-

(with-eval-after-load 'general
  (general-define-key
   :keymaps 'mu4e-headers-mode-map
   "m" #'mu4e-headers-mark-for-something
   "M" #'mu4e-headers-mark-for-move
   "." #'mu4e-mark-resolve-deferred-marks
   ;; d is shallowed by region fast keys
   "<backspace>" #'mu4e-headers-mark-for-trash))

(load-package mu4e
  :load-path "/opt/local/share/emacs/site-lisp/mu4e/"
  :init
  (setq mail-user-agent 'mu4e-user-agent)
  :config
  (setq send-mail-function 'sendmail-send-it)
  (setq mu4e-user-mail-address-list  '("ykf5041@psu.edu" "yuan@sdf.org" "casouri@gmail.com")
        mu4e-get-mail-command        "mbsync -a"
        message-kill-buffer-on-exit  t)
  (setq mu4e-bookmarks
        '(("flag: unread AND NOT flag:trashed AND NOT m:/sent" "Unread messages" ?u)
          ("m:/Gmail/INBOX AND NOT m:/sent" "Gmail" ?g)
          ("m:/Gmail/emacs-devel AND NOT m:/sent" "emacs-devel" ?e)
          ("m:/Gmail/emacs-debbugs AND NOT m:/sent" "emacs-debbugs" ?d)))
  ;; Folders.
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-trash-folder
        (lambda (msg)
          (cond ((mu4e-message-contact-field-matches
                  msg '(:to :cc :bcc) "ykf5041@psu.edu")
                 "/PSU/Trash")
                ((mu4e-message-contact-field-matches
                  msg '(:to :cc :bcc) "yuan@sdf.org")
                 "/SDF/Trash")
                ((mu4e-message-contact-field-matches
                  msg '(:to :cc :bcc) "casouri@gmail.com")
                 "/Gmail/[Gmail]/Trash")
                (t "/trash"))))
  (setq mu4e-sent-folder "/sent")
  (setq mu4e-refile-folder
        (lambda (msg)
          (cond ((mu4e-message-contact-field-matches
                  msg '(:to :cc :bcc) "ykf5041@psu.edu")
                 "/PSU/Archive")
                ((mu4e-message-contact-field-matches
                  msg '(:to :cc :bcc) "yuan@sdf.org")
                 "/SDF/INBOX.Archive")
                ((mu4e-message-contact-field-matches
                  msg '(:to :cc :bcc) "casouri@gmail.com")
                 "/Gmail/Register")
                (t "/archive"))))
  (setq mu4e-drafts-folder "/drafts"))

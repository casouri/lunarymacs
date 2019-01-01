;; Compatibility functions for earlier versions of emacs

;; The functions in this file are copied from more modern versions of
;; emacs and are Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2017
;; Free Software Foundation, Inc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs master has a bugfix for folding long headers when sending
;; messages. Include the fix for earlier versions of emacs. To avoid
;; interfering with gnus we only run the hook when called from
;; notmuch-message-mode.

(declare-function mail-header-fold-field "mail-parse" nil)

(defun notmuch-message--fold-long-headers ()
  (when (eq major-mode 'notmuch-message-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (when (and (looking-at "[^:]+:")
		 (> (- (line-end-position) (point)) 998))
	(mail-header-fold-field))
      (forward-line 1))))

(unless (fboundp 'message--fold-long-headers)
  (add-hook 'message-header-hook 'notmuch-message--fold-long-headers))

(if (fboundp 'setq-local)
    (defalias 'notmuch-setq-local 'setq-local)
  (defmacro notmuch-setq-local (var val)
    "Set variable VAR to value VAL in current buffer.

Backport of setq-local for emacs without setq-local (pre 24.3)."
    `(set (make-local-variable ',var) ,val)))

(if (fboundp 'read-char-choice)
    (defalias 'notmuch-read-char-choice 'read-char-choice)
  (defun notmuch-read-char-choice (prompt chars &optional inhibit-keyboard-quit)
  "Read and return one of CHARS, prompting for PROMPT.
Any input that is not one of CHARS is ignored.

If optional argument INHIBIT-KEYBOARD-QUIT is non-nil, ignore
keyboard-quit events while waiting for a valid input.

This is an exact copy of this function from emacs 24 for use on
emacs 23, except with the one emacs 24 only function it calls
inlined."
  (unless (consp chars)
    (error "Called `read-char-choice' without valid char choices"))
  (let (char done show-help (helpbuf " *Char Help*"))
    (let ((cursor-in-echo-area t)
          (executing-kbd-macro executing-kbd-macro)
	  (esc-flag nil))
      (save-window-excursion	      ; in case we call help-form-show
	(while (not done)
	  (unless (get-text-property 0 'face prompt)
	    (setq prompt (propertize prompt 'face 'minibuffer-prompt)))
	  (setq char (let ((inhibit-quit inhibit-keyboard-quit))
		       (read-key prompt)))
	  (and show-help (buffer-live-p (get-buffer helpbuf))
	       (kill-buffer helpbuf))
	  (cond
	   ((not (numberp char)))
	   ;; If caller has set help-form, that's enough.
	   ;; They don't explicitly have to add help-char to chars.
	   ((and help-form
		 (eq char help-char)
		 (setq show-help t)
		 ;; This is an inlined copy of help-form-show as that
		 ;; was introduced in emacs 24 too.
		 (let ((msg (eval help-form)))
		   (if (stringp msg)
		       (with-output-to-temp-buffer " *Char Help*"
			 (princ msg))))))
	   ((memq char chars)
	    (setq done t))
	   ((and executing-kbd-macro (= char -1))
	    ;; read-event returns -1 if we are in a kbd macro and
	    ;; there are no more events in the macro.  Attempt to
	    ;; get an event interactively.
	    (setq executing-kbd-macro nil))
	   ((not inhibit-keyboard-quit)
	    (cond
	     ((and (null esc-flag) (eq char ?\e))
	      (setq esc-flag t))
	     ((memq char '(?\C-g ?\e))
	      (keyboard-quit))))))))
    ;; Display the question with the answer.  But without cursor-in-echo-area.
    (message "%s%s" prompt (char-to-string char))
    char)))

;; End of compatibility functions

(provide 'notmuch-compat)

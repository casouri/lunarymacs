;;; utility.el --- Utilities      -*- lexical-binding: t; -*-

(require 'lunary)
(require 'luna-f)
(require 'cl-lib)
(require 'subr-x)

;;; Emacs 28 back port

(unless (boundp 'undo--last-change-was-undo-p)
  (defun undo--last-change-was-undo-p (undo-list)
    (while (and (consp undo-list) (eq (car undo-list) nil))
      (setq undo-list (cdr undo-list)))
    (gethash undo-list undo-equiv-table))

  (defun undo-redo (&optional arg)
    "Undo the last ARG undos."
    (interactive "*p")
    (cond
     ((not (undo--last-change-was-undo-p buffer-undo-list))
      (user-error "No undo to undo"))
     (t
      (let* ((ul buffer-undo-list)
             (new-ul
              (let ((undo-in-progress t))
                (while (and (consp ul) (eq (car ul) nil))
                  (setq ul (cdr ul)))
                (primitive-undo arg ul)))
             (new-pul (undo--last-change-was-undo-p new-ul)))
        (message "Redo%s" (if undo-in-region " in region" ""))
        (setq this-command 'undo)
        (setq pending-undo-list new-pul)
        (setq buffer-undo-list new-ul))))))

;;; Buffer

(defun switch-buffer-same-major-mode ()
  "Switch buffer among those with the same major mode."
  (interactive)
  (switch-to-buffer
   (completing-read
    "Buffer: "
    (mapcar #'buffer-name
            (cl-remove-if-not (lambda (buf)
                                (provided-mode-derived-p
                                 (buffer-local-value 'major-mode buf)
                                 major-mode))
                              (buffer-list))))))

(defun open-in-finder ()
  "Open ‘default-directory’ in Finder."
  (interactive)
  (shell-command (format "open '%s'" default-directory))
  ;; For some reason, we need to explicitly switch to Finder.
  (shell-command
   "osascript -e 'tell application \"Finder\" to activate'"))

(defun open-in-iterm ()
  "Open ‘default-directory’ iTerm."
  (interactive)
  (shell-command
   (format "open -a /Applications/iTerm.app %s" default-directory)))

(define-minor-mode inhibit-read-only-mode
  "Inhibit read-only in this buffer."
  :lighter ""
  (if inhibit-read-only-mode
      (setq-local inhibit-read-only t)
    (setq-local inhibit-read-only nil)))

;;; File

(defun luna-rename-file ()
  ;; https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun luna-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; ENV

(defun luna-load-env ()
  "Load PATH and CPATH from a file."
  (interactive)
  (condition-case err
      (progn (load "~/.emacsenv")
             (setq exec-path (split-string (getenv "PATH") ":")))
    (error (message (error-message-string err)))))

;;; Insert

(defvar luna-special-symbol-alist
  '(("(c)" . "©") ("tm" . "™")  ("s" . "§") ("em" . "—") 
    ("en" . "–") ("..." . "…") ("s<" . "⃖") ("s>" . "⃗") ("s^" . "ꜛ")
    ("sv" . "ꜜ") ("<" . "←") (">" . "→") ("^" . "↑") ("v" . "↓")
    ("l" . "‘") ("r" . "’") ("ll" . "“") ("rr" . "”") ("hand" . "☞")
    ;; paragraph separator
    ("p" . " ")
    ;; non-breaking space
    ("spc" . " ")
    ;; zero-width space
    ("0" . "​")
    ;; thin space
    ("thin" . " "))
  ;; don’t use tab character because we use that for splitting
  ;; in ‘luna-insert-special-symbol’
  "Alist used by `luna-insert-special-symbol'.")

(defun luna-insert-special-symbol (surname)
  "Insert special symbol at point, SURNAME is used to search for symbol.
E.g. SURNAME (c) to symbol ©."
  (interactive
   (list (car (split-string
               (completing-read
                "MAbbrev: "
                (mapcar (lambda (c)
                          (format "%s\t%s" (car c) (cdr c)))
                        luna-special-symbol-alist))
               "\t"))))
  (insert (alist-get surname luna-special-symbol-alist "" nil #'equal)))

(defun luna-make-accent-fn (name)
  "Return a command that insert “COMBINDING NAME” unicode char."
  (lambda () (interactive)
    (insert (char-from-name (concat "COMBINING " name)))))

;; (global-set-key (kbd "C-x 9 -") (luna-make-accent-fn "MACRON"))

;;; Auto insert

(defvar luna-autoinsert-template
  (luna-f-join user-emacs-directory "star/autoinsert-template.el")
  "The template file.")

(defun luna-autoinsert (description)
  "Autoinsert what auto-insert inserts.
With DESCRIPTION of the package."
  (interactive "MDescription: ")
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         ;; (year (format-time-string "%Y"))
         (feature (file-name-base (buffer-file-name)))
         (template (luna-f-content luna-autoinsert-template)))
    (insert (format template
                    filename description feature filename))))

;;; Cheat sheet

(defvar cheatsheet-file-dir (expand-file-name "cheatsheet"
                                              user-emacs-directory)
  "Under where you put the cheat sheets.")

(defvar cheatsheet-display-fn (lambda (txt) (message "%s" txt))
  "Function for displaying cheat sheet.")

(defun cheatsheet-display ()
  "Display cheat sheet for this major mode."
  (interactive)
  (let* ((mode-name (symbol-name major-mode))
         (file-path (expand-file-name mode-name cheatsheet-file-dir)))
    (condition-case nil
        (funcall cheatsheet-display-fn (luna-f-content file-path))
      (error (user-error "Cannot find cheat sheet for %s" major-mode)))))

(defalias 'helpme 'cheatsheet-display)

;;; ChangeLog

(defun copy-change-log ()
  (interactive)
  (let* ((fileset (cadr (vc-deduce-fileset t)))
         (changelog (cl-loop for file in fileset
                             do (progn (find-file file)
                                       (add-change-log-entry))
                             collect (buffer-string))))
    (when (string-match-p "changes to" (buffer-name)) (kill-buffer))
    (with-temp-buffer
      (dolist (log changelog)
        (insert log))
      (kill-ring-save (point-min) (point-max)))))

;;; Unfill

;; Copied straight from flywrap.el.
(defun unfill-region (beg end)
  "Remove newlines in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (search-forward "\n" end t)
      (unless
          ;; If we are at point-max, ‘char-after’ returns nil.
          (eq (point) (point-max))
        ;; Regarding the 'ascii: I can be more intelligent here
        ;; (include iso-latin, etc), but since the break point function
        ;; is from fill.el, better keep in sync with it. (see
        ;; ‘fill-move-to-break-point’).
        ;; Don’t remove consecutive newlines.
        (cond ((or (eq (char-before (1- (point))) ?\n)
                   (eq (char-after (point)) ?\n))
               nil)
              ;; Separate ascii characters with space
              ((and (eq (char-charset (char-before (1- (point)))) 'ascii)
	            (eq (char-charset (char-after (point))) 'ascii))
               (replace-match " "))
              ;; Don’t separate CJK characters.
              (t (replace-match "")))))))

;;; Windows newline

(defun windows-newline ()
  "Set file to use windows newline (\\r\\n)."
  (interactive)
  (set-buffer-file-coding-system 'dos))

;;; Kill

(defalias 'copy 'kill-new)

;;; Read key

(defun read-key-command ()
  "Read key."
  (interactive)
  (read-key-sequence "Key: "))

;;; Gif

(defun gif-animate ()
  "Animate Gif in buffer."
  (interactive)
  (when (derived-mode-p 'image-mode)
    (save-excursion
      (goto-char (point-min))
      (image-animate (plist-get (text-properties-at (point)) 'display)))))


;;; Prefix command

;; From Stefan M.
(defun copy-next-command-output ()
  "Prefix command to add the output of the next command to kill-ring."
  (interactive)
  (let ((md (minibuffer-depth))
        (marker (with-current-buffer "*Messages*"
                  (point-max-marker))))
    (cl-labels ((pre ()
                     (unless (> (minibuffer-depth) md)
                       (add-hook 'post-command-hook #'post)
                       (prepare)))
                (prepare ()
                         (with-current-buffer "*Messages*"
                           (move-marker marker (point-max))))
                (preserve ()
                          (unless (> (minibuffer-depth) md)
                            (remove-hook 'post-command-hook #'post)
                            (add-hook 'pre-command-hook #'pre)))
                (echo ()
                      (unless (> (minibuffer-depth) md)
                        "[copy-output]"))
                (post ()
                      (if (> (minibuffer-depth) md)
                          ;; Prepare, in case there's no pre-command-hook before
                          ;; the next post-command-hook.  E.g. in the case of
                          ;; execute-extended-command.
                          (prepare)
                        (remove-hook 'pre-command-hook #'pre)
                        (remove-hook 'post-command-hook #'post)
                        (remove-hook 'prefix-command-preserve-state-hook
                                     #'preserve)
                        (remove-hook 'prefix-command-echo-keystrokes-functions
                                     #'echo)
                        (prefix-command-update)
                        (with-current-buffer (marker-buffer marker)
                          (when (< marker (point-max))
                            (kill-new (buffer-substring marker (point-max)))))
                        (set-marker marker nil))))
      (add-hook 'prefix-command-preserve-state-hook #'preserve)
      (add-hook 'prefix-command-echo-keystrokes-functions #'echo)
      ;; (message "BEFORE: prefix-arg=%S current-prefix-arg=%S"
      ;;          prefix-arg current-prefix-arg)
      (prefix-command-preserve-state)
      ;; (message "AFTER: prefix-arg=%S current-prefix-arg=%S"
      ;;          prefix-arg current-prefix-arg)
      )))

;;; Provide

(provide 'utility)

;;; utility.el ends here


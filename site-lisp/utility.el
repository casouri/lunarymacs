;;; utility.el --- Utilities      -*- lexical-binding: t; -*-

;; (Inserted with ‘insert-function-summary’.)
;;
;; - ‘switch-buffer-same-major-mode’
;;     Switch buffer among those with the same major mode.
;; - ‘open-in-finder’
;;     Open ‘default-directory’ in Finder.
;; - ‘open-in-iterm’
;;     Open ‘default-directory’ iTerm.
;; - ‘luna-rename-file’
;;     Renames current buffer and file it is visiting.
;; - ‘luna-sudo-edit’
;;     Edit currently visited file as root.
;; - ‘luna-load-env’
;;     Load PATH and CPATH from a file.
;; - ‘luna-insert-special-symbol’
;;     Insert special symbol at point.
;; - ‘luna-make-accent-fn’
;;     Return a command that insert “COMBINDING NAME” unicode char.
;; - ‘luna-autoinsert’
;;     Autoinsert what auto-insert inserts.
;; - ‘cheatsheet-display’
;;     Display cheat sheet for this major mode.
;; - ‘copy-change-log’
;;     No docstring.
;; - ‘unfill-region’
;;     Remove newlines in the region from BEG to END.
;; - ‘windows-newline’
;;     Set file to use windows newline (\r\n).
;; - ‘read-key-command’
;;     Read key.
;; - ‘gif-animate’
;;     Animate Gif in buffer.
;; - ‘copy-next-command-output’
;;     Prefix command to add the output of the next command to kill-ring.
;; - ‘uuid’
;;     Insert an UUID.
;; - ‘new-bin’
;;     Create a new command with NAME under ~/bin.
;; - ‘insert-function-summary’
;;     Insert a summary of all the functions defined in this file.
;; - ‘lineup’
;;     Align columns by whitespace.
;; - ‘switch-buffer-same-major-mode’
;;     Switch buffer among those with the same major mode.
;; - ‘open-in-finder’
;;     Open ‘default-directory’ in Finder.
;; - ‘open-in-iterm’
;;     Open ‘default-directory’ iTerm.
;; - ‘luna-rename-file’
;;     Renames current buffer and file it is visiting.
;; - ‘luna-sudo-edit’
;;     Edit currently visited file as root.
;; - ‘luna-load-env’
;;     Load PATH and CPATH from a file.
;; - ‘luna-insert-special-symbol’
;;     Insert special symbol at point.
;; - ‘luna-make-accent-fn’
;;     Return a command that insert “COMBINDING NAME” unicode char.
;; - ‘luna-autoinsert’
;;     Autoinsert what auto-insert inserts.
;; - ‘cheatsheet-display’
;;     Display cheat sheet for this major mode.
;; - ‘copy-change-log’
;;     No docstring.
;; - ‘unfill-region’
;;     Remove newlines in the region from BEG to END.
;; - ‘dos-newline’
;;     Set file to use windows newline (\r\n).
;; - ‘read-key-command’
;;     Read key.
;; - ‘gif-animate’
;;     Animate Gif in buffer.
;; - ‘copy-next-command-output’
;;     Prefix command to add the output of the next command to kill-ring.
;; - ‘uuid’
;;     Insert an UUID.
;; - ‘new-bin’
;;     Create a new command with NAME under ~/bin.
;; - ‘insert-function-summary’
;;     Insert a summary of all the functions defined in this file.
;; - ‘lineup’
;;     Align columns by whitespace.
;; - ‘better-quit-window’
;;     Quit from current window.
;; - ‘regexp-count’
;;     Count the number of occurrences that matches REGEXP.

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
    ("en" . "–") ("ddd" . "…") ("s<" . "⃖") ("s>" . "⃗") ("s^" . "ꜛ")
    ("sv" . "ꜜ") ("<" . "←") (">" . "→") ("^" . "↑") ("v" . "↓")
    ("l" . "‘") ("r" . "’") ("ll" . "“") ("rr" . "”") ("hand" . "☞")
    ;; paragraph separator
    ("p" . " ")
    ;; non-breaking space
    ("spc" . " ")
    ;; zero-width space
    ("0" . "​")
    ;; thin space
    ("thin" . " ")
    ;; non-breaking hyphen
    ("n-" . "‑"))
  ;; don’t use tab character because we use that for splitting
  ;; in ‘luna-insert-special-symbol’
  "Alist used by `luna-insert-special-symbol'.")

(defun luna-insert-special-symbol (surname)
  "Insert special symbol at point.
SURNAME is used to search for symbol. E.g. SURNAME (c) to symbol ©."
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

;;; Misc commands

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

(defun dos-newline ()
  "Set file to use windows newline (\\r\\n)."
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defalias 'copy 'kill-new)

(defun read-key-command ()
  "Read key."
  (interactive)
  (read-key-sequence "Key: "))

(defun gif-animate ()
  "Animate Gif in buffer."
  (interactive)
  (when (derived-mode-p 'image-mode)
    (save-excursion
      (goto-char (point-min))
      (image-animate (plist-get (text-properties-at (point)) 'display)))))

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

(defun uuid ()
  "Insert an UUID."
  (interactive)
  (insert (string-trim (shell-command-to-string "uuid"))))

(defun new-bin (name)
  "Create a new command with NAME under ~/bin."
  (interactive "sName: ")
  (find-file (concat "~/bin/" name))
  (insert "#!/usr/bin/env bash\n")
  (save-buffer)
  (shell-script-mode)
  (require 'executable)
  (executable-make-buffer-file-executable-if-script-p))

(defun insert-function-summary ()
  "Insert a summary of all the functions defined in this file."
  (interactive)
  (let ((found nil)
        (start (point)))
    (save-excursion
      (while (re-search-forward
              (rx bol "(defun " (group (+ (not (any " "))))) nil t)
        (push (match-string-no-properties 1) found)))
    (dolist (sym (reverse found))
      (insert (format "- ‘%s’\n    %s\n" sym
                      (car (split-string
                            (or (documentation (intern sym))
                                "No docstring.")
                            "\n")))))
    (comment-region start (point))))

(defun lineup (start end)
  "Align columns by whitespace."
  (interactive "r")
  ;; From emacs-devel.
  (align-regexp start end "\\(\\s-*\\)\\s-" 1 0 t))

(defun better-quit-window ()
  "Quit from current window.
If this window used to display another buffer with different
major mode as the current one, switch to that buffer; if not,
delete the window."
  (interactive)
  (cl-loop for buffer-info in (window-prev-buffers)
           for buffer = (car buffer-info)
           ;; If the buffer has different major mode, switch to it.
           if (not (eq (buffer-local-value 'major-mode buffer)
                       major-mode))
           do (switch-to-buffer buffer)
           and return nil
           finally (delete-window)))

(defun regexp-count (regexp)
  "Count the number of occurrences that matches REGEXP.
Count from point to end of buffer."
  (interactive "sRegexp: ")
  (save-excursion
    (let ((count 0))
      (while (re-search-forward regexp nil t)
        (cl-incf count))
      (message "%s occurrences of %s" count regexp))))

(defun regexp-collect (regexp group)
  "Collect matches of REGEXP to kill-ring.
GROUP is the group number of the match you want to collect. Use 0
for whole match."
  (interactive "sRegexp: \nnGroup: ")
  (save-excursion
    (let ((string "")
          (count 0))
      (while (re-search-forward regexp nil t)
        (let ((match (match-string group)))
          (when (null match)
            (user-error
             "Cannot find the matched group, is group number correct?"))
          (setq string (concat string "\n" match))
          (cl-incf count)))
      (with-temp-buffer
        (insert string)
        (kill-ring-save (point-min) (point-max)))
      (message "Collected %s occurrences of %s to kill-ring"
               count regexp))))

(defun insert-webpage-ref (url)
  "Insert a URL with its title."
  (interactive (list (read-string "URL: " (current-kill 0) nil)))
  (require 'dom)
  (let (title dom)
    (with-current-buffer (url-retrieve-synchronously url)
      (setq dom (libxml-parse-html-region (point-min) (point-max)))
      (setq title (cl-caddr (car (dom-by-tag dom 'title)))))
    (insert (replace-regexp-in-string
             "[ \t\n\r]+" " "
             (string-trim (string-replace "\n" " " title)))
            "\n"
            url)))

(defun filter-lines (regexp)
  "Show only lines that match REGEXP.
Like ‘keep-lines’ but only hides non-matching lines. So this
function also works for read-only buffers."
  (interactive "sRegexp: ")
  )

(provide 'utility)

;;; utility.el ends here


;;; bklink.el --- Poor man's back-link      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; In org-roam, you can show back-links -- a list of files that
;; contains a link to the current file. The idea is nice but I don’t
;; like how org-roam and zetteldeft implement things. Hence this
;; minimal package.
;;
;; Some assumptions about the files that contains bklink:
;;
;;  1. All the files are in the same directory. So we can use filename
;;     to uniquely identify files.
;;  2. There is no sub-directories.
;;  3. There aren’t a ton of files. We use grep to search for links,
;;     no caching.
;;  4. filenames don’t contain “]]”.
;;
;; Advantages:
;;
;;  1. Filename as link, no id, no database.
;;  2. Works for multiple directories without configuration.
;;  3. Works for any file format.
;;  4. Back-link buffer follows the main buffer automatically.
;;
;; To use with Deft:
;;
;;     (add-hook 'deft-open-file-hook #'bklink-minor-mode)
;;     (setq deft-use-filter-string-for-filename t)
;;
;; Usage:
;;
;; Insert a link to another file:
;;
;;     M-x bklink-insert RET
;;
;; Show back-links:
;;
;;     M-x bklink-show-back-link RET
;;
;; Rename all links to the current file:
;;
;;     M-x bklink-rename new-name RET
;;
;; And then manually rename the current file.

;;; Code:

(require 'cl-lib)

;;; Backstage

(defvar bklink-regexp (rx (seq (group "[[")
                               (group (+? (not (any "/\n"))))
                               (group (? (or ".txt" ".org" ".md")))
                               (group "]]")))
  "Regular expression that matches a bklink.

Group 1 is opening delimiter.
Group 2 is base filename.
Group 3 is filename extension (if exists).
Group 4 is ending delimiter.

Change this variable and
`bklink--format-link' to change link format.")

(defsubst bklink--format-link (file)
  "Format FILE into a bklink. Basically [[FILE]]."
  (format "[[%s]]" file))

(defun bklink--get-file-list (file)
  "Return a list of files that’s in the same project with FILE.
Ignore dotfiles and directories."
  (cl-remove-if (lambda (f) (or (string-prefix-p "." f)
                                (file-directory-p f)))
                (directory-files (file-name-directory file))))

(defsubst bklink--format-back-link-buffer (buffer)
  "Format buffer name for the back-link for BUFFER."
  ;; We use buffer to generate new names to avoid name conflict:
  ;; files in different directories can have the same name.
  (format " *back-links for %s*" (buffer-name buffer)))

;;;; Link button

(define-button-type 'bklink
  'action #'bklink-follow-link
  'filename nil
  'follow-link t
  'use-window nil
  'help-echo "Jump to file")

(defun bklink-follow-link (button)
  "Jump to the file that BUTTON represents."
  (with-demoted-errors "Error when following the link: %s"
    (let ((file (button-get button 'filename)))
      ;; If we are in a back-link buffer, open the file in the
      ;; main buffer and delete back-link window.
      (if bklink--minion-p
          (if-let ((win (get-buffer-window bklink--master))
                   (bklink-win (selected-window)))
              (progn (select-window win)
                     (find-file file)
                     (set-window-parameter
                      bklink-win 'window-atom nil)
                     (delete-window bklink-win))
            (error
             "Cannot find the main buffer of this back-link buffer"))
        (find-file file)))
    (bklink-minor-mode)))

;;;; Highlight links

(define-minor-mode bklink-minor-mode
  "Recognizes bklinks."
  :lighter ""
  :keymap (make-sparse-keymap)
  (if bklink-minor-mode
      (jit-lock-register #'bklink-fontify)
    (jit-lock-unregister #'bklink-fontify))
  (bklink-managed-mode)
  (jit-lock-refontify))

(defun bklink-fontify (beg end)
  "Highlight bklinks between BEG and END."
  (goto-char beg)
  ;; FIXME: What if END is in the middle of a link?
  (while (re-search-forward bklink-regexp end t)
    ;; Hide opening and closing delimiters and file extension.
    (put-text-property (match-beginning 1) (match-end 1)
                       'display "“")
    (put-text-property (match-beginning 4) (match-end 4)
                       'display "”")
    (when (match-beginning 3)
      (put-text-property (match-beginning 3) (match-end 3) 'invisible t))
    ;; Highlight link.
    (make-text-button (match-beginning 0)
                      (match-end 0)
                      :type 'bklink
                      'filename (concat (match-string-no-properties 2)
                                        (or (match-string-no-properties 3)
                                            "")))))

;;;; Back-links

(defun bklink--get-linked-files (file callback)
  "Call CALLBACK with a list of files’ name that has a link to FILE."
  (let* ((name (generate-new-buffer-name " *bklink grep*"))
         (process (start-process
                   name name "grep" "-rlF"
                   (bklink--format-link (file-name-nondirectory file))
                   (file-name-directory file)))
         ;; When the grep process finishes, we parse the result files
         ;; and call CALLBACK with them.
         (sentinal
          (lambda (process event)
            (if (string-match-p (rx (or "finished" "exited"))
                                event)
                (if-let ((buf (process-buffer process)))
                    (with-current-buffer buf
                      (let ((files (split-string (buffer-string) "\n")))
                        (funcall callback
                                 (mapcar #'file-name-nondirectory
                                         (remove "" files)))))
                  (error "Bklink’s grep process’ buffer is killed"))
              (error "Bklink’s grep process failed with signal: %s"
                     event)))))
    (set-process-sentinel process sentinal)))

;;;; Manage back-link window
;;
;; The back-link buffer (minion) follows its document buffer (master).
;; If the master moves, the minion moves with it.

(defvar-local bklink--minion-p nil
  "Non-nil means this buffer is a back-link buffer.")

(defvar-local bklink--master nil
  "The main buffer for this back-link buffer.")

(defvar-local bklink--show-back-link nil
  "If non-nil, show the back-link buffer for this buffer.
Only used by the toggle function.")

(defun bklink--share-parent-any (win win-list)
  "Return non-nil if WIN and any window in WIN-LIST shares parent."
  (cl-labels ((share-parent (a b) (eq (window-parent a)
                                      (window-parent b))))
    (cl-loop for w in win-list
             if (share-parent win w)
             return t
             finally return nil)))

(defun bklink--display-minion (minion master-window)
  "Display MINION buffer next to MASTER-WINDOW."
  (let ((window (display-buffer-in-atom-window
                 minion `((side . below) (window . ,master-window)))))
    (set-window-dedicated-p window t)
    (fit-window-to-buffer window nil nil nil nil t)))

(defun bklink--delete-minion-window (minion-window)
  "Delete MINION-WINDOW."
  (set-window-parameter minion-window 'window-atom nil)
  (delete-window minion-window))

(defun bklink--sync-window (_)
  "Show/delete back-link window for document buffer accordingly."
  (dolist (buf (buffer-list))
    ;; BUF is either a minion or a master.
    (when (buffer-local-value 'bklink-minor-mode buf)
      (if (buffer-local-value 'bklink--minion-p buf)
          ;; For a back-link buffer MINION, make sure it’s next to
          ;; it MASTER. If not, delete MINION’s window.
          (let* ((minion buf)
                 (minion-windows (get-buffer-window-list minion))
                 (master (buffer-local-value 'bklink--master minion))
                 (master-windows (get-buffer-window-list master)))
            (dolist (minion-window minion-windows)
              (unless (bklink--share-parent-any
                       minion-window master-windows)
                (bklink--delete-minion-window minion-window))))
        ;; For a main buffer MASTER, make sure it has a back-link
        ;; buffer (MINION) next to it.
        (let* ((master buf)
               (master-windows (get-buffer-window-list master))
               (minion (get-buffer (bklink--format-back-link-buffer
                                    master)))
               (minion-windows (get-buffer-window-list minion)))
          (when minion
            (dolist (master-window master-windows)
              (unless (bklink--share-parent-any
                       master-window minion-windows)
                (bklink--display-minion minion master-window)))))))))

(define-minor-mode bklink-managed-mode
  "Manage back-link windows automatically."
  :global t
  :lighter ""
  (if bklink-managed-mode
      (add-hook 'window-buffer-change-functions
                #'bklink--sync-window)
    (remove-hook 'window-buffer-change-functions
                 #'bklink--sync-window)))

;;; Userland

(defun bklink-insert (file)
  "Insert a link to FILE.
If called interactively, prompt for the file."
  (interactive
   (list (completing-read
          "File: " (bklink--get-file-list (buffer-file-name)))))
  (insert (bklink--format-link file))
  (bklink-minor-mode))

(defun bklink-toggle-back-link ()
  "Toggle display of a buffer that show back-links.
The back-links are links to the files that has a link to this file."
  (interactive)
  (unless (executable-find "grep")
    (user-error "Displaying back-link needs grep but we cannot find it"))
  (bklink-minor-mode)
  (setq bklink--show-back-link (not bklink--show-back-link))
  (if bklink--show-back-link
      ;; Show.
      ;; MASTER is this buffer, MINION is the back-link buffer.
      (if-let ((file (buffer-file-name)))
          (let ((master (current-buffer))
                (minion (get-buffer-create (bklink--format-back-link-buffer
                                            (current-buffer)))))
            ;; Fire a sub-process to retrieve back-links.
            (bklink--get-linked-files
             file (lambda (file-list)
                    (with-current-buffer minion
                      (erase-buffer)
                      (if (null file-list)
                          (insert "No back-links found")
                        (dolist (file file-list)
                          (bklink-insert file)
                          (insert "\n"))))))
            ;; Initialize the back-link buffer...
            (with-current-buffer minion
              ;; Normally it’s pretty fast, so this just creates flicker.
              ;; (insert "Waiting for search to finish...")
              ;; We use ‘bklink-minor-mode’ to identify back-link
              ;; buffers.
              (bklink-minor-mode)
              (setq mode-line-format nil
                    bklink--minion-p t
                    bklink--master master))
            ;; ... and display the buffer.
            (bklink--sync-window nil))
        (user-error "This buffer is not associated with any file"))
    ;; Hide.
    (when-let ((minion (get-buffer (bklink--format-back-link-buffer
                                    (current-buffer)))))
      (dolist (win (get-buffer-window-list minion))
        (bklink--delete-minion-window win))
      ;; We kill the buffer so that ‘bklink--sync-window’ doesn’t
      ;; bring it up automatically.
      (kill-buffer minion))))

(defun bklink-rename (new-name)
  "Rename current file to NEW-NAME.
Rename bklinks that points to the current file point to NEW-NAME.
This command only rename links, you need to manually rename the
current file."
  ;; We run grep to find files that contain the old link, save them to
  ;; /tmp/bklink-rename, and use emacs to replace old links with new
  ;; links in each file. I can’t believe there is no painless way to
  ;; replace literal strings in sed.
  (interactive
   (let ((name (file-name-nondirectory (buffer-file-name))))
     (list (completing-read
            (format "Rename %s to: " name) nil nil name))))
  (unless (executable-find "grep")
    (user-error "Rename needs grep but we cannot find it"))
  (unless (executable-find "emacs")
    (user-error "Rename needs Emacs in PATH but we cannot find it"))
  (when (file-exists-p new-name)
    (user-error "Cannot rename to %s, this file already exists" new-name))
  (when (yes-or-no-p (format "Replace %s to %s?"
                             (file-name-nondirectory
                              (buffer-file-name))
                             new-name))
    (let* ((old-link (bklink--format-link (file-name-nondirectory
                                           (buffer-file-name))))
           (new-link (bklink--format-link new-name))
           (tmp-file (concat "/tmp/bklink-rename-"
                             (format-time-string "%s")))
           (command
            (concat
             (format "grep -rlF '%s' %s > %s"
                     old-link default-directory tmp-file)
             (format "; emacs --batch -l '%s'"
                     (find-library-name "bklink"))
             (format
              " --eval '(bklink--process-rename \"%s\" \"%s\" \"%s\")'"
              old-link new-link tmp-file))))
      (start-process-shell-command
       "bklink rename" " *bklink rename*" command)
      (message "Replacing %s with %s in the background"
               old-link new-link))))

(defun bklink--process-rename (old-link new-link path-file)
  "Replace OLD-LINK with NEW-LINK.
The files to replace are in PATH-FILE"
  (with-temp-buffer
    (insert-file-contents path-file)
    (dolist (file (split-string (buffer-string) "\n"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (search-forward old-link nil t)
          (replace-match new-link))
        (write-file file)))))

(provide 'bklink)

;;; bklink.el ends here

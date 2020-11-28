;;; bklink.el --- Poor man's back-link      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; In org-roam, you can show back links -- a list of files that
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
;;     no caching (though not hard to add one).
;;  4. filenames don’t contain “}]”.
;;
;; Advantages:
;;
;;  1. Filename as link, no id, no database.
;;  2. Works for multiple directories without configuration.
;;  3. Works for any file format.
;;  4. Back-link summary is not saved to the file.
;;
;; To use with Deft:
;;
;;     (add-hook 'deft-open-file-hook #'bklink-minor-mode)
;;     (setq deft-use-filter-string-for-filename t)
;;
;; Usage:
;;
;; Link format is [{filename.ext}]. Extension is optional.
;; Links are displayed as [filename] with 'link face.
;;
;; Insert a link to another file or edit a link:
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
;;
;; Notes:
;;
;; - If a link doesn’t have extension, bklink appends “.txt” to it
;;   when searching for the corresponding file.
;; - If the file a link points to doesn’t exist, bklink doesn’t create
;;   that file. Instead, bklink creates a buffer, and you can show
;;   back-links to it without creating that file.
;; - When ‘bklink-show-back-link-on-start’ is non-nil, back-link
;;   summary is shown on startup.
;; - When ‘bklink-more-match’ is non-nil, bklink also shows files that
;;   include the phrase but not as a link.
;; - When ‘bklink-use-form-feed’ is non-nil, bklink uses form-feed
;;   character to delineate back-link summary.

;;; Code:

(require 'cl-lib)
;; For `with-buffer-modified-unmodified'.
(require 'bookmark)

;;; Customize

(defvar bklink-use-form-feed t
  "If non-nil, use form-feed instead of dashes.")

(defvar bklink-show-back-link-on-start t
  "If non-nil, show back-links when `bklink-minor-mode' starts.")

(defvar bklink-more-match nil
  "If non-nil, bklink includes more matches in the back-link summary.

Besides explicitly links, we also include text that matches the
title but isn't a link.

For example, for Emacs.txt, we match not only [{Emacs.txt}],
but also Emacs.")

;;; Backstage

(defvar bklink-regexp (rx (seq (group "[{")
                               (group (+? (not (any "/"))))
                               (group (? (or ".txt" ".org" ".md")))
                               (group "}]")))
  "Regular expression that matches a bklink.

Group 1 is opening delimiter.
Group 2 is base filename.
Group 3 is filename extension (if exists).
Group 4 is ending delimiter.

Change this variable and
`bklink--format-link' to change link format.")

(defsubst bklink--format-link (file)
  "Format FILE into a bklink. Basically [[FILE]]."
  (format "[{%s}]" file))

(defun bklink--get-file-list (file)
  "Return a list of files that’s in the same project with FILE.
Ignore dotfiles and directories."
  (cl-remove-if (lambda (f) (or (string-prefix-p "." f)
                                (file-directory-p f)))
                ;; FILE could be a file-less buffer.
                (directory-files (or (file-name-directory file)
                                     default-directory))))

(defun bklink--search-at-point ()
  "Search for links at point and set match data accordingly.
Return t if found, nil otherwise. See `bklink--regepx' for
groups."
  (save-excursion
    (catch 'found
      (let ((p (point)))
        (beginning-of-line)
        (while (re-search-forward bklink-regexp (line-end-position) t)
          (if (<= (match-beginning 0) p (match-end 0))
              (throw 'found t)))))))

(defun bklink--file-at-point ()
  "Return the filename of the link at point.
Return nil if not found."
  (bklink--search-at-point)
  (buffer-substring-no-properties (match-beginning 2) (match-end 3)))

(defun bklink--set-file-at-point (name)
  "Set the file name of the link at point to NAME.
Do nothing if there is no link at point."
  (bklink--search-at-point)
  (save-excursion
    (goto-char (match-beginning 2))
    (delete-region (match-beginning 2) (match-end 3))
    (insert name)))

;;;; Link button

(define-button-type 'bklink
  'action #'bklink-follow-link
  'filename nil
  'follow-link t
  'use-window nil
  'help-echo "Jump to file")

;; Please the byte compiler.
(defvar bklink-minor-mode)
(defun bklink-follow-link (button)
  "Jump to the file that BUTTON represents.
If the file doesn't exist, create a buffer."
  (with-demoted-errors "Error when following the link: %s"
    (let* ((file (button-get button 'filename))
           (fullname (if (file-name-extension file)
                         file
                       (concat file ".txt"))))
      (if (file-exists-p fullname)
          (find-file fullname)
        (switch-to-buffer (get-buffer-create fullname))
        (insert file "\n\n")))
    (when (eq major-mode 'fundamental-mode)
      (text-mode))
    (unless bklink-minor-mode
      (bklink-minor-mode))))

(define-button-type 'bklink-url
  'action #'bklink-browse-url
  'url nil
  'follow-link t
  'help-echo "Open URL")

(defun bklink-browse-url (button)
  "Open URL in BUTTON."
  (browse-url (button-get button 'url)))

;;;; Highlight links

(define-minor-mode bklink-minor-mode
  "Recognizes bklinks."
  :lighter ""
  :keymap (make-sparse-keymap)
  (if bklink-minor-mode
      (progn (jit-lock-register #'bklink-fontify)
             (unless (derived-mode-p 'org-mode 'markdown-mode)
               (jit-lock-register #'bklink-fontify-url))
             (add-hook 'write-file-functions
                       #'bklink--write-file-function 90 t)
             ;; (add-hook 'fill-nobreak-predicate #'bklink--nobreak-p 90 t)
             (if (and bklink-show-back-link-on-start
                      (not bklink-show-back-link))
                 (bklink-show-back-link)))
    (jit-lock-unregister #'bklink-fontify)
    (jit-lock-unregister #'bklink-fontify-url)
    ;; (remove-hook 'fill-nobreak-predicate #'bklink--nobreak-p t)
    (with-silent-modifications
      (put-text-property (point-min) (point-max) 'display nil)))
  (jit-lock-refontify))

(defun bklink-fontify (beg end)
  "Highlight bklinks between BEG and END."
  (goto-char beg)
  (while (and (re-search-forward bklink-regexp nil t)
              (< (point) end))
    (let ((inhibit-read-only t))
      ;; Hide opening and closing delimiters and file extension.
      (with-silent-modifications
        ;; (put-text-property (match-beginning 0) (match-end 0)
        ;;                    'bklink-no-break t)
        (add-text-properties
         (match-beginning 1) (match-end 1)
         '(display "[" font-lock-face shadow face shadow))
        (add-text-properties
         (match-beginning 4) (match-end 4)
         '(display "]" font-lock-face shadow face shadow))
        (when (match-beginning 3)
          (put-text-property (match-beginning 3)
                             (match-end 3) 'invisible t))
        (put-text-property (match-beginning 0) (match-end 0)
                           'rear-nonsticky t)
        ;; Highlight link.
        (make-text-button (match-end 1)
                          (match-beginning 4)
                          :type 'bklink
                          'filename (concat
                                     (match-string-no-properties 2)
                                     (or (match-string-no-properties 3)
                                         "")))))))

(defun bklink-fontify-url (beg end)
  "Add clickable buttons to URLs between BEG and END.
Everything that matches `browse-url-button-regexp' will be made
clickable and will use `browse-url' to open the URLs in question."
  ;; Change face to font-lock-face.
  (goto-char beg)
  (while (re-search-forward browse-url-button-regexp end t)
    (make-text-button (match-beginning 0)
                      (match-end 0)
                      :type 'bklink-url
                      'url (match-string-no-properties 0))))

;; (defun bklink--nobreak-p ()
;;   "Return non-nil if shouldn't break at point."
;;   (text-property-any
;;    (max (1- (point)) (point-min)) (point) 'bklink-no-break t))

;;;; Back-link summary

(defvar bklink--back-link-regexp
  (rx (seq "\n" (or "\x0C" (= 70 "-")) "\n"
           ;; Non-greedy is important: otherwise we risk of
           ;; regexp stack overflow. That happened for buffers
           ;; when iimg data.
           (+? digit) " linked references to " (+? anything)
           (or "\x0C" (= 70 "-")) "\n"))
  "Regular expression that matches the beginning of a summary.")

(defun bklink--prune-back-link-summary ()
  "Remove back-links before save."
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    ;; Remove summary.
    (when (re-search-forward bklink--back-link-regexp nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun bklink--insert-back-link-summary (files buffer this-file)
  "Append back-link summary to BUFFER.
FILES is a list of filenames that contains the link.
THIS-FILE is the filename we are inserting summary into."
  (with-current-buffer buffer
    (save-excursion
      (with-buffer-modified-unmodified
       (bklink--prune-back-link-summary)
       (goto-char (point-max))
       (let* ((summary-start (point))
              (this-link (bklink--format-link this-file))
              (this-link-re
               (if bklink-more-match
                   ;; Prevent matching base64 string in iimg.
                   (concat (rx (any " \n{"))
                           (replace-regexp-in-string
                            " " "[ \n]*"
                            (file-name-base this-file))
                           (rx (any " \n}")))
                 (replace-regexp-in-string
                  " " "[ \n]*" this-link)))
              ;; A list of (FILE . SUMMARY). The grep search didn't
              ;; match against the complete link and we need to filter
              ;; out the false-positives here.
              (summary-list
               (mapcar
                (lambda (file)
                  (if (not (equal file this-file))
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (if (re-search-forward this-link-re nil t)
                            (when-let ((summary
                                        (or (if (org-at-table-p)
                                                (thing-at-point 'line)
                                              (thing-at-point 'sentence))
                                            "(No summary)")))
                              (cons file summary))))))
                files))
              (summary-list (remove nil summary-list))
              (inhibit-read-only t))
         ;; Insert separator.
         (insert "\n"
                 (if bklink-use-form-feed "\x0C" (make-string 70 ?-))
                 "\n"
                 ;; Plural might not be correct, but I don't care.
                 (format "%d linked references to %s:\n"
                         (length summary-list) this-link))
         (dolist (summary summary-list)
           (insert "\n")
           ;; Insert file link.
           (insert (bklink--format-link (car summary)) ":\n")
           ;; Insert surrounding sentence. We only get the first
           ;; sentence. Don’t fill the paragraph, filling might break
           ;; the layout of the original text.
           (let ((beg (point)))
             (insert (cdr summary))
             (indent-rigidly beg (point) 2))
           (insert "\n"))
         (insert (if bklink-use-form-feed "\x0C" (make-string 70 ?-)))
         (put-text-property summary-start (point) 'read-only t)
         ;; Add an non-read-only newline so the user and other
         ;; commands can append text at the end of the file.
         (insert "\n")
         (bklink-fontify summary-start (point)))))))

(defun bklink--write-file-function ()
  "Write to file without the back-links."
  (save-excursion
    (let ((this-buffer (current-buffer))
          (this-file (buffer-file-name)))
      (with-temp-buffer
        (insert-buffer-substring this-buffer)
        (bklink--prune-back-link-summary)
        (write-region (point-min) (point-max) this-file))
      (clear-visited-file-modtime)
      (set-buffer-modified-p nil)
      t)))

;;;; Retrieve back-links

(defun bklink--get-linked-files (file callback)
  "Call CALLBACK with a list of filenames that has a link to FILE."
  (let* ((name (generate-new-buffer-name " *bklink grep*"))
         (process (apply
                   #'start-process
                   name name "grep" "-ilF"
                   ;; The link could span multiple lines (because of
                   ;; filling), so we only search for the part before
                   ;; first space as a preliminary filter. We later do
                   ;; an accurate search in
                   ;; `bklink--insert-back-link-summary'.
                   (car (split-string
                         (if bklink-more-match
                             (file-name-base file)
                           (bklink--format-link
                            (file-name-nondirectory file)))))
                   (bklink--get-file-list file)))
         ;; When the grep process finishes, we parse the result files
         ;; and call CALLBACK with them.
         (sentinal
          (lambda (process event)
            (if (string-match-p (rx (or "finished" "exited"))
                                event)
                (if-let ((buf (process-buffer process)))
                    (unwind-protect
                        (with-current-buffer buf
                          (let ((files (split-string
                                        (buffer-string) "\n")))
                            (funcall callback
                                     (mapcar #'file-name-nondirectory
                                             (remove "" files)))))
                      (kill-buffer buf))
                  (error "Bklink’s grep process’ buffer is killed"))
              (error "Bklink’s grep process failed with signal: %s"
                     event)))))
    (set-process-sentinel process sentinal)))

;;; Userland

(defun bklink-insert ()
  "Insert a link to a file.
If point not on a link, insert a new link, if already on a link,
edit the link."
  (interactive)
  (if (bklink--search-at-point)
      (let ((file (completing-read
                   "New file: "
                   (mapcar #'file-name-base
                           (bklink--get-file-list (buffer-file-name)))
                   nil nil (bklink--file-at-point))))
        (bklink--set-file-at-point file))
    (let ((file (completing-read
                 "File: "
                 (mapcar #'file-name-base
                         (bklink--get-file-list (buffer-file-name))))))
      (insert (bklink--format-link file))))
  (bklink-minor-mode))

(define-minor-mode bklink-show-back-link
  "Toggle display of a buffer that show back-links.
The back-links are links to the files that has a link to this file."
  :lighter ""
  (unless (executable-find "grep")
    (user-error "Displaying back-link needs grep but we cannot find it"))
  (unless bklink-minor-mode
    (user-error "Bklink-minor-mode is not on"))
  (if bklink-show-back-link
      ;; The buffer could be not having a file.
      (if-let ((file (or (buffer-file-name) (buffer-name)))
               (buffer (current-buffer)))
          ;; Fire a sub-process to retrieve back-links.
          (bklink--get-linked-files
           file (lambda (file-list)
                  (bklink--insert-back-link-summary
                   file-list buffer (file-name-nondirectory file))))
        (user-error "This buffer is not associated with any file"))
    (with-buffer-modified-unmodified
     (save-excursion
       (bklink--prune-back-link-summary)))))

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
             (format "grep -ilF '%s' %s > %s"
                     (car (split-string old-link))
                     default-directory tmp-file)
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
        (let ((link-re (string-join
                        (mapcar #'regexp-quote
                                (split-string old-link))
                        "[ \n]*")))
          (while (re-search-forward link-re nil t)
            (replace-match new-link)))
        (write-file file)))))

(provide 'bklink)

;;; bklink.el ends here

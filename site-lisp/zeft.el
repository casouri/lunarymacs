;;; zeft.el --- Yet another Deft      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Why I need to write another Deft:
;;
;; Deft decides to capture user inputs and construct the search phrase
;; itself. This allows the point to be anywhere in the buffer while
;; updating the search phrase. That prohibits any Emacs input method
;; from working. And I can’t select the search phrase like normal
;; buffer text
;; 
;; There are other motivations, like the desire for a interface
;; similar to Apple’s Note.app, where each file comes with a
;; multi-line excerpt.
;;
;; Usage:
;;
;; Type M-x zeft RET, and you should see the Zeft buffer. Type in your
;; search phrase in the first line and the result will show up as you
;; type. Press C-n and C-p to go through each file. You can preview a
;; file by pressing SPC when the point is on a file, or click the file
;; with the mouse. Press RET to open the file in the same window.
;;
;; Type C-c C-g to force a refresh. When point is on the search
;; phrase, press RET to create a file with the search phrase as
;; filename and title.
;;
;; Note that:
;;
;; 1. Zeft only looks for first-level files in ‘zeft-directory’. That is,
;;    files in sub-directories are not searched.
;;
;; 2. Zeft creates new files by using the search phrase as the
;;    filename and title, if you want otherwise, redefine
;;    ‘zeft-create-note’.
;;
;; 3. Zeft saves the current window configuration before switching to
;;    Zeft buffer. When Zeft buffer is killed, Zeft restores the saved
;;    window configuration.
;;
;; 4. Zeft only display the first 20 files when the search phrase is
;;    empty, if you want to see all the files, force a refresh by
;;    typing C-c C-g.

;;; Code:

(require 'cl-lib)

;;; Customize

(defgroup zeft
  '((zeft-directory custom-variable)
    (zeft-find-file-hook custom-variable)
    (zeft-selection custom-variable)
    (zeft-inline-highlight custom-face)
    (zeft-preview-highlight custom-face)
    (zeft-load-file-hook custom-variable))
  "Zeft note interface."
  :group 'applications)

(defcustom zeft-directory (expand-file-name "~/.deft")
  "Directory in where notes are stored. Must be a full path."
  :type 'directory
  :group 'zeft)

(defcustom zeft-find-file-hook nil
  "Hook run when Zeft opens a file."
  :type 'hook
  :group 'zeft)

(defface zeft-selection
  '((t . (:inherit region :extend t)))
  "Face for the current selected search result."
  :group 'zeft)

(defface zeft-inline-highlight
  '((t . (:inherit underline :extend t)))
  "Face for inline highlighting in Zeft buffer."
  :group 'zeft)

(defface zeft-preview-highlight
  '((t . (:inherit highlight :extend t)))
  "Face for highlighting the preview buffer."
  :group 'zeft)

(defcustom zeft-load-file-hook nil
  "Functions run before zeft loads a file into database."
  :type 'hook
  :group 'zeft)

;;; Helpers

(defvar zeft--last-window-config nil
  "Window configuration before Zeft starts.")

(defvar zeft--database nil
  "An alist of (FILENAME . FILE-CONTENT).")

(defun zeft--work-buffer ()
  "Return the work buffer for Zeft. Used for holding file contents."
  (get-buffer-create " *zeft work*"))

(defun zeft--buffer ()
  "Return zeft buffer."
  (get-buffer-create "*zeft*"))

(defun zeft--after-save-hook ()
  "Update ‘zeft--database’."
  (when (equal zeft-directory default-directory)
    (zeft--save-to-database
     (buffer-file-name) (zeft--work-buffer))))

(defun zeft--save-to-database (file &optional work-buffer)
  "Save FILE to ‘zeft--database’.
WORK-BUFFER is the buffer used to store file contents
temporarily, if nil, defaults to the current buffer. Return the
saved file content."
  (with-current-buffer (or work-buffer (current-buffer))
    (erase-buffer)
    (insert-file-contents file)
    (run-hooks 'zeft-load-file-hook)
    (setf (alist-get file zeft--database nil nil #'equal)
          (buffer-substring-no-properties (point-min) (point-max)))))

(defvar zeft-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zeft-create-note)
    (define-key map (kbd "C-c C-g") #'zeft-force-refresh)
    (define-key map (kbd "C-n") #'zeft-next)
    (define-key map (kbd "C-p") #'zeft-previous)
    map)
  "Mode map for `zeft-mode'.")

(define-derived-mode zeft-mode fundamental-mode
  "Zeft" "Search for notes and display summaries."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (visual-line-mode)
    (setq default-directory zeft-directory
          zeft--last-window-config (current-window-configuration))
    (add-hook 'after-change-functions
              (lambda (&rest _) (zeft-refresh)) 0 t)
    (add-hook 'window-size-change-functions #'zeft-refresh 0 t)
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when zeft--last-window-config
                  (set-window-configuration zeft--last-window-config)))
              0 t)
    (insert "\n\nInsert search phrase and press RET to search.")
    (goto-char (point-min))))

;;; Userland

(defun zeft ()
  "Start Zeft."
  (interactive)
  (setq zeft--last-window-config (current-window-configuration))
  (switch-to-buffer (zeft--buffer))
  (unless zeft--database
    (dolist (file (zeft--file-list))
      (zeft--save-to-database file (zeft--work-buffer))))
  (unless (derived-mode-p 'zeft-mode)
    (zeft-mode))
  ;; This hooks is intentionally global.
  (add-hook 'after-save-hook #'zeft--after-save-hook))

(defun zeft-create-note ()
  "Create a new note with the current search phrase as the title."
  (interactive)
  (let* ((search-phrase (zeft--get-search-phrase))
         (file-path (expand-file-name (concat search-phrase ".txt")
                                      zeft-directory))
         (exists-p (file-exists-p file-path)))
    ;; If there is no match, create the file without confirmation,
    ;; otherwise prompt for confirmation. NOTE: this is not DRY, but
    ;; should be ok.
    (when (or (search-forward "Press RET to create a new note" nil t)
              (y-or-n-p (format "Create file `%s'.txt? " search-phrase)))
      (find-file file-path)
      (unless exists-p
        (insert search-phrase "\n\n")
        (save-buffer)))))

(defvar-local zeft--select-overlay nil
  "Overlay used for highlighting selected search result.")

(defun zeft--highlight-file-at-point ()
  "Activate (highlight) the file excerpt button at point."
  (when-let ((button (button-at (point))))
    ;; Create the overlay if it doesn't exist yet.
    (when (null zeft--select-overlay)
      (setq zeft--select-overlay (make-overlay (button-start button)
                                               (button-end button)))
      (overlay-put zeft--select-overlay 'evaporate t)
      (overlay-put zeft--select-overlay 'face 'zeft-selection))
    ;; Move the overlay over the file.
    (move-overlay zeft--select-overlay
                  (button-start button) (button-end button))))

(defun zeft-next ()
  "Move to next file excerpt."
  (interactive)
  (when (forward-button 1 nil nil t)
    (zeft--highlight-file-at-point)))

(defun zeft-previous ()
  "Move to previous file excerpt."
  (interactive)
  (if (backward-button 1 nil nil t)
      (zeft--highlight-file-at-point)
    ;; Go to the end of the search phrase.
    (goto-char (point-min))
    (end-of-line)))

;;; Draw

(defvar zeft--preview-window nil
  "Zeft shows file previews in this window.")

(defvar-local zeft--last-search-phrase ""
  "The search phrase used when we last refreshed Zeft buffer.")

(defvar-local zeft--last-file-list nil
  "The last matched files list. Nil indicates that the last list
is empty.")

(defun zeft--get-search-phrase ()
  "Return the search phrase. Assumes current buffer is a zeft buffer."
  (save-excursion
    (goto-char (point-min))
    (string-trim
     (buffer-substring-no-properties (point) (line-end-position)))))

(defun zeft--find-file-at-point ()
  "View file at point."
  (interactive)
  (find-file (button-get (button-at (point)) 'path)))

(defun zeft--preview-file (file &optional select)
  "View FILE in another window.
If SELECT is non-nil, select the buffer after displaying it."
  (interactive)
  (let* ((buffer (find-file-noselect file))
         (search-phrase (zeft--get-search-phrase))
         (keyword-list (split-string search-phrase)))
    (if (and (window-live-p zeft--preview-window)
             (not (eq zeft--preview-window (selected-window))))
        (with-selected-window zeft--preview-window
          (switch-to-buffer buffer))
      (setq zeft--preview-window
            (display-buffer
             buffer '((display-buffer-use-some-window
                       display-buffer-in-direction
                       display-buffer-pop-up-window)
                      . ((inhibit-same-window . t)
                         (direction . right)
                         (window-width
                          . (lambda (win)
                              (let ((width (window-width)))
                                (when (< width 50)
                                  (window-resize
                                   win (- 50 width) t))))))))))
    (if select (select-window zeft--preview-window))
    (with-current-buffer buffer
      (zeft--highlight-matched keyword-list)
      (run-hooks 'zeft-find-file-hook))))

(define-button-type 'zeft-excerpt
  'action (lambda (button)
            ;; If the file is no already highlighted, highlight it
            ;; first.
            (when (not (and zeft--select-overlay
                            (overlay-buffer zeft--select-overlay)
                            (<= (overlay-start zeft--select-overlay)
                                (button-start button)
                                (overlay-end zeft--select-overlay))))
              (goto-char (button-start button))
              (zeft--highlight-file-at-point))
            (zeft--preview-file (button-get button 'path)))
  'keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map button-map)
            (define-key map (kbd "RET") #'zeft--find-file-at-point)
            (define-key map (kbd "SPC") #'push-button)
            map)
  'help-echo "Open this file"
  'follow-link t
  'face 'default
  'mouse-face 'zeft-selection)

(defun zeft--highlight-search-phrase ()
  "Highlight search phrases in buffer."
  (let ((keyword-list (split-string (zeft--get-search-phrase)))
        (inhibit-read-only t))
    (dolist (keyword keyword-list)
      (goto-char (point-min))
      (forward-line 2)
      ;; We use overlay because overlay allows face composition.
      ;; So we can have bold + underline.
      (while (search-forward keyword nil t)
        (let ((ov (make-overlay (match-beginning 0)
                                (match-end 0))))
          (overlay-put ov 'face 'zeft-inline-highlight)
          (overlay-put ov 'zeft-highlight t)
          (overlay-put ov 'evaporate t))))))

(defun zeft--insert-file-excerpt (file)
  "Insert an excerpt for FILE at point.
This excerpt contains note title and content except and is
clickable. FILE should be an absolute path."
  (let ((excerpt-len (floor (* 2.7 (1- (window-width)))))
        (last-search-term
         (car (last (split-string (zeft--get-search-phrase)))))
        title excerpt)
    (with-current-buffer (zeft--work-buffer)
      (widen)
      (erase-buffer)
      (insert (alist-get file zeft--database nil nil #'equal))
      (goto-char (point-min))
      (search-forward "#+TITLE: " (line-end-position) t)
      (setq title (buffer-substring-no-properties
                   (point) (line-end-position)))
      (when (eq title "") (setq title "no title"))
      (forward-line)
      (narrow-to-region (point) (point-max))
      ;; Grab excerpt.
      (setq excerpt (string-trim
                     (replace-regexp-in-string
                      "[[:space:]]+"
                      " "
                      (if (and last-search-term
                               (search-forward last-search-term nil t))
                          (buffer-substring-no-properties
                           (max (- (point) (/ excerpt-len 2))
                                (point-min))
                           (min (+ (point) (/ excerpt-len 2))
                                (point-max)))
                        (buffer-substring-no-properties
                         (point)
                         (min (+ (point) excerpt-len)
                              (point-max))))))))
    ;; Now we insert the excerpt
    (let ((start (point)))
      (insert (propertize title 'face '(:weight bold))
              "\n"
              (propertize excerpt 'face '(:weight light))
              "\n\n")
      ;; If we use overlay (with `make-button'), the button's face
      ;; will override the bold and light face we specified above.
      (make-text-button start (- (point) 2)
                        :type 'zeft-excerpt
                        'path file))))

;;; Refresh and search

(defun zeft-force-refresh ()
  "Force refresh Zeft buffer."
  (interactive)
  (zeft-refresh t))

(defun zeft--file-list ()
  "Return a list of all files in ‘zeft-directory’."
  (cl-remove-if-not
   (lambda (file)
     (and (file-regular-p file)
          (not (string-prefix-p
                "." (file-name-base file)))))
   (directory-files zeft-directory t)))

(defun zeft-refresh (&optional force)
  "Search for notes and display their summaries.
If FORCE is non-nil, refresh even if the search phrase didn't change."
  (interactive)
  (catch 'early-term
    (let ((search-phrase (zeft--get-search-phrase)))
      (when (and (derived-mode-p 'zeft-mode)
                 (or force (not (equal search-phrase
                                       zeft--last-search-phrase))))
        (let* ((phrase-empty (equal search-phrase ""))
               (file-list
                (while-no-input
                  (zeft--search
                   search-phrase
                   ;; If the current search phrase contains the previous
                   ;; search phrase, we can just use the last file-list.
                   (if (and (not phrase-empty)
                            (string-prefix-p zeft--last-search-phrase
                                             search-phrase))
                       zeft--last-file-list
                     nil)))))
          ;; Early termination by ‘while-no-input’.
          (if (eq file-list t) (throw 'early-term nil))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t)
                (orig-point (point))
                new-content)
            ;; Sort files.
            (setq file-list (cl-sort file-list #'file-newer-than-file-p))
            ;; Don’t display all files when the search phrase is empty.
            (when (and (not force) phrase-empty)
              (setq file-list (cl-subseq file-list 0 20)))
            ;; Insert file summaries. We get the new content then
            ;; insert it whole so that we can use ‘while-no-input’.
            (setq new-content
                  (if file-list
                      (while-no-input
                        (with-temp-buffer
                          (dolist (file file-list)
                            (zeft--insert-file-excerpt file))
                          (buffer-string)))
                    ;; NOTE: this string is referred in
                    ;; ‘zeft-create-note’.
                    "Press RET to create a new note"))
            ;; Early termination by ‘while-no-input’.
            (if (eq new-content t) (throw 'early-term nil))
            ;; Actually insert the new content.
            (goto-char (point-min))
            (forward-line 2)
            (delete-region (point) (point-max))
            (let ((start (point)))
              (insert new-content)
              ;; If we use (- start 2), emacs-rime cannot work.
              (put-text-property (- start 1) (point)
                                 'read-only t))
            (zeft--highlight-search-phrase)
            (setq zeft--last-search-phrase search-phrase)
            (setq zeft--last-file-list
                  (if phrase-empty nil file-list))
            (set-buffer-modified-p nil)
            ;; Save excursion wouldn’t work since we erased the buffer
            ;; and re-inserted contents.
            (goto-char orig-point)
            ;; Re-apply highlight.
            (zeft--highlight-file-at-point)))))))

(defun zeft--search (phrase &optional shortcut-file-list)
  "Return a list of files that contains PHRASE.
If SHORTCUT-FILE-LIST is non-nil, search in that list. PHRASE is
a space separated string, each keyword in that phrase is searched
in each files, if a file contains all keywords (in any order) the
file is considered a hit."
  (let* ((keyword-list (split-string phrase))
         (file-list (or shortcut-file-list (zeft--file-list))))
    (with-current-buffer (zeft--work-buffer)
      (cl-loop for file in file-list
               if (condition-case nil
                      (when (file-exists-p file)
                        (erase-buffer)
                        (if-let ((content (alist-get file zeft--database
                                                     nil nil #'equal)))
                            (insert content)
                          ;; This function will insert the file
                          ;; content to the current buffer.
                          (zeft--save-to-database file))
                        (dolist (keyword keyword-list t)
                          (goto-char (point-min))
                          (search-forward keyword)))
                    (search-failed nil))
               collect file))))

;;; Highlight matched phrases

(defun zeft--highlight-matched (keyword-list)
  "Highlight keywords in KEYWORD-LIST in the current buffer."
  (save-excursion
    ;; Add highlight overlays.
    (dolist (keyword keyword-list)
      (goto-char (point-min))
      (while (search-forward keyword nil t)
        (let ((ov (make-overlay (match-beginning 0)
                                (match-end 0))))
          (overlay-put ov 'face 'zeft-preview-highlight)
          (overlay-put ov 'zeft-highlight t))))
    ;; Add cleanup hook.
    (add-hook 'window-selection-change-functions
              #'zeft--cleanup-highlight
              0 t)))

(defun zeft--cleanup-highlight (window)
  "Cleanup highlights in WINDOW."
  (when (eq window (selected-window))
    (let ((ov-list (overlays-in (point-min)
                                (point-max))))
      (dolist (ov ov-list)
        (when (overlay-get ov 'zeft-highlight)
          (delete-overlay ov))))
    (remove-hook 'window-selection-change-functions
                 #'zeft--cleanup-highlight
                 t)))

(provide 'zeft)

;;; zeft.el ends here

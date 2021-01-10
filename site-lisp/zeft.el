;;; zeft.el --- Yet another Deft      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Why I need to write another Deft:
;;
;; Deft made two design decisions. While ingenious, these designs
;; bring problems that I can’t easily workaround. First, Deft decides
;; to capture user inputs and construct the search phrase itself. This
;; allows the point to be anywhere in the buffer while updating the
;; search phrase. That prohibits any Emacs input method from working.
;; And I can’t select the search phrase like normal buffer text
;; Second, Deft decides to load all files into memory and simply
;; search in them when filtering. As inefficient and wasteful as it
;; sounds, this works well. However, it does make Deft a bit slow on
;; startup. I have to wait for a second or two every time I start
;; Deft. And a mortal like me can’t help but worrying about the memory
;; used for storing all these files.
;; 
;; There are other motivations, like the desire for a interface
;; similar to Apple’s Note.app, where each file comes with a
;; multi-line excerpt and there is another window previewing the
;; currently selected file.
;;
;; Usage:
;;
;; Type M-x zeft RET, and you should see the Zeft buffer. Type in your
;; search phrase in the first line and the result will show up as you
;; type. Press C-n and C-p to go through each file. You can preview a
;; file by pressing SPC when the point is on a file, or click the file
;; with mouse. Press RET to open the file in the same window.
;;
;; Type C-c C-g to force a refresh. When point is on the search
;; phrase, press RET to create a file with the search phrase as
;; filename and title.
;;
;; Note that:
;;
;; 1. Zeft uses grep, make sure it is installed in your system and can
;;    be found by Emacs. If you have rg on your system, Zeft
;;    automatically uses rg.
;;
;; 2. Zeft starts filtering when the search phrase is at least 3
;;    characters long. To search with phrase shorter than that, use
;;    C-c C-g to force a refresh.
;;
;; 3. Zeft creates new files by using the search phrase as the
;;    filename and title, if you want otherwise, email me or redefine
;;    ‘zeft-create-note’.
;;
;; 4. Zeft saves the current window configuration before switching to
;;    Zeft buffer. When Zeft buffer is killed, Zeft restores the saved
;;    window configuration.

;;; Code:

(require 'cl-lib)

;;; Customize

(defcustom zeft-directory (expand-file-name "~/.deft")
  "Directory in where notes are stored."
  :group 'zeft
  :type 'string)

(defcustom zeft-find-file-hook nil
  "Hook run when Zeft opens a file."
  :group 'zeft
  :type 'list)

(defface zeft-selection
  '((t . (:inherit region :extend t)))
  "Face for the current selected search result."
  :group 'zeft)

(defface zeft-highlight
  '((t . (:inherit underline :extend t)))
  "Face for highlighting in Zeft."
  :group 'zeft)

;;; Helpers

(defvar zeft--last-window-config nil
  "Window configuration before Zeft starts.")

(defun zeft--buffer ()
  "Return zeft buffer."
  (get-buffer-create "*zeft*"))

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
    (add-hook 'post-command-hook #'zeft-refresh 0 t)
    (add-hook 'window-size-change-functions #'zeft-refresh 0 t)
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when zeft--last-window-config
                  (set-window-configuration zeft--last-window-config)))
              0 t)
    (insert "\n\nInsert search phrase and press RET to search.")
    (goto-char (point-min))))

;; Start search.
(defvar zeft--start-time nil)
;; Grep returns.
(defvar zeft--grep-done-time nil)
;; Search in Emacs finishes.
(defvar zeft--search-done-time nil)
;; Inserted all file excerpts.
(defvar zeft--insert-done-time nil)
(defvar zeft--debug-mode nil)

;;; Userland

(defun zeft ()
  "Start Zeft."
  (interactive)
  (setq zeft--last-window-config (current-window-configuration))
  (switch-to-buffer (zeft--buffer))
  (unless (derived-mode-p 'zeft-mode)
    (zeft-mode)))

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
      (zeft--view-file file-path t)
      (unless exists-p
        (insert search-phrase "\n\n")
        (save-buffer)))))

(defvar-local zeft--select-overlay nil
  "Overlay used for highlighting selected search result.")

(defun zeft--highlight-file-at-point ()
  "Activate (highlight) the file excerpt button at point."
  (let ((button (button-at (point))))
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
  "Move to previous file excertp."
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
  "The last matched files list.")

(defun zeft--get-search-phrase ()
  "Return the search phrase. Assumes current buffer is a zeft buffer."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (point) (line-end-position))))

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
          (overlay-put ov 'face 'zeft-highlight)
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
    (with-current-buffer (get-buffer-create " *zeft work*")
      (widen)
      (erase-buffer)
      (insert-file-contents file)
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

(defun zeft-force-refresh ()
  "Force refresh Zeft buffer."
  (interactive)
  (zeft-refresh t))

(defun zeft-refresh (&optional force)
  "Search for notes and display their summaries.
If FORCE is non-nil, refresh even if the search phrase didn't change."
  (interactive)
  (let ((search-phrase (zeft--get-search-phrase))
        (buffer (current-buffer)))
    ;; Only refresh when search phrase changes is long enough.
    (when (and (derived-mode-p 'zeft-mode)
               (or force
                   (and (not (equal search-phrase
                                    zeft--last-search-phrase))
                        (not (< 0 (string-width search-phrase) 3)))))
      (zeft--search
       search-phrase
       (lambda (file-list)
         (setq zeft--search-done-time (current-time))
         (setq file-list
               (cl-sort file-list
                        #'file-newer-than-file-p))
         ;; Insert file summaries.
         (let ((inhibit-read-only t))
           (with-current-buffer buffer
             (save-excursion
               (goto-char (point-min))
               (forward-line 2)
               (delete-region (point) (point-max))
               (let ((start (point)))
                 (if file-list
                     (dolist (file file-list)
                       (zeft--insert-file-excerpt file))
                   (insert "Press RET to create a new note"))
                 (put-text-property (- start 2) (point)
                                    'read-only t))
               (zeft--highlight-search-phrase)
               (setq zeft--last-search-phrase search-phrase
                     zeft--last-file-list file-list)
               (set-buffer-modified-p nil))))
         (setq zeft--insert-done-time (current-time))
         (when zeft--debug-mode
           (with-temp-buffer
             (insert (format "Grep: %f  Search: %f  Insert: %f  Seaching: %s\n"
                             (time-to-seconds
                              (time-subtract zeft--grep-done-time
                                             zeft--start-time))
                             (time-to-seconds
                              (time-subtract zeft--search-done-time
                                             zeft--grep-done-time))
                             (time-to-seconds
                              (time-subtract zeft--insert-done-time
                                             zeft--search-done-time))
                             search-phrase))
             (append-to-file (point-min) (point-max) "~/zeft-time"))))
       ;; If the current search phrase includes the previous search
       ;; phrase, we can just filter the last file-list.
       (if (and zeft--last-search-phrase
                (string-prefix-p
                 zeft--last-search-phrase search-phrase))
           zeft--last-file-list)))))

;;; Search

(defun zeft--search (phrase callback &optional shortcut-file-list)
  "Search for PHRASE in `zeft-directory'.
And call CALLBACK with matched files.
If SHORTCUT-FILE-LIST non-nil, filter upon that list."
  ;; We do two rounds of search/filtering:
  ;;
  ;; Round 1: grep the first keyword
  ;; Round 2: on the basis of the result of round 1, search the whole
  ;; search phrase in Emacs.
  ;;
  ;; Hopefully round 1 can filter out most of the files so round 2
  ;; isn't too slow. If SHORTCUT-FILE-LIST is non-nil and short
  ;; enough, we skip round 1 and goes straight to round 2 with that
  ;; list.
  (setq zeft--start-time (current-time))
  (let* ((keyword-list (split-string phrase))
         (file-list (cl-remove-if-not
                     (lambda (file)
                       (and (file-regular-p file)
                            (not (string-prefix-p
                                  "." (file-name-base file)))))
                     (directory-files zeft-directory t)))
         ;; Given a list of files, FILTER searches for rest of the
         ;; keywords. This is the filter function for round 2.
         (filter (lambda (file-list keyword-list)
                   (funcall
                    callback
                    (with-temp-buffer
                      (cl-loop
                       for file in file-list
                       if (condition-case nil
                              (when (file-exists-p file)
                                (erase-buffer)
                                (insert-file-contents file)
                                (dolist (keyword keyword-list t)
                                  (goto-char (point-min))
                                  (re-search-forward keyword)))
                            (search-failed nil))
                       collect file))))))
    (cond
     ;; Search phrase is empty, simply return the full list.
     ((null keyword-list)
      (setq zeft--grep-done-time (current-time))
      (setq zeft--search-done-time (current-time))
      (funcall callback file-list))
     ;; Have a shortcut and it's short enough, skip round 1 and go
     ;; straight to round 2.
     ((and shortcut-file-list (< (length shortcut-file-list) 10))
      (setq zeft--grep-done-time (current-time))
      (funcall filter shortcut-file-list keyword-list))
     ;; Normal search, do round 1 then round 2.
     (t
      (let* ((name (generate-new-buffer-name " *zeft grep*"))
             (grep (if (executable-find "rg") "rg" "grep"))
             (process (apply
                       #'start-process
                       ;; Ignore case, List filenames, Fixed literals
                       ;; (no regexp).
                       name name grep "-ilFs"
                       ;; The phrase could span multiple lines
                       ;; (because of filling), so we only search for
                       ;; the part before first space as a preliminary
                       ;; filter. We later do an accurate search.
                       (car keyword-list)
                       ;; Search all regular, visible files.
                       file-list))
             ;; When the grep process finishes, we parse the result
             ;; files and call FILTER with them.
             (sentinal
              (lambda (process event)
                (if (string-match-p (rx (or "finished" "exited"))
                                    event)
                    (if-let ((buf (process-buffer process)))
                        (unwind-protect
                            (with-current-buffer buf
                              (let ((files (split-string
                                            (buffer-string) "\n")))
                                (setq zeft--grep-done-time (current-time))
                                (funcall filter
                                         (remove "" files)
                                         (cdr keyword-list))))
                          (kill-buffer buf))
                      (error "Zeft’s grep process’ buffer is killed"))
                  (error "Zeft’s grep process failed with signal: %s"
                         event)))))
        (set-process-sentinel process sentinal))))))

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
          (overlay-put ov 'face 'zeft-highlight)
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

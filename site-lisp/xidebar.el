;;; xidebar.el --- Imenu + File Browser Sidebar  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; Commentary:
;;
;; This package provides a xidebar that contain either a code outline
;; or a project file browser. Pressing "x" will switch between the two
;; modes. The xidebar updates its content based on the currently
;; selected window. The xidebar is local to the frame and tab-bar tab,
;; so you can have multiple independent ones per frame/tab.
;;
;; To use xidebar, first turn on the global minor mode
;; ‘xidebar-monitor-mode’, then use ‘xidebar-toggle’ to toggle the
;; frame/tab-specific xidebar.
;;
;; The intended use-case is for the xidebar to be a visual aid, and to
;; use mouse to click on outlines and files. A more keyboard-centric
;; workflow is possible, but isn’t the primary concern.
;;
;; Xidebar can use either ASCII or image icons. Set
;; ‘xidebar-file-icon-function’ to
;; ‘xidebar-file-icon-vscode(-file-only)’ to use vscode-icon.el for
;; image icons.
;;
;; Customizable faces:
;; - xidebar-body
;; - xidebar-jmenu-section-header
;; - xidebar-file-icon

;;; Developer:
;;
;; Since the xidebar is tab and frame-specific, we don’t set a
;; xidebar--buffer variable, instead, we use ‘xidebar--get-window’,
;; which goes through all visible window and look for the
;; ‘xidebar-window’ window parameter.
;;
;; Since the primary use-case is to use mouse to click on files, the
;; file is opened in the currently selected window. To support
;; keyboard workflows, we probably need to add a popup menu to select
;; which window to open the file in.
;;
;; To avoid generating xidebar content from scratch every time we
;; context switch (because the selected window changed), I added a
;; cache. The cache also saves the scrolling position.
;; Highlight/visibility implemented by text properties are also
;; conveniently preserved in the cache, since we just store the buffer
;; string to the cache.
;;
;; Refresh, render, and update: The mode backend provides two
;; functions, render and update. Render is from generating content
;; from scratch; update is for updating content (new files, new jmenu
;; entries). ‘xidebar-refresh’ is a global function that runs a) when
;; selected window has changed (context switch); b) when an idle timer
;; is set off (background update); or c) when the user manually
;; requests for it.


;;; Code:

(require 'cl-seq)
(require 'project)
(require 'outline)

;;; Customization

(defgroup xidebar nil
  "Frame-specific xidebar."
  :group 'xidebar)

(defface xidebar-body '((t . (:inherit variable-pitch)))
  "Face for the main body of the xidebar.")

(defface xidebar-jmenu-section-header
  '((t . (:inherit (xidebar-body header-line))))
  "Face for the section header in Imenu mode.
Stuff like Variables, Functions, etc.")

(defface xidebar-file-icon
  '((t . (:inherit (shadow fixed-pitch))))
  "Face for the ASCII file icons.")

(defvar xidebar-refresh-predicate #'xidebar-should-refresh-p
  "A function deciding whether xidebar should refresh.")

(defvar xidebar-file-icon-function #'xidebar-file-icon-ascii
  "A function that applies a suitable icon for a file.
The function should take four arguments (BEG WIDTH PATH OPEN-P),
BEG is the start of the text on which the function should apply
text property to. WIDTH is the pixel width of the icon. PATH is
the absolute path of the file/directory, OPEN-P is non-nil if
it’s for an expanded directory.

The function should apply property to the text from BEG to (+ 2
BEG), and the visual width of the text should be WIDTH pixels
wide.")

;;; Variables

(defvar-local xidebar--source-window nil
  "The window this xidebar is associated with.
Eg, if xidebar opens a file, it’s in this window.")

(defvar xidebar--verbose nil
  "Whether to print debugging information.")

;;; Jmenu
;;
;; Jmenu is like Imenu, but is flat, and orders entries by their
;; position, and uses outline headers as section headers.
;;
;; (ref:jmenu-entry) A Jmenu entry is of the form (MARKER LABEL TYPE).
;; MARKER and LABEL are the same as in an Imenu, TYPE is either
;; ‘entry’ or ‘header’.

(define-button-type 'xidebar-jmenu
  'follow-link t
  'action #'xidebar--jmenu-button-action
  'face 'default
  'mouse-face 'region
  'help-echo "Jump to location")

(define-button-type 'xidebar-jmenu-header
  'follow-link t
  'action #'xidebar--jmenu-button-action
  'face 'xidebar-jmenu-section-header
  'mouse-face 'highlight
  'help-echo "Jump to location")

(defun xidebar--jmenu-button-action (button)
  "Jump to the location represented by BUTTON."
  (let ((marker (button-get button 'marker)))
    (select-window xidebar--source-window)
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)))

(defun xidebar--flatten-imenu-alist (alist return-list)
  "Flatten Imenu ALIST.
Append to RETURN-LIST and return it. Return a list of Jmenu
entries, see (ref:jmenu-entry)."
  (dolist (entry alist)
    (if (consp (cdr entry))
        (setq return-list (xidebar--flatten-imenu-alist
                           (cdr entry) return-list))
      (push (list (cdr entry) ; Marker.
                  (car entry) ; Label.
                  'entry)
            return-list)))
  return-list)

(defun xidebar--get-jmenu (buffer)
  "Get the jmenu of BUFFER.
Return a list of (MARKER LABEL TYPE)."
  (with-current-buffer buffer
    (let (entry-list)
      (save-excursion
        (setq entry-list
              (xidebar--flatten-imenu-alist
               (funcall imenu-create-index-function)
               nil))
        ;; Add section headers.
        (when outline-minor-mode
          (goto-char (point-min))
          (while (if outline-search-function
                     (funcall outline-search-function nil t)
                   (re-search-forward
                    (concat "^\\(?:" outline-regexp "\\)")
			        nil t))
            (let ((marker (make-marker)))
              (move-marker marker (match-beginning 0))
              (push (list marker
                          (string-trim-left
                           (match-string-no-properties 0)
                           comment-start-skip)
                          'header)
                    entry-list))))
        (cl-sort entry-list #'< :key #'car)))))

(defun xidebar--render-jmenu (buffer)
  "Insert BUFFER’s jmenu into current buffer."
  (let ((entry-list (xidebar--get-jmenu buffer))
        (last-header-pos -1))
    (pcase-dolist (`(,marker ,label ,type) entry-list)
      (setq label (string-replace "\n" " " (string-trim label)))
      (setq label (concat label
                          (propertize " " 'display
                                      '(space :align-to right))))
      (pcase type
        ('entry
         (unless (= marker last-header-pos)
           (insert-text-button label
                               'type 'xidebar-jmenu
                               'marker marker)
           (insert "\n")))
        ('header
         (insert-text-button label
                             'type 'xidebar-jmenu-header
                             'marker marker)
         (setq last-header-pos marker)
         (insert "\n"))))))

(defun xidebar--update-jmenu (buffer)
  "Update the xidebar content for BUFFER by simply re-rendering it."
  (erase-buffer)
  (xidebar--render-jmenu buffer))

;;; Project file buttons

(define-button-type 'xidebar-file-button
  'follow-link t
  'action #'xidebar--project-files-button-action
  'face 'default
  'mouse-face 'region
  'help-echo "Expand directory or open file")

(defun xidebar--directory-regular-files (path)
  "Return a list of regular files under directory at PATH.
The list is sorted so that directories comes first."
  (let ((file-re directory-files-no-dot-files-regexp))
    (cl-sort (directory-files path t file-re)
             (lambda (a _)
               (file-directory-p a)))))

(defun xidebar--insert-directory-files (button)
  "Insert directory files after BUTTON.
BUTTON should be a ‘xidebar-file-button’ and should represent a
directory."
  (let ((path (button-get button 'xidebar-file-path))
        (indent-level
         (button-get button 'xidebar-indent-level))
        beg end)
    (save-excursion
      (goto-char (1+ (button-end button)))
      (setq beg (point-marker))
      ;; Sort directories first.
      (dolist (file (xidebar--directory-regular-files path))
        (xidebar--insert-file-button file (1+ indent-level)))
      (setq end (point-marker))
      (set-marker-insertion-type end t)
      (button-put button 'xidebar-expanded-region
                  (cons beg end)))))

(defun xidebar--project-files-button-action (button)
  "Expand directory or open file.
BUTTON should be a ‘siebar-file-button’."
  (let ((path (button-get button 'xidebar-file-path))
        (inhibit-read-only t))
    (if (file-regular-p path)
        ;; Open file.
        (progn
          (select-window xidebar--source-window)
          (unless (equal buffer-file-name path)
            (find-file path)))
      ;; Expand/fold directory.
      (if-let ((region (button-get button 'xidebar-expanded-region)))
          ;; If directory content is already in buffer. Toggle
          ;; visibility for expand/fold.
          (progn (delete-region (car region) (cdr region))
                 (button-put button 'xidebar-expanded-region nil))
        ;; If directory content isn’t in the buffer, insert them.
        (xidebar--insert-directory-files button))
      (let* ((indent-level (button-get button 'xidebar-indent-level))
             (beg (+ (button-start button) (* 2 indent-level)))
             (expanded (button-get button 'xidebar-expanded-region)))
        (funcall xidebar-file-icon-function
                 beg (* 2 (frame-char-width)) path expanded)))))

(defun xidebar--insert-file-button (path indent-level)
  "Insert the button for the file at PATH.
PATH should be an absolute path. INDENT-LEVEL is the number of
spaces to insert before label."
  (let ((beg (point)))
    (insert-text-button
     (concat (propertize (make-string (* 2 indent-level) ?\s)
                         'face 'fixed-pitch)
             "xx"
             (file-name-nondirectory (string-trim-right path "/"))
             (propertize " " 'display '(space :align-to right)))
     'type 'xidebar-file-button
     'xidebar-file-path path
     ;; If the content of this directory is inserted into the buffer,
     ;; this cons (BEG . END) marks that content.
     'xidebar-expanded-region nil
     'xidebar-indent-level indent-level)
    (funcall xidebar-file-icon-function
             (+ beg (* 2 indent-level))
             (* 2 (frame-char-width))
             path
             nil))
  (insert "\n"))

;;; Project files

(defun xidebar--buffer-project-root (buffer)
  "Return the absolute path of the project root of BUFFER.
The project root is used as a cache key."
  (with-current-buffer buffer
    (expand-file-name
     (if-let ((proj (project-current)))
         (project-root proj)
       default-directory))))

(defun xidebar--render-project-files (buffer)
  "Render project files content for BUFFER."
  (let ((proj-root (xidebar--buffer-project-root buffer)))
    (dolist (file (xidebar--directory-regular-files proj-root))
      (xidebar--insert-file-button file 0))))

(defun xidebar--update-project-files (buffer)
  "Update the xidebar content for BUFFER in the current buffer."
  (let ((orig-point (point))
        (expanded-files nil)
        (should-be-zero 0))
    ;; Find all the expanded directories.
    (goto-char (point-min))
    (while (eq should-be-zero 0)
      (when-let ((button (button-at (point))))
        (when (button-get button 'xidebar-expanded-region)
          (push (button-get button 'xidebar-file-path)
                expanded-files)))
      (setq should-be-zero (forward-line 1)))
    (setq expanded-files
          (cl-sort expanded-files #'< :key #'length))
    (erase-buffer)
    ;; Insert first-level directories/files.
    (xidebar--render-project-files buffer)
    ;; Expand those that was expanded.
    (goto-char (point-min))
    (setq should-be-zero 0)
    (while (eq should-be-zero 0)
      (when-let ((button (button-at (point))))
        (when (member (button-get button 'xidebar-file-path)
                      expanded-files)
          (xidebar--project-files-button-action button)))
      (setq should-be-zero (forward-line 1)))
    (goto-char orig-point)))

;;; Project files icon

(defun xidebar-file-icon-ascii (beg _width path open-p)
  "Apply the ASCII icon for PATH from BEG to END.
WIDTH and OPEN-P are described in ‘xidebar-file-icon-function’."
  (add-text-properties
   beg (+ beg 2)
   `( display ,(if (file-regular-p path) "f "
                 (if open-p "↓ " "→ "))
      face 'xidebar-file-icon)))

(declare-function vscode-icon-for-file "vscode-icon")
(defun xidebar-file-icon-vscode (beg width path open-p)
  "Apply the VSCode icon for PATH at BEG.
WIDTH is the pixel width of the icon. OPEN-P is described in
‘xidebar-file-icon-function’."
  (if (not (fboundp 'vscode-icon-for-file))
      (xidebar-file-icon-ascii beg width path open-p)
    (put-text-property
     beg (+ beg 1)
     'display (let ((img (vscode-icon-for-file path t open-p)))
                (setf (image-property img :width) (- width 2))
                (setf (image-property img :scale) nil)
                img))
    (put-text-property (+ beg 1) (+ beg 2)
                       'display '(space :width (2)))))

(defun xidebar-file-icon-vscode-file-only (beg width path open-p)
  "Apply the VSCode icon for PATH at BEG.
WIDTH is the pixel width of the icon. OPEN-P is described in
‘xidebar-file-icon-function’."
  (if (or (not (fboundp 'vscode-icon-for-file))
          (file-directory-p path))
      (xidebar-file-icon-ascii beg width path open-p)
    (xidebar-file-icon-vscode beg width path open-p)))

;;; Modes

(defvar-local xidebar--current-mode 'project-files
  "The mode this xidebar is in.
It can be ‘jmenu’ or ‘project-files’.")

(defvar-local xidebar--current-cache-key nil
  "Cache key corresponding to the current content.")

(defvar xidebar-mode-alist '(( jmenu identity
                               xidebar--render-jmenu
                               xidebar--update-jmenu)
                             ( project-files
                               xidebar--buffer-project-root
                               xidebar--render-project-files
                               xidebar--update-project-files))
  "An alist of xidebar modes.

Each key should be a symbol representing a mode, like ‘jmenu’.

Each value should be a list of three functions, a key function, a
render function, and an update function. All three takes the
source buffer as the argument. The key function should return a
cache key (compared with ‘equal’); the render function should
insert the xidebar content of corresponding to the source buffer;
the update function should update the buffer content, for simple
modes, it can be the same as the render function (ie, update by
simply re-rendering the content). More sophisticated modes might
want to update the content but keep some state consistent. Eg,
project-files mode keeps expanded directories expanded across
updates.

When the render function runs, it’s given an empty buffer; when
the update function runs, the old buffer content is still intact.

The render function should draw persistent state (eg, folding)
with text properties rather than overlays, because text
properties are preserved in cache.")

(defun xidebar--render (mode source-window &optional xidebar-window)
  "Switch to MODE in the current xidebar buffer.
SOURCE-WINDOW is usually the selected window and the xidebar
content corresponds to it’s window buffer. If not omitted,
XIDEBAR-WINDOW is the window containing the xidebar. Return
non-nil if the content did change."
  (let* ((inhibit-read-only t)
         (source-buffer (window-buffer source-window))
         (key-fn (nth 0 (alist-get mode xidebar-mode-alist)))
         (render-fn (nth 1 (alist-get mode xidebar-mode-alist)))
         (key (funcall key-fn source-buffer)))
    (when xidebar--verbose
      (message "Source buffer: %s, mode: %s, old key: %s, new key: %s"
               source-buffer mode xidebar--current-cache-key key))
    (if (and (eq xidebar--current-mode mode)
             (equal xidebar--current-cache-key key))
        nil
      ;; Save current xidebar content and scrolling to cache.
      (xidebar--cache-set
       xidebar--current-mode xidebar--current-cache-key
       (list (window-point xidebar-window)
             (window-start xidebar-window)
             (buffer-string))
       20)
      ;; Insert new content.
      (erase-buffer)
      (if-let ((cache-entry (xidebar--cache-get mode key)))
          (pcase-let ((`(,point ,start ,content)
                       cache-entry))
            (when xidebar--verbose
              (message "Cache hit"))
            (insert content)
            (when xidebar-window
              (set-window-point xidebar-window point)
              (set-window-start xidebar-window start)))
        (when xidebar--verbose
          (message "Cache miss"))
        (funcall render-fn source-buffer)
        (goto-char (point-min)))
      (setq xidebar--current-mode mode
            xidebar--current-cache-key key)
      ;; Indicate that buffer content changed.
      t)))

(defun xidebar--update (mode source-window)
  "Update xidebar buffer (current buffer) for MODE.
SOURCE-WINDOW is the source window."
  (let ((inhibit-read-only t)
        (source-buffer (window-buffer source-window))
        (update-fn (nth 2 (alist-get mode xidebar-mode-alist))))
    (funcall update-fn source-buffer)))

;;; Xidebar

(defun xidebar--create (source-window)
  "Create a xidebar buffer, set it up, and return it.
SOURCE-WINDOW is set to the xidebar buffer’s source window."
  (let ((buf (generate-new-buffer "*xidebar*")))
    (with-current-buffer buf
      (xidebar-mode)
      (setq xidebar--source-window source-window
            buffer-read-only t)
      (xidebar--render 'project-files source-window)
      (face-remap-set-base 'default 'xidebar-body)
      (face-remap-set-base 'fringe 'xidebar-body)
      (setq mode-line-format nil))
    buf))

(defun xidebar--get-window ()
  "Return the current xidebar window."
  (catch 'found
    (dolist (win (window-list))
      (when (window-parameter win 'xidebar-window)
        (throw 'found win)))))

(defun xidebar--display-buffer-setup-window (buffer)
  "Display BUFFER in a side window and setup the window."
  (let ((window (display-buffer-in-side-window
                 buffer '((side . left)
                          (window-width . 20)))))
    (set-window-parameter window 'xidebar-window t)
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-dedicated-p window t)))

(defun xidebar-toggle ()
  "Toggle xidebar in the current frame/window configuration."
  (interactive)
  (if-let ((xidebar-window (xidebar--get-window)))
      (let ((buffer (window-buffer xidebar-window)))
        ;; Because the window is dedicated, killing the buffer deletes
        ;; the window.
        (kill-buffer buffer))
    (xidebar--display-buffer-setup-window
     (xidebar--create (selected-window)))))

(defun xidebar-refresh (&optional update)
  "Refresh the xidebar in this frame/window configuration.

Use the selected window as the new source window.

Normally this function only makes sure the xidebar content
matches the new source window, but if UPDATE is t, also update
the content to make sure the content reflects the latest state of
the source window (new files, new functions, etc)."
  (interactive)
  (when (funcall xidebar-refresh-predicate)
    (when-let* ((xidebar-window (xidebar--get-window))
                (xidebar-buffer (window-buffer xidebar-window))
                ;; If selected window is xidebar window, use the old
                ;; source-window, otherwise use the selected window as
                ;; the new source window.
                (source-window (if (window-parameter
                                    (selected-window) 'xidebar-window)
                                   nil
                                 (selected-window))))
      (with-current-buffer xidebar-buffer
        (setq source-window (or source-window
                                xidebar--source-window))
        ;; First make sure the buffer content corresponds to the
        ;; current source buffer.
        (xidebar--render xidebar--current-mode
                         source-window
                         xidebar-window)
        (when update
          (xidebar--update xidebar--current-mode source-window))
        (setq xidebar--source-window source-window)))))

(defun xidebar-should-refresh-p ()
  "Don’t refresh in minibuffer and special mode."
  (and (not (minibufferp))
       (not (derived-mode-p 'special-mode))
       (not (window-parameter (selected-window) 'xidebar-window))))

;;; Xidebar switch

(defun xidebar-switch ()
  "Switch between modes in the xidebar."
  (interactive)
  (xidebar--render (if (eq xidebar--current-mode 'jmenu)
                       'project-files 'jmenu)
                   xidebar--source-window
                   (xidebar--get-window)))

(defvar-keymap xidebar-mode-map
  "x" #'xidebar-switch)

(define-derived-mode xidebar-mode special-mode "Xidebar"
  "Mode for the xidebar buffer."
  )

;;; Monitor

(defun xidebar--on-window-select (frame)
  "Refresh xidebar when any window is selected in FRAME.
Use the selected window as the source window for the xidebar."
  (ignore frame)
  (xidebar-refresh))

(defun xidebar--on-idle-timer ()
  "Refresh the content of the xidebar."
  (xidebar-refresh t))

(defvar xidebar--monitor-timer nil
  "A timer used by ‘xidebar-monitor-mode’.")

(define-minor-mode xidebar-monitor-mode
  "Monitor window change and live-update the xidebar."
  :global t
  (if xidebar-monitor-mode
      (progn
        (add-hook 'window-selection-change-functions
                  #'xidebar--on-window-select 0)
        (setq xidebar--monitor-timer
              (run-with-idle-timer
               5 t #'xidebar--on-idle-timer)))

    (add-hook 'window-selection-change-functions
              #'xidebar--on-window-select)
    (when xidebar--monitor-timer
      (cancel-timer xidebar--monitor-timer))))

;;; Memoization

;; I gave every mode a separate cache because I don’t want one mode’s
;; cache push out another mode’s cache, when they update at different
;; frequency.
(defvar xidebar--cache nil
  "A cache for xidebar content.

This is an alist ((MODE . DATA-ALIST)...), where MODE is a symbol
explaining which mode is the DATA-ALIST cache storing content
for, eg, ‘jmenu ‘project-files’.

DATA-ALIST is the actually cache alist of the form ((KEY .
CONTENT)), where KEY is compared with ‘eq’ or ‘equal’, and
CONTENT is a string (buffer string of the xidebar).")

;; I used USE-EQ rather than a more generic TESTFN, because a branch
;; is probably faster than funcall, but it probably doesn’t matter.
(defun xidebar--cache-get (mode key &optional use-eq)
  "Look for an entry for KEY in the cache for MODE.

If hit, return the value and move the entry to the front. If
USE-EQ is non-nil, use ‘eq’ rather than ‘equal’ for comparison."
  (when-let* ((cache (assoc mode xidebar--cache))
              (tail (cdr cache))
              (head cache))
    (catch 'hit
      (while tail
        (let* ((entry (car tail))
               (entry-key (car entry))
               (entry-val (cdr entry)))
          (when (if use-eq (eq key entry-key) (equal key entry-key))
            ;; Remove the this entry from the cache.
            (setcdr head (cdr tail))
            ;; Add then entry back at the top of the cache.
            (setcdr cache (cons entry (cdr cache)))
            (throw 'hit entry-val))
          (setq head tail
                tail (cdr tail)))))))

(defun xidebar--cache-set (mode key val &optional size-limit use-eq)
  "Insert/replace a cache entry (KEY . VAL) to the cache for MODE.

If there are more than SIZE-LIMIT entries in the cache already,
drop the oldest entry. If USE-EQ is non-nil, use ‘eq’ rather than
‘equal’ for comparison."
  (let ((cache (or (assoc mode xidebar--cache)
                   (progn (push (cons mode nil) xidebar--cache)
                          (assoc mode xidebar--cache)))))
    ;; If an entry with KEY already exists, replace its VAL.
    (if-let ((existing-entry (xidebar--cache-get mode key use-eq)))
        (setf (cdr (car (cdr cache))) val)
      ;; If not, insert a new entry into the cache.
      (let ((tail (cdr cache))
            (head cache)
            (idx 0))
        ;; Remove the SIZE-LIMIT’th entry and everything after, if it
        ;; exists.
        (while tail
          (if (and size-limit (>= idx (1- size-limit)))
              (setcdr head nil))
          (setq head tail
                tail (cdr tail)
                idx (1+ idx)))
        ;; Add the new entry at the front.
        (setcdr cache (cons (cons key val) (cdr cache)))))))

(provide 'xidebar)

;;; xidebar.el ends here

;;; switch-to-buffer-in-tab.el --- Switch to buffer in tab  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Imagine this scenario: you create several tabs for different
;; projects, tab 1 for project 1, tab 2 for project 2. You want to go
;; to a buffer in project 1, but you are in tab 2. You forgot you are
;; in tab 2. You use switch-to-buffer, now you have a project 1 buffer
;; in project 2.
;;
;; To avoid this, we can advice switch-to-buffer such that if a buffer
;; is displayed in another tab, and you switch to that buffer, instead
;; of switching to that buffer in the current tab and window, it
;; brings you to the tab that’s alreadying showing the buffer.
;;
;; That’s what ‘switch-to-buffer-in-tab’ does. In addition, it ignores
;; the current tab when looking for exising displayed buffers.
;;
;; And if occasionally the jumping isn’t what you want, you can revert
;; the effect of ‘switch-to-buffer-in-tab’ and use plain
;; ‘switch-to-buffer’ by typing C-k immediately after the jump
;; happens. This feature can be turned off by setting
;; ‘switch-to-buffer-in-tab-allow-revert’.

;;; Code:

(require 'cl-lib)
(require 'tab-bar)

(defvar stbit-allow-revert t
  "If t, allow typing C-k immediately after a switch to revert the effect.")

(defvar stbit--last-context nil
  "Stores the context of the last buffer switch.
A plist of (:tab TAB-NAME :window WINDOW :buffer (BUFFER NORECORD
FORCE-SAME-WINDOW)).")

(defvar stbit--revert-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") #'stbit-revert)
    map)
  "Transient keymap used for reverting ‘switch-to-buffer-in-tab’.")

(defun stbit--tab-buffers (tab)
  "Get buffers in TAB."
  (let ((window-state (alist-get 'ws tab)))
    (stbit--get-buffers (cddr window-state))))

;;;###autoload
(defun stbit--get-buffers (tree)
  "Traverse TREE and gather buffers in it.
TREE should be a cons. Somewhere in TREE should be a form
like (buffer <buffer name> ...). The <buffer name> is what we
collect."
  (if (consp tree)
      (if (eq (car tree) 'buffer)
          (list (cadr tree))
        (append (stbit--get-buffers (car tree))
                (stbit--get-buffers (cdr tree))))
    nil))

;;;###autoload
(defun switch-to-buffer-in-tab (buffer &optional norecord force-same-window)
  "Switch to BUFFER if it exists in some tab in current frame.

Specifically, switch to the tab that shows BUFFER (excluding
current tab), and select the window displaying BUFFER. Return the
window if success, nil if BUFFER isn’t displayed in any tab or if
there isn’t any tab.

NORECORD and FORCE-SAME-WINDOW is the same in ‘switch-to-buffer’."
  (setq stbit--last-context
        (list :buffer (list buffer norecord force-same-window)
              :window (selected-window)))
  (let*
      ((buffer (if (stringp buffer) buffer (buffer-name buffer)))
       (tabs (tab-bar-tabs))
       ;; Shape: ((tab-name . buffers) ...)
       (tabs-and-buffers
        (mapcar (lambda (tab)
                  (let* ((tab-type (car tab))
                         (info (cdr tab))
                         (tab-name (alist-get 'name info))
                         (window-state (alist-get 'ws info)))
                    ;; The ‘tab-type’ be either ‘tab’ or
                    ;; ‘current-tab’.
                    (if (eq tab-type 'tab)
                        (cons tab-name (stbit--get-buffers
                                        window-state))
                      (setq stbit--last-context
                            `(:tab ,tab-name ,@stbit--last-context))
                      'current-tab)))
                tabs))
       (current-tab-index
        (cl-position 'current-tab tabs-and-buffers :test #'eq))
       ;; Shape: ((idx . tab-name) ...)
       (tabs-that-include-buffer
        (cl-loop while tabs-and-buffers
                 for idx = 0 then (1+ idx)
                 for tab-and-buffers = (pop tabs-and-buffers)
                 if (and (consp tab-and-buffers)
                         (cl-find buffer (cdr tab-and-buffers)))
                 collect (cons idx (car tab-and-buffers))))
       ;; Sort the tabs by vicinity to the current tab.
       (tabs-sort-by-vicinity
        (cl-sort tabs-that-include-buffer
                 #'<= :key (lambda (elm)
                             (abs (- (car elm) current-tab-index))))))
    (if (null tabs-sort-by-vicinity)
        (switch-to-buffer buffer norecord force-same-window)
      (tab-bar-switch-to-tab (cdar tabs-sort-by-vicinity))
      (dolist (window (window-list))
        (when (eq (window-buffer window) buffer)
          (select-window window)))
      (when stbit-allow-revert
        (message "%s"
                 (let ((overriding-local-map stbit--revert-transient-map))
                   (substitute-command-keys "Type \\[switch-to-buffer-in-tab-revert] to revert the jump made by ‘switch-to-buffer-in-tab’")))
        (set-transient-map stbit--revert-transient-map)))))

(defun stbit-revert ()
  "Revert the last ‘switch-to-buffer-in-tab’.
And do what plain ‘switch-to-buffer’ would do."
  (interactive)
  (when stbit--last-context
    (tab-bar-switch-to-tab (plist-get stbit--last-context :tab))
    (select-window (plist-get stbit--last-context :window))
    (apply #'switch-to-buffer (plist-get stbit--last-context :buffer))))

(provide 'switch-to-buffer-in-tab)

;; Local Variables:
;; read-symbol-shorthands: (("stbit-" . "switch-to-buffer-in-tab-"))
;; End:

;;; stbit.el ends here

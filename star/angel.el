;; -*- lexical-binding: t -*-
;;
;; In the end, Angel defeats Evil.

(require 'pause)
(require 'cl-lib)
(require 'utility)

(autoload 'iscroll-down "iscroll.el")
(autoload 'iscroll-up "iscroll.el")

;;; Keys

(luna-def-key
 ;; Meta bindings
 "M-n"   #'scroll-up
 "M-p"   #'scroll-down
 "M-/"   #'hippie-expand
 ;; "M-f"   #'luna-forward-word
 ;; "M-b"   #'luna-backward-word

 ;; Control bindings
 "C-v"   #'set-mark-command
 "C-x C-r" #'recursive-edit

 ;; Super bindings
 "s-n"   #'luna-scroll-up-reserve-point
 "s-p"   #'luna-scroll-down-reserve-point
 "s-u"   #'revert-buffer

 ;; Jump back
 "C-o" #'pop-to-mark-command
 "M-o" #'winner-undo

 "C-x C-c" #'clean-exit

 ;; Remaps
 [remap backward-delete-char-untabify] #'luna-hungry-delete
 [remap delete-indentation] #'luna-hungry-delete
 [remap c-electric-backspace] #'luna-hungry-delete
 [remap move-beginning-of-line] #'smarter-move-beginning-of-line

 ;; Super -> Meta
 "s-<backspace>" (kbd "M-<backspace>")
 "s-d"   (kbd "M-d")
 "s-f"   (kbd "M-f")
 "s-b"   (kbd "M-b")
 "s-a"   (kbd "M-a")
 "s-e"   (kbd "M-e")
 "s-w"   (kbd "M-w")
 "s-q"   (kbd "M-q")
 "C-s-p" (kbd "C-M-p")
 "C-s-n" (kbd "C-M-n")
 "C-s-f" (kbd "C-M-f")
 "C-s-b" (kbd "C-M-b")
 "C-s-t" (kbd "C-M-t")
 "C-s-;" (kbd "C-M-;")
 "C-s-c" (kbd "C-M-c")

 ;; For some reason, keyboard macro doesn't work.
 "s-["   (kbd "M-[")
 "s-]"   (kbd "M-]")
 ;; Super -> Control
 "s-\\"  (kbd "C-\\")

 :prefix "C-x"
 "9"   (kbd "C-x 1 C-x 3")
 "C-v" #'rectangle-mark-mode
 "k"   '("kill-buffer" .
         (lambda () (interactive)
           (kill-buffer (current-buffer))))
 "C-," #'beginning-of-buffer ; as of <
 "C-." #'end-of-buffer ; as of >
 "C-b" #'switch-to-buffer
 "C-d" #'dired-jump
 
 :prefix "C-c"
 "C-b" #'switch-buffer-same-major-mode

 :---
 :keymaps 'prog-mode-map
 "M-a"   #'beginning-of-defun
 "M-e"   #'end-of-defun
 "C-M-f" #'forward-sexp
 "C-M-b" #'backward-sexp
 
 :keymaps 'text-mode-map
 "M-a"   #'backward-paragraph
 "M-e"   #'forward-paragraph
 "C-M-f" #'forward-sentence
 "C-M-b" #'backward-sentence)

;;; Scrolling

(defvar luna-scroll-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'luna-scroll-up-reserve-point)
    (define-key map (kbd "s-n") #'luna-scroll-up-reserve-point)
    (define-key map (kbd "p") #'luna-scroll-down-reserve-point)
    (define-key map (kbd "s-n") #'luna-scroll-up-reserve-point)
    map)
  "Transient map for `luna-scroll-mode'.")

(defvar luna-scroll-optimized-command-list
  '(luna-scroll-down-reserve-point
    luna-scroll-up-reserve-point
    iscroll-next-line
    iscroll-next-line)
  "Commands that inhibit `post-command-hook'.")

(defsubst luna-scroll-setup-optimize ()
  "Setup optimization and return the recover function."
  ;; Inhibit flyspell jit-lock and other stuff when scrolling. In
  ;; particular, flyspell is very slow.
  (declare (indent 1))
  (let* ((post-command-hook-backup post-command-hook)
         (pre-command-hook-backup pre-command-hook)
         (recover-fn
          (lambda ()
            (when (not (memq this-command
                             luna-scroll-optimized-command-list))
              (setq post-command-hook
                    post-command-hook-backup
                    pre-command-hook
                    pre-command-hook-backup)))))
    (when (not (memq last-command luna-scroll-optimized-command-list))
      ;; First in a series of luna scroll commands. Empty
      ;; `post-command-hook' and add a hook to recover it.
      (add-hook 'post-command-hook recover-fn 0 t))))

(defun luna-scroll-down-reserve-point ()
  "Scroll down `luna-scroll-amount' lines.
Keeps the relative position of point against window."
  (interactive)
  ;; (luna-scroll-setup-optimize)
  (if (derived-mode-p 'prog-mode)
      ;; This is actually better than
      ;; `scroll-preserve-screen-position'.
      (progn (scroll-down 3)
             (vertical-motion -3))
    (iscroll-down 2))
  ;; Prevent me from accidentally inserting n and p.
  (set-transient-map luna-scroll-map t))

(defun luna-scroll-up-reserve-point ()
  "Scroll up `luna-scroll-amount' lines.
Keeps the relative position of point against window."
  (interactive)
  ;; (luna-scroll-setup-optimize)
  (if (derived-mode-p 'prog-mode)
      (progn (scroll-up 3)
             (vertical-motion 3))
    (iscroll-up 2))
  (set-transient-map luna-scroll-map t))

;;; Better C-a

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;; Query Replace+ (cgn)

(defun query-replace+ (beg end &optional delete)
  "Select region between BEG and END and query replace it.
Edit the underlined region and type C-M-c to start
`query-replace'. Type C-g or C-] to abort. If DELETE non-nil,
delete region when invoked."
  (interactive "r")
  (if (not (region-active-p))
      (message "Select the text to be replaced first")
    (let ((string (buffer-substring-no-properties
                   beg end))
          (ov (make-overlay beg end nil nil t)))
      (deactivate-mark)
      (when delete (delete-region beg end))
      (overlay-put ov 'face '(:underline t))
      (pause
        (query-replace string (buffer-substring-no-properties
                               (overlay-start ov)
                               (overlay-end ov)))
        nil
        (delete-overlay ov)))))

(defun query-replace+delete (beg end)
  "Delete region between BEG and END and query replace it.
Edit the underlined region and type C-c C-c to start
`query-replace'. Type C-g to abort."
  (interactive "r")
  (query-replace+ beg end t))

;;; Better isearch

;; https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs
(defun luna-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (kill-ring-save nil nil t)
    (deactivate-mark)
    (isearch-yank-kill)))

(defun luna-rcenter-advice (&rest _) (recenter))
(advice-add 'isearch-repeat-forward :after #'luna-rcenter-advice)
(advice-add 'isearch-repeat-backward :after #'luna-rcenter-advice)
(add-hook 'isearch-mode-hook #'luna-isearch-with-region)

;;; Transient map in region (y p)

;; Unlike `emulation-mode-map-alists', luna-def-key allows more
;; flexibility for the predicate.
(luna-def-key
 :when (lambda ()
         (and mark-active (not (derived-mode-p 'magit-status-mode))))
 :keymaps 'override
 "p" '("override-paste" . (lambda (b e)
                            (interactive "r")
                            (delete-region b e) (yank)))
 "x" #'exchange-point-and-mark
 ";" #'comment-dwim
 "y" #'kill-ring-save
 "C-y" #'kill-ring-save
 "Y" '("copy-&-keep-region" .
       (lambda
         (b e)
         (interactive "r")
         (kill-new (buffer-substring b e))
         (message "Region saved")))
 "r" #'query-replace+
 "R" #'query-replace+delete
 ;; isolate
 "s" #'isolate-quick-add
 "S" #'isolate-long-add
 "d" #'isolate-quick-delete
 "D" #'isolate-long-delete
 "c" #'isolate-quick-change
 "C" #'isolate-long-change)

;;; Hungrey delete

(defun luna-hungry-delete (&rest _)
  "Delete backwards and also take away white spaces around point."
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (catch 'end
      (let ((p (point)) beg end line-count)
        (save-excursion
          (skip-chars-backward " \t\n")
          (setq beg (point))
          (goto-char p)
          (skip-chars-forward " \t\n")
          (setq end (point)))
        (setq line-count
              (cl-count ?\n (buffer-substring-no-properties beg end)))
        (if (or (eq beg end)
                (eq (ppss-depth (syntax-ppss)) 0)
                (save-excursion (skip-chars-backward " \t")
                                (not (eq (char-before) ?\n))))
            (backward-delete-char-untabify 1)
          (delete-region beg end)
          (cond ((eq (char-after) ?})
                 (insert "\n")
                 (indent-for-tab-command))
                ((eq (char-after) ?\))
                 nil)
                ((> line-count 1)
                 (insert "\n")
                 (indent-for-tab-command))
                (t (insert " "))))))))

;;; Jump back

(defun push-mark-unless (&rest _)
  "Push a marker to `mark-ring'...
...unless a marker in the same line already exists."
  (interactive)
  (let ((marker (catch 'ret
                  (dolist (marker mark-ring)
                    (if (<= (line-beginning-position)
                            (marker-position marker)
                            (line-end-position))
                        (throw 'ret marker))))))
    (if marker
        (progn
          (setq mark-ring (remove marker mark-ring))
          (push marker mark-ring))
      (push (point-marker) mark-ring))
    (message "Mark set")))

(defun mark-before (command)
  "Add advice after COMMAND to push mark before it executes."
  (advice-add command :before #'push-mark-unless))

(mark-before #'isearch-forward)
(mark-before #'isearch-backward)
(mark-before #'end-of-buffer)
(mark-before #'beginning-of-buffer)
(mark-before #'luna-jump-back)
(mark-before #'counsel-imenu)

;;; Clean exit

(defun clean-exit ()
  "Exit Emacs cleanly.
If there are unsaved buffer, pop up a list for them to be saved
before existing. Replaces ‘save-buffers-kill-terminal’."
  (interactive)
  (if (frame-parameter nil 'client)
      (server-save-buffers-kill-terminal arg)
    (if-let ((buf-list (seq-filter (lambda (buf)
                                     (and (buffer-modified-p buf)
                                          (buffer-file-name buf)))
                                   (buffer-list))))
        (progn
          (pop-to-buffer (list-buffers-noselect t buf-list))
          (message "s to save, C-k to kill, x to execute"))
      (save-buffers-kill-emacs))))

;;; angel.el ends here

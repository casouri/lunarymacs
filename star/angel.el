;; -*- lexical-binding: t -*-
;;
;; In the end, good defeats Evil.

(require 'pause)
(require 'cl-lib)
(require 'utility)
(require 'transform)
(require 'iscroll)

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
 "C-,"   #'luna-jump-back

 ;; Super bindings
 "s-n"   #'luna-scroll-up-reserve-point
 "s-p"   #'luna-scroll-down-reserve-point
 "s-/"   #'transform-previous-char
 "s-u"   #'revert-buffer

 ;; Etc
 "C-M-;" #'inline-replace

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
 ;; For some reason, keyboard macro doesn't work.
 "s-!"   #'shell-command
 ;; Super -> Control
 "s-\\"  (kbd "C-\\")

 :prefix "C-x"
 "9"   (kbd "C-x 1 C-x 3")
 "C-f" #'luna-find-file
 "C-v" #'rectangle-mark-mode
 "`"   #'luna-expand-window
 "k"   '("kill-buffer" .
         (lambda (&optional arg) (interactive "p")
           (if (eq arg 4)
               (call-interactively #'kill-buffer)
             (kill-buffer (current-buffer)))))
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

;;; Navigation (w W e E b B)
;;
;; Obsolete
;;
;; Forward/backward stop at the beginning of the next word and also
;; line end/beginning. For CJK characters, move by four characters.

(defun luna-forward-word ()
  (interactive)
  (cl-labels ((ideograph-p (ch) (aref (char-category-set ch) ?|)))
    (cond ((ideograph-p (char-after))
           (dotimes (_ 4)
             (when (ideograph-p (char-after))
               (forward-char))))
          ((looking-at "\n")
           (skip-chars-forward "\n"))
          ((looking-at "[[:alpha:]]")
           (progn (while (and (looking-at "[[:alpha:]]")
                              (not (ideograph-p (char-after))))
                    (forward-char))
                  (skip-chars-forward "^[:alpha:]\n")))
          (t (skip-chars-forward "^[:alpha:]\n")))))

(defun luna-backward-word ()
  (interactive)
  (cl-labels ((ideograph-p (ch) (aref (char-category-set ch) ?|)))
    (cond ((ideograph-p (char-before))
           (dotimes (_ 4)
             (when (ideograph-p (char-before))
               (backward-char))))
          ((looking-back "\n" 1)
           (skip-chars-backward "\n"))
          ((looking-back "[[:alpha:]\n]" 1)
           (progn (while (and (looking-back "[[:alpha:]]" 1)
                              (not (ideograph-p (char-before))))
                    (backward-char))
                  (skip-chars-backward "^[:alpha:]\n")))
          (t (skip-chars-backward "^[:alpha:]\n")))))

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
    angel-scroll-next-line
    angel-scroll-previous-line)
  "Commands that inhibit `post-command-hook'.")

(defvar-local luna-scroll-post-command-hook-backup nil
  "Backup for `post-command-hook'.")

(defvar luna-scroll-recover-post-command-hook-timer nil
  "Timer for recovering `post-command-hook'.")

(defsubst luna-scroll-setup-optimize ()
  "Setup optimization and return the recover function."
  ;; Inhibit flyspell jit-lock and other stuff when scrolling. In
  ;; particular, flyspell is very slow.
  (declare (indent 1))
  (let ((recover-fn
         (lambda ()
           (setq post-command-hook
                 luna-scroll-post-command-hook-backup
                 luna-scroll-recover-post-command-hook-timer
                 nil))))
    (unless (memq last-command luna-scroll-optimized-command-list)
      ;; Empty `post-command-hook'...
      (setq luna-scroll-post-command-hook-backup post-command-hook
            post-command-hook nil)
      ;; ...and set a timer to recover it. When the timer runs, it
      ;; sets itself to nil.
      (unless luna-scroll-recover-post-command-hook-timer
        (setq luna-scroll-recover-post-command-hook-timer
              (run-with-idle-timer 0.2 nil recover-fn))))))

(defun luna-scroll-down-reserve-point ()
  "Scroll down `luna-scroll-amount' lines.
Keeps the relative position of point against window."
  (interactive)
  (luna-scroll-setup-optimize)
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
  (luna-scroll-setup-optimize)
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
Edit the underlined region and type C-c C-c to start
`query-replace'. Type C-g to abort. If DELETE non-nil, delete
region when invoked."
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
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))

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

;;; Inline replace (:s)

(defvar inline-replace-last-input "")
(defvar inline-replace-history nil)
(defvar inline-replace-count 1)
(defvar inline-replace-original-buffer nil)
(defvar inline-replace-overlay nil)
(defvar inline-replace-beg nil)

(defvar inline-replace-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-p") #'inline-replace-previous)
    (define-key map (kbd "C-n") #'inline-replace-next)
    map))

(defun inline-replace-previous ()
  "Previous match."
  (interactive)
  (when (> inline-replace-count 1)
    (cl-decf inline-replace-count)))

(defun inline-replace-next ()
  "Next match."
  (interactive)
  (cl-incf inline-replace-count))

(defun inline-replace ()
  "Search for the matching REGEXP COUNT times before END.
You can use \\&, \\N to refer matched text."
  (interactive)
  (condition-case nil
      (save-excursion
        (setq inline-replace-beg
              (progn (beginning-of-line) (point-marker)))
        (setq inline-replace-original-buffer (current-buffer))
        (add-hook 'post-command-hook #'inline-replace-highlight)

        (let* ((minibuffer-local-map inline-replace-minibuffer-map)
               (input (read-string "regexp/replacement: " nil
                                   'inline-replace-history))
               (replace (or (nth 1 (split-string input "/")) "")))
          (goto-char inline-replace-beg)
          (ignore-errors
            (re-search-forward
             (car (split-string input "/"))
             (line-end-position) t inline-replace-count))

          (unless (equal input inline-replace-last-input)
            (push input inline-replace-history)
            (setq inline-replace-last-input input))
          (remove-hook 'post-command-hook #'inline-replace-highlight)
          (delete-overlay inline-replace-overlay)
          (replace-match replace)
          (setq inline-replace-count 1)))
    ((quit error)
     (delete-overlay inline-replace-overlay)
     (remove-hook 'post-command-hook #'inline-replace-highlight)
     (setq inline-replace-count 1))))

(defun inline-replace-highlight ()
  "Highlight matched text and replacement."
  (when inline-replace-overlay
    (delete-overlay inline-replace-overlay))
  (when (>= (point-max) (length "regexp/replacement: "))
    (let* ((input (buffer-substring-no-properties
                   (1+ (length "regexp/replacement: ")) (point-max)))
           (replace (or (nth 1 (split-string input "/")) "")))
      (with-current-buffer inline-replace-original-buffer
        (goto-char inline-replace-beg)
        ;; if no match and count is greater than 1, try to decrease
        ;; count this way if there are only 2 match, you can't
        ;; increase count to anything greater than 2
        (while (and (not (ignore-errors
                           (re-search-forward
                            (car (split-string input "/"))
                            (line-end-position) t inline-replace-count)))
                    (> inline-replace-count 1))
          (decf inline-replace-count))
        (setq inline-replace-overlay
              (make-overlay (match-beginning 0) (match-end 0)))
        (overlay-put inline-replace-overlay
                     'face '(:strike-through t :background "#75000F"))
        (overlay-put inline-replace-overlay 'after-string
                     (propertize replace 'face
                                 '(:background "#078A00")))))))

;;; Jump back

(defvar luna-jump-back-marker nil
  "Marker set for `luna-jump-back'.")

(defvar luna-jump-back-monitored-command-list
  '(swiper helm-swoop isearch-forward isearch-backward
           end-of-buffer beginning-of-buffer query-replace
           replace-string counsel-imenu query-replace+
           query-replace+delete)
  "Set mark before running these commands.")

(defun luna-jump-back ()
  "Jump back to previous position."
  (interactive)
  (if (not luna-jump-back-marker)
      (message "No marker set")
    ;; Set `luna-jump-back-marker' to point and jump so we can jump
    ;; back.
    (let ((here (point-marker))
          (there luna-jump-back-marker))
      (setq luna-jump-back-marker here)
      (goto-char there))))

(defun luna-maybe-set-marker-to-jump-back ()
  "Set marker to jump back if this command is search or jump."
  (when (member this-command luna-jump-back-monitored-command-list)
    (setq luna-jump-back-marker (point-marker))))

(add-hook 'pre-command-hook #'luna-maybe-set-marker-to-jump-back)

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

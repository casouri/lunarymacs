;;; -*- lexical-binding: t -*-

(require 'pause)
(require 'cl-lib)

;;; Keys

(with-eval-after-load 'luna-general-config
  (general-define-key
   "M-n"   #'scroll-up
   "M-p"   #'scroll-down
   "M-/"   #'hippie-expand

   "s-n"   #'luna-scroll-down-reserve-point
   "s-p"   #'luna-scroll-up-reserve-point
   ;; "s-a"   #'backward-sentence
   ;; "s-e"   #'forward-sentence
   "M-%"   #'query-replace+
   "C-,"   #'luna-jump-back

   "M-j"   #'find-char

   "C-'"   #'luna-set-mark
   "M-'"   #'luna-jump
   "C-M-;" #'inline-replace
   "M-f"   #'next-char
   "M-b"   #'last-char

   "C-."   #'undo-tree-redo

   "M-v"   #'select-line
   "C-="   #'er/expand-region

   "C-M-0" #'forward-sexp ; \)
   "C-M-9" #'backward-sexp ; ;\(
   "C-M-p" #'luna-up-list-backward
   "C-M-n" #'down-list

   "C-v"   #'set-mark-command

   "s-D"   '((lambda () (interactive)
               (shell-command-to-string
                (format "open dict://%s"
                        (if (not (region-active-p))
                            (symbol-at-point)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))))
             :wk "search in Dictionary"))

  (general-define-key
   :prefix "C-x"
   "C-f" #'luna-find-file
   "C-u" #'undo-tree-visualize
   "C-v" #'cua-rectangle-mark-mode
   "`"   #'luna-expand-window
   "k"   '((lambda (&optional arg) (interactive)
             (if (eq arg 4)
                 (call-interactively #'kill-buffer)
               (kill-buffer (current-buffer))))
           :which-key "kill-buffer")
   "C-," #'beginning-of-buffer ; as of <
   "C-." #'end-of-buffer ; as of >
   "C-b" #'switch-to-buffer
   "C-d" '((lambda () (interactive) (dired default-directory)) :which-key "open default directory")
   "j" #'luna-jump-or-set)

  (general-define-key
   :prefix "C-c"
   "C-b" #'switch-buffer-same-major-mode))

(defvar luna-scroll-map (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "n") #'luna-scroll-down-reserve-point)
                          (define-key map (kbd "p") #'luna-scroll-up-reserve-point)
                          map)
  "Transient map for `luna-scroll-mode'.")

(define-minor-mode luna-scroll-mode
  "Scroll and not insert n/p accidentally when switching between C-n/p and M-n/p."
  :lighter ""
  (if luna-scroll-mode
      (set-transient-map luna-scroll-map t)))

(advice-add #'luna-scroll-down-reserve-point :after (lambda () (luna-scroll-mode)))
(advice-add #'luna-scroll-up-reserve-point :after (lambda () (luna-scroll-mode)))

;;; Navigation (w W e E b B)
;;
;; Overall behavior:
;;
;; last-char goes back a word, stops at beginning of line and parenthesis
;; next-char goes foward a wrod, stops at end of line and parenthesis

(defmacro forward-char-while (condition &optional whitespace-charset)
  "Go forward while CONDITION evaluate to t.
But if hit newline, stop,  rollback (skipping spaces) and throw 'return.
If WHITESPACE-CHARSET is non-nil,
chars in it will be used as white space char (to be skipped over when rolling bck)."
  `(while ,condition
     (when (eq (char-after) ?\n)
       (while (member (char-after) (or ,whitespace-charset '(?\s)))
         (backword-char))
       (throw 'return nil))
     (forward-char)))

(defmacro backward-char-while (condition &optional whitespace-charset)
  "Go backward while CONDITION evaluate to t.
But if hit newline, stop,  rollback (skipping spaces) and throw 'return.
If WHITESPACE-CHARSET is non-nil,
chars in it will be used as white space char (to be skipped over when rolling bck)."
  `(while ,condition
     (when (eq (char-before) ?\n)
       (while (member (char-after) (or ,whitespace-charset '(?\s)))
         (forward-char))
       (throw 'return nil))
     (backward-char)))

(defsubst next-of (charset &optional stop-charset whitespace-charset)
  "Forward until hit char from CHARSET. Or before a char from STOP-CHARSET.
But if hit newline, stop,  rollback (skipping spaces) and throw 'return.
If WHITESPACE-CHARSET is non-nil,
chars in it will be used as white space char (to be skipped over when rolling back)."
  ;; skip over stop-charset char if you are already on one
  (catch 'return
    ;; skip over eol if already on it
    (when (eq (char-after) ?\n) (forward-char))
    (when stop-charset
      (forward-char-while (member (char-after) stop-charset)))
    ;; skip over charset car if you are already on one
    (forward-char-while (member (char-after) charset))
    ;; go forwarduntill hit a char not from charset
    (unless (member (char-after) stop-charset)
      (forward-char-while (not (member (char-after) charset))))))

(defsubst last-of (charset &optional stop-charset)
  "Backward until hit char from CHARSET. Or before a char from STOP-CHARSET.
But if hit newline, stop, rollback (skipping spaces) and throw 'return.
If WHITESPACE-CHARSET is non-nil,
chars in it will be used as white space char (to be skipped over when rolling back)."
  (catch 'return
    ;; skip over eol if already on it
    (when (eq (save-excursion (back-to-indentation) (point)) (point))
      (beginning-of-line) (backward-char) (throw 'return nil))
    (when stop-charset
      (backward-char-while (member (char-before) stop-charset)))
    (backward-char-while (member (char-before) charset))
    (unless (member (char-before) stop-charset)
      (backward-char-while (not (member (char-before) charset))))))

(defun next-space ()
  "Go to next space."
  (interactive)
  (next-of '(?\s ?\n ?\t) '(?\( ?\))))

(defun last-space ()
  "Go to last space."
  (interactive)
  (last-of '(?\s ?\n ?\t) '(?\( ?\))))

(defun next-space-char ()
  "Go to next char after space."
  (interactive)
  (next-of '(?\s ?\n ?\t))
  (forward-char))

(defun last-space-char ()
  "Go to last char before space."
  (interactive)
  (last-of '(?\s ?\n ?\t))
  (backward-char))

(defvar punc-list '(?` ?` ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\( ?\)
                       ?- ?_ ?= ?+ ?\[ ?\] ?{ ?} ?\\ ?| ?\;
                       ?: ?' ?\" ?, ?< ?. ?> ?/ ??))

(defun next-punc ()
  "Go to next punctuation."
  (interactive)
  (next-of punc-list))

(defun last-punc ()
  "Go to next punctuation. Do ARG times."
  (interactive)
  (last-of punc-list))

(defvar char-list '(?Q ?q ?W ?w ?E ?e ?R ?r ?T ?t ?Y ?y
                       ?U ?u ?I ?i ?O ?o ?P ?p ?A ?a ?S
                       ?s ?D ?d ?F ?f ?G ?g ?H ?h ?J ?j
                       ?K ?k ?L ?l ?Z ?z ?X ?x ?C ?c ?V
                       ?v ?B ?b ?N ?n ?M ?m ?1 ?2 ?3 ?4
                       ?5 ?6 ?7 ?8 ?9 ?0))

(defun next-char (&optional arg)
  "Go to next character. Do ARG times."
  (interactive "p")
  (next-of char-list '(?\( ?\))))

(defun last-char (&optional arg)
  "Go to next character. Do ARG times."
  (interactive "p")
  (last-of char-list '(?\( ?\))))

(defun select-line ()
  "Select whole line."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

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

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;; Query Replace+ (cgn)

(defun query-replace+ (beg end)
  "Select a region and invoke this function.
Edit the underlined region and press C-c C-c to qurey-replace."
  (interactive "r")
  (if (not (region-active-p))
      (message "Select the text to be replaced first")
    (let ((string (buffer-substring-no-properties
                   beg end))
          (ov (make-overlay beg end nil nil t)))
      (deactivate-mark)
      (overlay-put ov 'face '(:underline t))
      (pause
        (query-replace string (buffer-substring-no-properties
                               (overlay-start ov)
                               (overlay-end ov)))
        nil
        (delete-overlay ov)))))

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

(defconst angel-transient-mode-map-alist
  `((mark-active
     ,@(let ((map (make-sparse-keymap)))
         ;; operations
         (define-key map "p" (lambda (b e)
                               (interactive "r") (delete-region b e) (yank)))
         (define-key map "x" #'exchange-point-and-mark)
         (define-key map ";" #'comment-dwim)
         (define-key map "y" #'kill-ring-save)
         (define-key map (kbd "C-y") #'kill-ring-save)
         (define-key map "Y" (lambda
                               (b e)
                               (interactive "r")
                               (kill-new (buffer-substring b e))
                               (message "Region saved")))
         ;; isolate
         (define-key map "s" #'isolate-quick-add)
         (define-key map "S" #'isolate-long-add)
         (define-key map "d" #'isolate-quick-delete)
         (define-key map "D" #'isolate-long-delete)
         (define-key map "c" #'isolate-quick-change)
         (define-key map "C" #'isolate-long-change)
         ;; mark things
         (define-key map "f" #'er/mark-defun)
         (define-key map "w" #'er/mark-word)
         (define-key map "W" #'er/mark-symbol)
         (define-key map "P" #'mark-paragraph)
         ;; inner & outer
         ;; (define-key map "C-i" inner-map)
         ;; (define-key map "C-a" outer-map)
         ;; (define-key inner-map "q" #'er/mark-inside-quotes)
         ;; (define-key outer-map "q" #'er/mark-outside-quotes)
         ;; (define-key inner-map "b" #'er/mark-inside-pairs)
         ;; (define-key outer-map "b" #'er/mark-outside-pairs)
         (define-key map "q" #'er/mark-inside-quotes)
         (define-key map "b" #'er/mark-inside-pairs)

         ;; expand-region
         (define-key map (kbd "C--") #'er/contract-region)
         (define-key map (kbd "C-=") #'er/expand-region)
         map))))

(add-to-list 'emulation-mode-map-alists
             'angel-transient-mode-map-alist t)

;;; Jump char (f)

(load-package find-char
  :commands (find-char find-char-backward-cmd))

;;; Switch buffer same major mode

(defun switch-buffer-same-major-mode ()
  "Switch buffer among those who have the same major mode as the current one."
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

;;; Inline replace (:s)

(defvar inline-replace-last-input "")
(defvar inline-replace-history nil)
(defvar inline-replace-count 1)
(defvar inline-replace-original-buffer nil)
(defvar inline-replace-overlay nil)
(defvar inline-replace-beg nil)

(defvar inline-replace-minibuffer-map (let ((map minibuffer-local-map))
                                        (define-key map (kbd "C-p") #'inline-replace-previous)
                                        (define-key map (kbd "C-n") #'inline-replace-next)
                                        map))

(defun inline-replace-previous ()
  "Previous match."
  (interactive)
  (when (> inline-replace-count 1)
    (decf inline-replace-count)))

(defun inline-replace-next ()
  "Next match."
  (interactive)
  (incf inline-replace-count))

(defun inline-replace ()
  "Search for the matching REGEXP COUNT times before END.
You can use \\&, \\N to refer matched text."
  (interactive)
  (condition-case nil
      (save-excursion
        (setq inline-replace-beg (progn (beginning-of-line) (point-marker)))
        (setq inline-replace-original-buffer (current-buffer))
        (add-hook 'post-command-hook #'inline-replace-highlight)

        (let* ((minibuffer-local-map inline-replace-minibuffer-map)
               (input (read-string "regexp/replacement: " nil 'inline-replace-history))
               (replace (or (nth 1 (split-string input "/")) "")))
          (goto-char inline-replace-beg)
          (ignore-errors (re-search-forward (car (split-string input "/")) (line-end-position) t inline-replace-count))

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
    (let* ((input (buffer-substring-no-properties (1+ (length "regexp/replacement: ")) (point-max)))
           (replace (or (nth 1 (split-string input "/")) "")))
      (with-current-buffer inline-replace-original-buffer
        (goto-char inline-replace-beg)
        ;; if no match and count is greater than 1, try to decrease count
        ;; this way if there are only 2 match, you can't increase count to anything greater than 2
        (while (and (not (ignore-errors (re-search-forward (car (split-string input "/")) (line-end-position) t inline-replace-count)))
                    (> inline-replace-count 1))
          (decf inline-replace-count))
        (setq inline-replace-overlay (make-overlay (match-beginning 0) (match-end 0)))
        (overlay-put inline-replace-overlay 'face '(:strike-through t :background "#75000F"))
        (overlay-put inline-replace-overlay 'after-string (propertize replace 'face '(:background "#078A00")))))))

;;; Jump back

(defvar luna-marker-alist nil
  "An alist of (char . marker).")

(defun luna-jump-or-set (char)
  "Jump to register CHAR if CHAR is lowercase.
Set register CHAR to point if CHAR is uppercase."
  (interactive "cRegister <- Char(a/A)")
  (let ((lower-char (downcase char)))
    (if (eql lower-char char)
        ;; lower case, jump
        (if-let ((marker (alist-get lower-char luna-marker-alist)))
            (goto-char marker)
          (message "Register %c unset" char))
      ;; upper case, set
      (setf (alist-get lower-char luna-marker-alist)
            (point-marker)))))

(defvar luna-jump-back-marker nil
  "Marker set for `luna-jump-back'.")

(defvar luna-jump-back-monitored-command-list
  '(isearch-forward helm-swoop isearch-backward end-of-buffer beginning-of-buffer query-replace replace-string)
  "Commands in this list sets mark before execution for jumping back later.")

(defun luna-jump-back ()
  "Jump back to previous position."
  (interactive)
  (if (not luna-jump-back-marker)
      (message "No marker set")
    ;; set `luna-jump-back-marker' to point and jump back
    ;; so we can keep jumping back and forth
    (let ((here (point-marker))
          (there luna-jump-back-marker))
      (setq luna-jump-back-marker here)
      (goto-char there))))

(defun luna-maybe-set-marker-to-jump-back ()
  "Set marker to jump back if this command is search or jump."
  (when (member this-command luna-jump-back-monitored-command-list)
    (setq luna-jump-back-marker (point-marker))))

(add-hook 'pre-command-hook #'luna-maybe-set-marker-to-jump-back)

;;; Abbrev

(defun luna-insert-space-or-expand-abbrev ()
  "Expand abbrev if previous char is a space, then insert space."
  (interactive)
  (if (not (equal (char-before) ?\s))
      (insert-char ?\s)
    (backward-char)
    (unless (expand-abbrev)
      (insert-char ?\s))
    (forward-char)))

;; (global-set-key " " #'luna-insert-space-or-expand-abbrev)
(read-abbrev-file (luna-f-join user-emacs-directory "star/abbrev-file.el"))

;;; find file project

(defun luna-find-file (&optional arg)
  "Find file. If called with ARG, find file in project."
  (interactive "p")
  (call-interactively
   (if (eq arg 4)
       #'project-find-file
     #'find-file)))

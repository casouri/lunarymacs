;;; -*- lexical-binding: t -*-

;;; Keys

(with-eval-after-load 'general
  (general-define-key
   "s-n"   #'luna-scroll-down-reserve-point
   "s-p"   #'luna-scroll-up-reserve-point)

  (general-define-key
   :keymas minibuffer-local-map
   "C-<return>" #'newline)

  (general-define-key
   :keymaps 'override
   "M-j"   #'find-char
   "C-M-'" #'find-char-backward-cmd
   "M-'"   #'avy-goto-char
   "C-M-;" #'inline-replace
   "M-f"   #'next-char
   "M-b"   #'last-char

   "M-y"   #'kill-region

   "C-."   #'undo-tree-redo

   "M-v"   #'select-line
   "C-="   #'er/expand-region

   "C-M-0" #'forward-sexp ; \)
   "C-M-9" #'backward-sexp ; ;\(
   "C-M-p" #'luna-up-list-backward
   "C-M-n" #'down-list

   "C-v"   #'set-mark-command

   ;; "C-h" (general-simulate-key "C-b")
   ;; "C-l" (general-simulate-key "C-f")
   ;; "C-j" (general-simulate-key "C-n")
   ;; "C-k" (general-simulate-key "C-p")
   ;; "M-h" (general-simulate-key "M-b")
   ;; "M-l" (general-simulate-key "M-f")
   ;; "M-j" (general-simulate-key "M-n")
   ;; "M-k" (general-simulate-key "M-p")

   "M--" #'delete-other-windows
   "M-0" #'luna-quit-window
   "M-9" '((lambda () (interactive) (kill-buffer (current-buffer))) :which-key "kill-current-buffer")
   )

  (luna-cx-leader
   "C-u" #'undo-tree-visualize
   "C-v" #'cua-rectangle-mark-mode
   "9"   '((lambda () (interactive) (kill-buffer (current-buffer))) :which-key "kill-current-buffer")
   "0"   #'luna-quit-window
   "C-," #'beginning-of-buffer ; as of <
   "C-." #'end-of-buffer ; as of >
   "C-b" #'switch-to-buffer
   "C-;" #'goto-last-change
   "M-;" #'goto-last-change-reverse))

(defun luna-up-list-backward ()
  "`up-list' but up to the beginning instead of the end."
  (interactive)
  (up-list)
  (backward-list))


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

;;; Navigation
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
But if hit newline, stop,  rollback (skipping spaces) and throw 'return.
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

;;; Query Replace +

(defvar query-replace+-mode-overlay nil
  "Overlay of region to be replaced.")

(defvar query-replace+-mode-from-string nil
  "The from-string for `query-replace'.")

(define-minor-mode query-replace+-mode
  "Edit region and query replace."
  :lighter "QUERY"
  (if query-replace+-mode
      (if (not mark-active)
          (setq query-replace+-mode nil)
        (overlay-put
         (setq query-replace+-mode-from-string
               (buffer-substring
                (region-beginning)
                (region-end))
               query-replace+-mode-overlay
               (make-overlay (region-beginning)
                             (region-end)
                             nil
                             nil
                             t))
         'face '(:inherit region)))
    (overlay-put query-replace+-mode-overlay
                 'face '(:inherit lazy-highlight))
    (goto-char (overlay-end
                query-replace+-mode-overlay))
    (query-replace query-replace+-mode-from-string
                   (buffer-substring-no-properties
                    (overlay-start
                     query-replace+-mode-overlay)
                    (overlay-end
                     query-replace+-mode-overlay)))
    (delete-overlay
     query-replace+-mode-overlay)))

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

;;; Transient map in region

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


;;; Jump char

(load-package find-char
  :commands (find-char find-char-backward-cmd))

;;; Inline replace


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

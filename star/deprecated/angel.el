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


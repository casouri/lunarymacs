;;; info+.el --- Prettier Info      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Scratch
;;
;; (let ((reg (Info--next-block)))
;;   (Info--block-type (car reg) (cdr reg)))

;; (defun highlight-region (reg)
;;   (set-mark (car reg))
;;   (goto-char (cdr reg))
;;   (transient-mark-mode))

;; (highlight-region (Info--next-block))

;;; Code:
;;

(require 'cl-lib)
(require 'pcase)

;; Block types:
;; | Body indent1 indent2
;; | BulletBody indent1 indent2
;; | DetailList indent1 indent2
;; | MenuHeader
;; | MenuEntry align
;; | Definition indent
;; | Code

(defface info-body `((t . (:inherit variable-pitch :height 1.2)))
  "Face for body text in Info buffer."
  :group 'info)

(defface info-inline-code `((t . (:inherit shadow)))
  "Face for inline code in Info buffer."
  :group 'info)

(defun Info--next-block ()
  "Return (BEG . END) of next text block after point.
Move point to BEG.
If search failed, return nil."
  (condition-case nil
      (let (beg end)
        ;; Non-empty line
        (re-search-forward "^[^\n]+$")
        (setq beg (match-beginning 0))
        (if (re-search-forward
             (rx (or "\n\n"
                     (seq "\n" (* " ") digit "." (+ " "))
                     (seq "\n* ")))
             nil t)
            (setq end (match-beginning 0))
          (setq end (point-max)))
        (cons beg end))
    (search-failed nil)))

(defsubst Info--menu-entry-detail-beg (limit)
  "Go to the beginning of the entry detail before LIMIT.
Assumes the point is at BOL.
Return nil if not found"
  (re-search-forward
   (rx (seq "*" " " (+ (not (any "\n"))) (group (>= 2 " "))))
   limit t))

(defun Info--block-type (beg end)
  "Return the type of the block between BEG and END.
Moves point."
  ;;       Code block
  (cl-labels ((indent () (- (point) (line-beginning-position))))
    (cond ((progn (goto-char beg)
                  (looking-at "* Menu:"))
           '(MenuHeader))
          ;; Menu (header or entry)
          ((progn (goto-char beg)
                  (looking-at "\\*"))
           (if (Info--menu-entry-detail-beg (line-end-position))
               `(MenuEntry ,(indent))
             '(MenuEntry 0)))
          ;; Definition
          ((progn (goto-char beg)
                  (skip-chars-forward " ")
                  (eq (char-after) ?-))
           (re-search-forward "\n +")
           `(Definition ,(indent)))
          ;; Body
          ((progn (goto-char beg)
                  (skip-chars-forward " ")
                  (or (looking-at "[0-9]\\.")
                      (looking-at "•")
                      (looking-at "[[:upper:]]")))
           (goto-char beg)
           (let (indent1 indent2)
             (skip-chars-forward " ")
             (setq indent1 (indent))
             (when (re-search-forward "\n" end t)
               (skip-chars-forward " ")
               (setq indent2 (indent)))
             (cond ((and indent2
                         (progn (goto-char beg)
                                (looking-at " +•")))
                    `(BulletBody ,indent1 ,(+ 2 indent1)))
                   ;; List
                   ((progn (goto-char beg)
                           (looking-at (rx (seq (* " ")
                                                (or digit upper)
                                                "."))))
                    `(BulletBody ,indent1 ,(+ 3 indent1)))
                   ;; Detail list
                   ((and indent2 (< indent1 indent2))
                    `(DetailList ,indent1 ,indent2))
                   ;; Body
                   (t (if indent2
                          `(Body ,indent1 ,(indent))
                        `(Body ,indent1 0))))))
          (t (goto-char beg)
             (skip-chars-forward " ")
             `(Code)))))

(defun Info--align-menu-entry (beg end align)
  "Align menu entries between BEG and END.
The type of the block should be (MenuEntry ALIGN)."
  (goto-char beg)
  (cl-labels
      ((glyph-width-at
        (p) (aref (aref (font-get-glyphs (font-at p) p (1+ p)) 0) 4)))
    (when (Info--menu-entry-detail-beg end)
      (put-text-property
       (match-beginning 1) (match-end 1)
       'display `(space :align-to
                        (,(* align (glyph-width-at (1- (point))))))))))

(defun Info--remove-indent ()
  "Remove the spaces at the beginning of this line."
  (goto-char (line-beginning-position))
  (skip-chars-forward " ")
  ;; (delete-region (line-beginning-position) (point))
  (put-text-property (line-beginning-position) (point) 'display ""))

(defun Info--remove-line-breaks (beg end)
  "Remove hard line braks between BEG and END.
Moves point."
  (goto-char end)
  (let ((end-mark (point-marker)))
    (goto-char beg)
    (skip-chars-forward " ")
    ;; (delete-region beg (point))
    (put-text-property beg (point) 'display "")
    (while (and (< (point) end-mark)
                (search-forward "\n" end-mark t))
      (let ((p (match-beginning 0)))
        (skip-chars-forward " ")
        ;; (delete-region p (point))
        ;; (insert " ")
        (when (< (point) end-mark)
          (put-text-property p (point) 'display " "))))))

(defun Info--unfontify-quote (beg end)
  "Remove info-body face from quoted text between BEG and END."
  (goto-char beg)
  (while (re-search-forward
          (rx (or (seq "`" (+? anychar) "'")
                  (seq "‘" (+? anychar) "’")
                  (seq (not (any "doesn" "don" "didn" "can"))
                       (group "'" (+? (not (any "\n"))) "'"))))
          end t)
    ;; Only unfontify inline quote.
    (when (plist-get (text-properties-at (point)) 'font-lock-face)
      (put-text-property (or (match-beginning 1) (match-beginning 0))
                         (or (match-end 1) (match-end 0))
                         'font-lock-face 'info-inline-code))))

(defun Info--fontify-block (beg end type)
  "Fontify block between BEG and END of TYPE.
Moves point."
  (goto-char beg)
  (pcase type
    (`(Body ,indent1 ,indent2)
     (put-text-property beg end 'font-lock-face 'info-body)
     (when (not (eq indent1 0))
       (put-text-property beg end 'line-prefix `(space :width ,indent1)))
     ;; We make the whole block indent the same, ignoring indent2.
     (ignore indent2)
     (put-text-property beg end 'wrap-prefix `(space :width ,indent1))
     ;; We want to include the final new line for line-height to take
     ;; effect.
     (put-text-property beg (1+ end) 'line-spacing 0.3)
     ;; This function messes positions up, has to run at the end.
     (Info--remove-line-breaks beg end))

    (`(BulletBody ,indent1 ,indent2)
     (re-search-forward (rx (seq (* " ")
                                 (or "•"
                                     (seq digit ". ")
                                     (seq upper ". ")))))
     ;; We want to keep the bullet in default font.
     (put-text-property (point) end 'font-lock-face 'info-body)
     (when (not (eq indent1 0))
       (put-text-property beg end 'line-prefix `(space :width ,indent1)))
     ;; We add 2 to indent1 to align rest body with the bullet.
     (put-text-property beg end 'wrap-prefix `(space :width ,indent2))
     (put-text-property beg (1+ end) 'line-spacing 0.3)
     (Info--remove-line-breaks beg end))

    (`(MenuHeader))

    (`(MenuEntry ,align)
     (let (pixel-align)
       ;; First, align first line’s detail.
       (when (Info--menu-entry-detail-beg end)
         ;; matched range is the white space between subject and detail.
         (put-text-property
          (match-beginning 1) (match-end 1)
          'display `(space :align-to ,align))
         ;; We skip over the stars. Because info-menu-star is monospaced
         ;; and we want to keep the stars consistent.
         (put-text-property
          (match-end 1) end 'font-lock-face 'info-body)
         ;; Add 1 to end so the newline can get the property.
         (put-text-property beg (1+ end) 'line-spacing 0.3)
         (put-text-property
          (match-end 1) (1+ end) 'wrap-prefix `(space :width ,align))
         (Info--remove-line-breaks (match-end 1) end))))

    (`(DetailList ,indent1 ,indent2)
     (Info--remove-indent)
     (goto-char beg)
     (search-forward "\n")
     (let ((p (point)))
       (put-text-property beg (1+ end) 'line-spacing 0.3)
       (put-text-property p end 'font-lock-face 'info-body)
       (put-text-property p end 'line-prefix `(space :width ,indent2))
       (put-text-property p end 'wrap-prefix `(space :width ,indent2))
       (put-text-property beg (1- p) 'line-prefix
                          `(space :width ,indent1))
       (Info--remove-line-breaks p end)))

    (`(Definition ,indent)
     (re-search-forward "\n +")
     (let ((p (point)))
       (put-text-property beg (1+ end) 'line-spacing 0.3)
       (put-text-property p end 'font-lock-face 'info-body)
       (put-text-property p end 'line-prefix `(space :width ,indent))
       (put-text-property p end 'wrap-prefix `(space :width ,indent))
       (Info--remove-line-breaks p end)))
    (`(Code ,indent) (ignore indent))))

(defun Info--prettify-buffer ()
  "Prettify Info buffer."
  (interactive)
  (when (not (equal "Top" Info-current-node))
    (save-excursion
      (let ((buffer-read-only nil))
        (goto-char (point-min))
        (re-search-forward "[=-\\*]$")
        (let (region)
          (while (setq region (Info--next-block))
            (let ((beg (car region))
                  (end (cdr region))
                  end-mark)
              (setq end-mark (make-marker))
              (set-marker end-mark end)
              (condition-case nil
                  (progn
                    (Info--fontify-block
                     beg end (Info--block-type beg end))
                    (Info--unfontify-quote beg end-mark))
                ((debug search-failed)
                 (message "Failed to fontify block %d %d" beg end)))
              (goto-char end-mark))))
        (Info-fontify-node)
        (visual-line-mode)))))

(define-minor-mode info-pretty-mode
  "Prettified Info."
  :global t
  :lighter ""
  (if info-pretty-mode
      (add-hook 'Info-selection-hook #'Info--prettify-buffer)
    (remove-hook 'Info-selection-hook #'Info--prettify-buffer)))



(provide 'info+)

;;; info+.el ends here

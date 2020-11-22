;;; auto-tab-stop.el --- Automatically set tab stops      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'cl-lib)

(defun glyph-width-at-point (&optional point)
  "Return the pixel width of the glyph at POINT.
The buffer has to be visible."
  (let* ((p (or point (point)))
         (display (plist-get (text-properties-at (1+ p))
                             'display)))
    (if (and (consp display)
             (eq (car display) 'image))
        (car (image-size display 'pixel))
      ;; car + mapcar to translate the vector to a list.
      (aref (car (mapcar
                  #'identity (font-get-glyphs (font-at p) p (1+ p))))
            4))))

(defun glyph-width-from-to (from to)
  "Return the width of the glyphs from FROM (inclusive) to TO (exclusive).
The buffer has to be visible.
FROM has to be less than TO."
  (let ((width 0))
    (save-excursion
      (goto-char from)
      (while (< (point) to)
        (setq width (+ width (glyph-width-at-point)))
        (forward-char)))
    width))

(defun calculate-tab-stop-list (endpos)
  "Calculate and return a list specifying proper tabs stops.
The tabs stops make sure each column in the current paragraph
has enough space.  Start from point and stop at ENDPOS."
  (save-excursion
    (let ((column-width-alist nil)
          (column-index 0)
          (column-start (point))
          (inc 0)
          tab-stop-list
          (pos 0))
      (or (re-search-forward (rx (or "\n" "\t")) nil t)
          (goto-char endpos))
      
      (while (< (point) endpos)
        
        (if (eq (char-before) ?\n)
            ;; We are at a new line, reset column index.
            (setq column-index 0)
          ;; We are at (the end of) a tab, get the x position of the end
          ;; of previous column.
          (let ((oldmax (alist-get column-index column-width-alist))
                (column-width
                 (glyph-width-from-to column-start (point))))
            (if (> column-width (or oldmax 0))
                (setf (alist-get column-index column-width-alist)
                      column-width))))
        ;; Prepare for the next iteration.
        (unless (eq (char-before) ?\n)
          (cl-incf column-index))
        (setq column-start (point))
        (or (re-search-forward (rx (or "\n" "\t")) nil t)
            (goto-char endpos)))
      ;; Now we have a list of pixel column widths, build tab-stop-list.
      (while (alist-get inc column-width-alist)
        ;; Pad 16 pixels after each column
        (setq pos (+ pos (alist-get inc column-width-alist) 16))
        (push pos tab-stop-list)
        (cl-incf inc))
      (nreverse tab-stop-list))))

(defun visual-align-table ()
  "Visually align the table at point."
  (interactive)
  (let ((start-point (point))
        start end)
    (or (re-search-forward "\n\n" nil t)
        (goto-char (point-max)))
    (setq end (point))
    (goto-char start-point)
    (if (re-search-backward "\n\n" nil t)
        (setq start (+ 2 (point)))
      (setq start (point-min)))
    (put-text-property start end 'pixel-tab-stop-list
                       (calculate-tab-stop-list end))))


(provide 'auto-tab-stop)

;;; auto-tab-stop.el ends here

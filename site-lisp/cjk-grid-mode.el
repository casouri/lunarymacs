;;; cjk-grid-mode.el --- Align CJK characters to a grid like in terminals  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; 这个minor mode通过给每一个CJK字符加上min-width text property实现等
;; 高等宽。每个字都加一个单独的text property会有一点性能负担，但是不算
;; 很明显。使用方法：
;;
;;     M-x cjk-grid-mode RET
;;
;; 为了最佳的显示效果和性能，我推荐在汉字占主体的文件里用通过调整字高
;; 实现对齐（因为汉字天然比英文高，汉字高英文低看起来比较美观）；在英
;; 文占主体的文件里用cjk-grid-mode（因为用字高对齐的话，占少数的汉字会
;; 明显高出来，显得很突兀）。而且，在汉字少的文件里开cjk-grid-mode，性
;; 能负担会很小。

;;; Code:

(defsubst cjk-grid-mode--cjk-p (char)
  "Return non-nil if CHAR is a CJK character."
  (let ((entry (aref (category-table) char)))
    ;; Use ‘describe-categories’ for a full list of categories.
    ;; Another way is to use char-script-table, which is not as
    ;; convenient.
    (or (aref entry ?c) ; Chinese
        (aref entry ?h) ; Korean
        (aref entry ?j) ; Japanese
        )))

(defun cjk-grid-mode--fontify (start end)
  "Fontify each CJK character between START and END.
Fontify them with the min-width property so they align to the
ASCII grid."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (and (not (get-text-property (point) 'display))
                 (cjk-grid-mode--cjk-p (char-after)))
        ;; Make the CJK character at least 2 "normal" characters wide.
        ;; We construct new lists with ‘list’ because the affected
        ;; characters are identified by the ‘(2)’ list in the display
        ;; property, compared with ‘eq’.
        (put-text-property (point) (1+ (point))
                           'display (list 'min-width (list 2))))
      (forward-char))))

(defun cjk-grid-mode--unfontify (start end)
  "Unfontify each CJK character between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (equal (get-text-property (point) 'display) '(min-width (2)))
        (remove-text-properties (point) (1+ (point)) '(display)))
      (forward-char))))

(define-minor-mode cjk-grid-mode
  "This minor mode aligns CJK characters to a grid like in terminals.

Turning this mode off in very large buffers might be slow."
  :lighter " CJKg"
  (if cjk-grid-mode
      (progn
        (add-hook 'jit-lock-functions #'cjk-grid-mode--fontify 0 t)
        (unless jit-lock-mode
          (jit-lock-mode 1))
        (jit-lock-refontify))
    ;; Leaving ‘jit-lock-mode’ on is harmless.
    (remove-hook 'jit-lock-functions #'cjk-grid-mode--fontify t)
    (cjk-grid-mode--unfontify (point-min) (point-max))))

(provide 'cjk-grid-mode)

;;; cjk-grid-mode.el ends here

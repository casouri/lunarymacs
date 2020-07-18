;;; commentary.el --- Commentary <-> Org      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;;; Commentary end

;;; Code:
;;

(require 'luna-f)
(require 'org)
(require 'subr-x)

(defun commentary--commented (text)
  "Return TEXT with semicolon before each line."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (insert ";; ")
    (while (eq 0 (forward-line 1))
      (insert ";; "))
    ;; remove the last line “;; ”
    (backward-delete-char 3)
    (buffer-string)))

(defun commentary--replace-match (beg end text)
  "Replace the portion in buffer that begins with BEG and end with END with TEXT."
  (save-excursion
    (goto-char (point-min))
    (let ((beg (progn (re-search-forward beg nil t)
                      (match-beginning 0)))
          (end (progn (re-search-forward end nil t)
                      (match-end 0))))
      (if (and beg end)
          (progn (delete-region beg end)
                 (insert text))
        (error "Could find matched region")))))

(defun commentary-import-readme ()
  "Import commentary from README.org file in the same directory."
  (interactive)
  (let* ((org-source (luna-f-content
                      (luna-f-join default-directory "README.org")))
         ;; export to ascii format
         (ascii-export (with-temp-buffer
                         (let ((buf (current-buffer))
                               (org-export-use-babel nil))
                           (with-temp-buffer
                             (insert org-source)
                             (org-export-to-buffer 'ascii buf
                               nil nil nil t '(:ascii-charset utf-8))))
                         (buffer-string)))
         ;; add semicolon to each line beginning
         ;; (commented-source (commentary--commented org-source))
         (commented-export (commentary--commented ascii-export)))
    (commentary--replace-match "^;;; Commentary:" "^;;;* Commentary end"
                      (format ";;; Commentary:\n;;\n%s\n;;\n;; Commentary end"
                              (string-remove-suffix "\n"
                                                    commented-export)))
    ))

(provide 'commentary)

;;; commentary.el ends here

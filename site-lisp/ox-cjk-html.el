;;; ox-cjk-html.el --- Org CJK HTML export backend      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This backend is a thin wrapper around the default HTML backend. The
;; problem with the default one is that, if you fill your CJK
;; paragraph, the newline are preserved in the HTML output and are
;; displayed as white spaces (between CJK characters) in a browser,
;; which is undesirable. This backend fixes that problem.
;;
;; Usage
;;
;; 	(org-export-to-buffer 'cjk-html BUFFER)



;;; Code:
;;

(require 'ox-html)

(org-export-define-derived-backend 'cjk-html 'html
  :translate-alist '((plain-text . org-cjk-html-plain-text)))

(defvar org-cjk-charset '(chinese-cns11643-15
                          chinese-sisheng
                          big5-hkscs
                          korean-ksc5601
                          japanese-jisx0213\.2004-1
                          japanese-jisx0213-a
                          japanese-jisx0213-2
                          japanese-jisx0213-1
                          japanese-jisx0212
                          japanese-jisx0208-1978
                          japanese-jisx0208
                          chinese-big5-2
                          chinese-big5-1
                          big5
                          chinese-cns11643-7
                          chinese-cns11643-6
                          chinese-cns11643-5
                          chinese-cns11643-4
                          chinese-cns11643-3
                          chinese-cns11643-2
                          chinese-cns11643-1
                          chinese-gbk
                          chinese-gb2312
                          katakana-jisx0201))

(defun org-cjk-p (char)
  "Return t if CHAR is a CJK character."
  (char-charset char org-cjk-charset))

(defun org-cjk-translate-newline (text)
  "Remove newline or replace newline with space in TEXT. 

Remove newline for CJK characters, replace with space for other
characters."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      ;; CJK character.
      (when-let ((char-before (char-before (1- (point))))
                 (char-after (char-after)))
        (cond ((and (org-cjk-p char-before)
                    (org-cjk-p char-after))
               (replace-match ""))
              ;; First newline at consecutive newlines.
              ((eq char-after ?\n) nil)
              ;; Rest newline at consecutive newlines.
              ((eq char-before ?\n)
               (replace-match ""))
              ;; other character
              (t (replace-match " ")))))
    (buffer-string)))

(defun org-cjk-html-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Replace newline with whitespace (English) or simply remove it (CJK).
  (org-html-plain-text (org-cjk-translate-newline text) info))

(provide 'ox-cjk-html)

;;; ox-cjk-html.el ends here

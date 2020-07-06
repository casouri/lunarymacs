;;; org-patch.el --- Org Patches      -*- lexical-binding: t; -*-

;;; Display ~ and = as `

(with-eval-after-load 'org
  (setq org-emphasis-alist
        '(("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim (display "`"))
          ("~" org-code verbatim (display "`"))
          ("+"
           (:strike-through t))))

  (defun org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
			    (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
	  (let* ((marker (match-string 2))
	         (verbatim? (member marker '("~" "="))))
	    (when (save-excursion
		    (goto-char (match-beginning 0))
		    (and
		     ;; Do not match table hlines.
		     (not (and (equal marker "+")
			       (org-match-line
			        "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
		     ;; Do not match headline stars.  Do not consider
		     ;; stars of a headline as closing marker for bold
		     ;; markup either.
		     (not (and (equal marker "*")
			       (save-excursion
			         (forward-char)
			         (skip-chars-backward "*")
			         (looking-at-p org-outline-regexp-bol))))
		     ;; Match full emphasis markup regexp.
		     (looking-at (if verbatim? org-verbatim-re org-emph-re))
		     ;; Do not span over paragraph boundaries.
		     (not (string-match-p org-element-paragraph-separate
					  (match-string 2)))
		     ;; Do not span over cells in table rows.
		     (not (and (save-match-data (org-match-line "[ \t]*|"))
			       (string-match-p "|" (match-string 4))))))
              ;; beg
	      (pcase-let ((`(,_ ,face ,_ ,props) (assoc marker org-emphasis-alist)))
                ;; end
	        (font-lock-prepend-text-property
	         (match-beginning 2) (match-end 2) 'face face)
	        (when verbatim?
		  (org-remove-flyspell-overlays-in
		   (match-beginning 0) (match-end 0))
		  (remove-text-properties (match-beginning 2) (match-end 2)
					  '(display t invisible t intangible t)))
	        (add-text-properties (match-beginning 2) (match-end 2)
				     '(font-lock-multiline t org-emphasis t))
                ;; beg
                (when props
                  (add-text-properties (match-end 4) (match-beginning 5)
				       props)
                  (add-text-properties (match-beginning 3) (match-end 3)
				       props))
                ;; end
	        (when org-hide-emphasis-markers
		  (add-text-properties (match-end 4) (match-beginning 5)
				       '(invisible org-link))
		  (add-text-properties (match-beginning 3) (match-end 3)
				       '(invisible org-link)))
	        (throw :exit t)))))))))

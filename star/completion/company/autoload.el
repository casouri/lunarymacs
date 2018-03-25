;; ;;;###autoload
;; (defun moon/reload-snippet-dir ()
;;   "Reload yasnippet directories (add them into load-path)."
;;   (interactive)
;;   (dolist (dir (directory-files (concat moon-emacs-d-dir "snippet/")))
;;     (add-to-list 'load-path dir)))

;;;###autoload
(defun moon/cancel-completion-and-next-line ()
  "When completing with company, execute this function to cancle completion and go to next line.

Basically Escape + Enter. Because pressing enter when completing will apply the selected candidate."
  (interactive)
  (company-abort)
  (next-line))

;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/kill-python-interpreter ()
  (interactive)
  (when (member "#<buffer *Python*>" (buffer-list))
    (kill-buffer "*Python*"))
  )


;;;###autoload
(defun ein:select-jpnb-notebook ()
  "Chose a notebook to open from a list"
  (interactive)
  (ein:notebooklist-open-notebook-by-file-name
   (completing-read
    "Select notebook: "
    (ein:get-notebook-list) nil t nil))
  t)


;; (plist-get (car (ein:$notebooklist-data (car (ein:notebooklist-list)))) ':path)


;;;###autoload
(defun ein:get-notebook-list ()
  "Get a list of note book paths to display in ivy minibuffer."
  (let ((path-list nil))
    (dolist (notebook
             (ein:$notebooklist-data
              (car (ein:notebooklist-list)))
             path-list)
      (push
       (plist-get notebook ':name)
       path-list))))


;;;###autoload
(defun lsp-format-on-save-mode ()
  "Toggle format-on-save."
  (interactive)
  (lsp-format-on-save-mode moon-format-on-save))

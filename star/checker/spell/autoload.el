;;; -*- lexical-binding: t -*-

;;;###autoload
(defun moon/toggle-spell-check ()
  "Toggle flyspell minor mode. 

If in prog-mode, flyspell-prog-mode will be enabled instead."
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
    (flyspell-mode)))

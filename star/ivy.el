;;; ivy.el --- Ivy config      -*- lexical-binding: t; -*-

(require 'luna-f)

;;; Key
(luna-with-eval-after-load 'key.general
  (luna-default-leader
    "s s" #'swiper))

;;; Package
(load-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (advice-add 'ivy-sort-function-buffer :before #'luna-ivy-sort-match-buffers)
  (advice-add 'ivy-switch-buffer :override #'luna-ivy-switch-buffer))

(load-package swiper
  :commands (swiper))

(load-package counsel
  :config (counsel-mode))

(load-package ivy-prescient
  :after ivy
  :init
  (setq prescient-save-file
        (luna-f-join user-emacs-directory
                     "cache/prescient-save.el"))
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

(load-package recentf-ext)

;;; Sort function

(defvar luna-ivy-sort-buffer-rank-list '("*helpful"
                                         "*Flymake"
                                         "*magit: "
                                         "*Customize")
  "top of the rank list means bottom of the sort.")

(defun luna-ivy-sort-buffer (a b)
  "Sort buffer A and B.
Return t if A should be in front of B."
  (let ((a-score (luna-ivy-sort-score a))
        (b-score (luna-ivy-sort-score b)))
    (> a-score b-score)))

(defun luna-ivy-sort-score (buf)
  "Score a buffer based on its pattern (prefix).
Higher the score, higher in the buffer list (in front)."
  (cl-loop for prefix in luna-ivy-sort-buffer-rank-list
           for score = 0 then (1+ score)
           if (string-prefix-p prefix buf)
           return score
           end
           finally return (1+ score)))

(defun luna-ivy-sort-match-buffers (_ candidate)
  "Sort match candidates (buffer names)."
  (sort candidate #'luna-ivy-sort-buffer))

(defun luna-ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :preselect 1
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

;;; ivy.el ends here

;; -*- lexical-binding: t; -*-

(require 'luna-f)

;;; Note
;;
;; In ivy search session:
;;
;; C-c C-o ivy-occur : port current search to a new buffer like occur
;; M-o : additional actions
;;
;; In global map:
;; M-y : counsel-yank-pop (list kill-ring)


;;; Key

(with-eval-after-load 'luna-general-config
  (luna-default-leader
    "ss" #'swiper)
  (general-define-key
   "M-y" #'counsel-yank-pop))


;;; Package

(load-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ;; use full path for virtual buffer
        ivy-virtual-abbreviate 'full)
  (add-to-list 'ivy-sort-functions-alist
               '(luna-ivy-switch-buffer . luna-ivy-sort-buffer))
  (ivy-mode))

(load-package swiper
  :commands swiper)

(load-package counsel
  :config (counsel-mode))

;; For M-x history.
(add-to-list 'luna-package-list 'smex)

;; ivy-prescient doesn’t support ivy anymore.
;;
;; (load-package ivy-prescient
;;   :after (ivy counsel)
;;   :init
;;   (setq prescient-save-file
;;         (luna-f-join user-emacs-directory
;;                      "cache/prescient-save.el"))
;;   :config
;;   (ivy-prescient-mode)
;;   (prescient-persist-mode))

;; use the forked version in site-lisp
(use-package recentf-ext
  :config (recentf-mode))

(use-package ivy-xref
  :config
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;;; Sort function

(defvar luna-ivy-sort-buffer-rank-list '("*helpful"
                                         "*Flymake"
                                         "*magit: "
                                         "*Customize"
                                         "*EGLOT")
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
            :caller 'luna-ivy-switch-buffer
            :sort t))

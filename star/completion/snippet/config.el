;;; -*- lexical-binding: t -*-

(use-package| yasnippet
  :init
  (setq yas-snippet-dirs (list (concat moon-emacs-d-dir "snippet/")))
  (setq yas-verbosity 0) ; don't message anything
  :config
  (yas-global-mode 1)
  (yas-reload-all))

(defvar moon-enable-company-yas nil
  "Whether to enable yasnippet completion in company.")

(defun company-mode-backend-with-yas (backend)
  (if (or (not moon-enable-company-yas)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(add-hook 'moon-post-init-hook
          (lambda ()
            (setq company-backends
                  (mapcar #'company-mode-backend-with-yas company-backends))) t)



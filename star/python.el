;;; -*- lexical-binding: t -*-

;;; Config

(setq python-indent-offset 4)

;;;; Sagemath

(load-package sage-shell-mode
  :init (add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-shell:sage-mode))
  :commands (sage-shell-mode sage-shell:sage-mode))

(luna-with-eval-after-load 'ui.console-buffer
  (add-to-list 'luna-console-buffer-alist '(sage-shell:sage-mode . "*Sage*"))
  (add-to-list 'luna-console-buffer-alist '(python-mode . "*Python*")))

;;;; IDE

;; (load-package anaconda-mode
;;   :commands (anaconda-mode)
;;   :init (add-hook 'python-mode-hook #'anaconda-mode))

;; (load-package company-anaconda
;;   :after (anaconda-mode)
;;   :init (with-eval-after-load 'company
;;           (add-to-list 'company-backends 'company-anaconda)))

;; (with-eval-after-load 'flycheck
;;   (add-hook 'python-mode-hook #'flycheck-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (unless (string-match "\.sage$" (buffer-file-name))
              (eglot-ensure))))
(add-to-list 'luna-smart-format-alist '(python-mode . eglot-format-buffer))

(add-hook 'python-mode-hook #'company-mode)

;;;; Virtual env

(load-package pyvenv
  :config (setq pyvenv-mode-line-indicator
                '(pyvenv-virtual-env-name
                  (" [py: " pyvenv-virtual-env-name "]")))
  :commands pyvenv-activate)

;;;; Exec path

;; set this to a absolute path then pyvenv won't work
;; because it sets environment, but this variable
(customize-set-value 'python-shell-interpreter "python3")

;;;; Quickrun
;;
;; note this is set after setting exec path

(with-eval-after-load 'quickrun
  (quickrun-add-command "python"
                        '((:command . (lambda () python-shell-interpreter))
                          (:description . "Run python with binary in `python-shell-interpreter'."))
                        :override t))

;;;; Mode line
;;
;; The mode line segment shows current python executable
;; hover text is the full path
;; clicking it opens the customize panel for `python-shell-interpreter'
;;
;; pyvenv displays the virtual env

(defvar luna-python-mode-line-map (let ((map (make-sparse-keymap)))
                                    (define-key map (vector 'mode-line 'down-mouse-1)
                                      (lambda ()
                                        (interactive)
                                        (customize-apropos "python-shell-interpreter")))
                                    map))

(defun luna-python-exec-mode-line ()
  "Return a mode line segment for python executable."
  (propertize (file-name-base python-shell-interpreter)
              'help-echo (executable-find python-shell-interpreter)
              'keymap luna-python-mode-line-map))

(add-to-list 'mode-line-misc-info
             '(:eval (if (eq major-mode 'python-mode)
                         (list "  " (luna-python-exec-mode-line) "  "))
                     ""))

(add-to-list 'mode-line-misc-info
             '(:eval (if (and (eq major-mode 'python-mode) (featurep 'pyvenv))
                         (list "  " pyvenv-mode-line-indicator "  ")
                       "")))

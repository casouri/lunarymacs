;; -*- lexical-binding: t -*-

;;; Config

(setq python-indent-offset 4
      python-shell-completion-native-enable nil)

;;; Packages

;;;; Sagemath

(load-package sage-shell-mode
  :init (add-to-list 'auto-mode-alist
                     '("\\.sage\\'" . sage-shell:sage-mode))
  :commands sage-shell-mode sage-shell:sage-mode)


(load-package eglot
  :extern "pyright npm install -g pyright"
  :defer
  :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '(python-mode . ("pyright-langserver" "--stdio")))
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (when (and buffer-file-name
                         (not (string-match "\\.sage$"
                                            (buffer-file-name))))
                (eglot-ensure)))))

;;;; Virtual env

(load-package pyvenv
  :config (setq pyvenv-mode-line-indicator
                '(pyvenv-virtual-env-name
                  (" [py: " pyvenv-virtual-env-name "]")))
  :commands pyvenv-activate)

;;;; Exec path

;; if you set this to a absolute path, pyvenv won't work
(setq python-shell-interpreter "python3")

;;;; Quickrun
;;
;; note this is set after setting exec path

(with-eval-after-load 'quickrun
  (quickrun-add-command
    "python"
    '((:command . (lambda () python-shell-interpreter))
      (:description . "Run python with the binary in `python-shell-interpreter'."))
    :override t))

;;;; Mode line
;;
;; The mode line segment shows current python executable.
;; The hovering text is the full path.
;; Clicking it opens the customize panel for `python-shell-interpreter'.
;;
;; pyvenv displays the virtual env.

(defvar luna-python-mode-line-map
  (let ((map (make-sparse-keymap)))
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
             '(:eval (if (and (eq major-mode 'python-mode)
                              (featurep 'pyvenv))
                         (list "  " pyvenv-mode-line-indicator "  ")
                       "")))

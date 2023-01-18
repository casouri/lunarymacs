;; -*- lexical-binding: t -*-

;;; Config

(setq python-indent-offset 4
      python-shell-completion-native-enable nil)

;;; Packages

;; (load-package sage-shell-mode
;;   :init (add-to-list 'auto-mode-alist
;;                      '("\\.sage\\'" . sage-shell:sage-mode))
;;   :commands sage-shell-mode sage-shell:sage-mode)

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (when (and buffer-file-name
;;                        (not (string-match "\\.sage$"
;;                                           (buffer-file-name))))
;;               (eglot-ensure))))

(load-package eglot
  :defer
  :config (push '(python-base-mode . ("pyright-langserver" "--stdio"))
                eglot-server-programs)
  :extern "pyright")

(luna-note-extern "pyright"
  "Python language server, install by
npm install --global pyright")

;;;; Virtual env

(load-package pyvenv
  :config (setq pyvenv-mode-line-indicator
                '(pyvenv-virtual-env-name
                  (" [py: " pyvenv-virtual-env-name "]")))
  :commands pyvenv-activate)

;;;; Exec path

;; if you set this to a absolute path, pyvenv won't work
(setq python-shell-interpreter "python3")

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

;;;; Mod

(with-eval-after-load 'python
  (defun python-indent-dedent-line-backspace (arg)
    "De-indent current line or delete.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation. If region is active,
delete not indent."
    (interactive "*p")
    (if (region-active-p)
        (backward-delete-char-untabify arg)
      (when (null (python-indent-dedent-line))
        (backward-delete-char-untabify arg)))))

(defun setup-python ()
  nil)

(add-hook 'python-base-mode-hook #'setup-python)

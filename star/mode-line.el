;; -*- lexical-binding: t; -*-

(load-package minions
  :config
  (add-hook #'after-init-hook #'minions-mode 90))

(defun restart-minions-mode ()
  (interactive)
  (minions-mode -1)
  (minions-mode))

(defun luna-edit-lighter ()
  (if (buffer-modified-p)
      "| "
    "  "))

(defun luna-root-lighter ()
  (if (equal user-login-name "root")
      "ROOT "
    ""))

(defun make-lighter (str empty-value empty-return)
  "Make a ligher for mode-line.

If STR equal to EMPTY-VALUE(nil, \"\"), return EMPTY-RETURN,
else return STR."
  (if (equal str empty-value)
      empty-return
    str))

(defvar luna-flymake-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (vector 'mode-line mouse-wheel-up-event) #'flymake-goto-prev-error)
    (define-key map
      (vector 'mode-line mouse-wheel-down-event) #'flymake-goto-next-error)
    map))

(defun luna-dedicated-window-mode-line ()
  (if (window-dedicated-p)
      "LOCK"
    ""))

(defun luna-flymake-mode-line ()
  (require 'subr-x)
  (let* ((known (hash-table-keys flymake--backend-state))
         (running (flymake-running-backends))
         (disabled (flymake-disabled-backends))
         (reported (flymake-reporting-backends))
         (diags-by-type (make-hash-table))
         (all-disabled (and disabled (null running)))
         (some-waiting (cl-set-difference running reported)))
    (maphash (lambda (_b state)
               (mapc (lambda (diag)
                       (push diag
                             (gethash (flymake--diag-type diag)
                                      diags-by-type)))
                     (flymake--backend-state-diags state)))
             flymake--backend-state)
    (apply #'concat
           (mapcar
            (lambda (args)
              (apply
               (lambda (num str face pad)
                 (concat
                  (propertize
                   (format str num)
                   'face face
                   'keymap luna-flymake-mode-line-map
                   'help-echo (format "%d running backens\nScroll up/down: previous/next diagnose"
                                      (length running)))
                  (propertize pad 'face '(:foreground "gray"))))
               args))
            `((,(length (gethash :error diags-by-type)) "%d " error "|")
              (,(length (gethash :warning diags-by-type)) " %d " warning "|")
              (,(length (gethash :note diags-by-type)) " %d" success ""))))))

(setq-default mode-line-format
              (let ((spaces "   "))
                `((:eval (luna-root-lighter))
                  (:eval (luna-dedicated-window-mode-line))
                  ;; (:eval (luna-edit-lighter))
                  vc-mode
                  ,spaces
                  ;; (:eval (if (bound-and-true-p eyebrowse-mode)
                  ;;            (eyebrowse-mode-line-indicator) ""))
                  "%b"
                  ,spaces
                  mode-line-modes
                  ,spaces
                  (:eval (if (bound-and-true-p flymake-mode)
                             (luna-flymake-mode-line) "OK"))
                  ,spaces
                  mode-line-misc-info
                  ,spaces
                  "%I"
                  ,spaces
                  (:eval (if (bound-and-true-p nyan-lite-mode)
                             (nyan-lite-mode-line) "ฅ Φ ω Φ ฅ"))
                  ,spaces
                  "%p"
                  ;; ,spaces
                  ;; (:eval (symbol-name buffer-file-coding-system))
                  ;; "  %l:%c"
                  mode-line-end-spaces)))

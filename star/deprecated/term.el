;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Keys

(with-eval-after-load 'luna-general-config
  (general-define-key "s-e" #'vterm-toggle))

;;; Package

;; Add this to your zshrc
;;
;; if [ "$INSIDE_EMACS" = "vterm" ]
;;    then setopt PROMPT_SUBST
;;         PROMPT=$PROMPT'$(vterm_prompt_end)'
;; fi

(use-package vterm
  :load-path "~/attic/emacs-libvterm"
  :config
  (setq vterm-shell "zsh")
  (add-hook 'vterm-set-title-functions 'vterm--rename-buffer-as-title))

;;; Functions

(defun vterm--rename-buffer-as-title (title)
  (rename-buffer (format "vterm @ %s" title) t))

(defvar vterm-toggle-prev-buffer nil
  "Used by ‘vterm-toggle’.")

(defun vterm-buffer-list ()
  "Return a list of vterm buffers."
  (cl-remove-if-not (lambda (buf)
                      (provided-mode-derived-p
                       (buffer-local-value 'major-mode buf)
                       'vterm-mode))
                    (buffer-list)))

(defun vterm-cd (dir)
  "cd to DIR."
  (when (derived-mode-p 'vterm-mode)
    (vterm-send-key "u" nil nil t)
    (execute-kbd-macro (format "cd %s" dir))))

(defun vterm-toggle (&optional arg)
  "Toggle vterm.

ARG: C-u: open the vterm buffer with the same dir of current buffer
If there exists an Vterm buffer with current directory, use that,
otherwise create one.

C-u C-u: same as C-u, but reuse a existing vterm buffer instead of
creating one."
  (interactive "p")
  (if (derived-mode-p 'vterm-mode)
      ;; toggle off
      (if vterm-toggle-prev-buffer
          (switch-to-buffer vterm-toggle-prev-buffer)
        (user-error "No previous buffer to jump back to"))
    ;; toggle on
    (let ((buffer-list (vterm-buffer-list)))
      (cond
       ((or (eq arg 4) ; C-u
            (eq arg 16)) ; C-u C-u
        ;; open in current dir
        (let* ((dir default-directory)
               (buffer-with-same-dir
                (catch 'found
                  (dolist (buffer buffer-list nil)
                    (when (equal dir (buffer-local-value
                                      'default-directory
                                      buffer))
                      (throw 'found buffer))))))
          ;; found the buffer with the same dir
          (setq vterm-toggle-prev-buffer
                (current-buffer))
          (if buffer-with-same-dir
              (switch-to-buffer buffer-with-same-dir)
            ;; or not and maybe create a new one
            (switch-to-buffer
             (if (eq arg 16)
                 (progn (message "No valid vterm buffer found, reuse one.")
                        (car buffer-list))
               (message "No valid vterm buffer found, create a new one.")
               (vterm)))
            (vterm-cd dir))))
       ;; simply open
       (t (setq vterm-toggle-prev-buffer (current-buffer))
          (switch-to-buffer (or (car buffer-list)
                                (vterm))))))))

;;; term.el ends here

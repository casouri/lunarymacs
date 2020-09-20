;;;; notmuch
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/notmuch")
(setq notmuch-init-file (luna-f-join user-emacs-directory "star/notmuch-config.el"))
(setq message-auto-save-directory "~/mail/draft")
(setq message-kill-buffer-on-exit t)
(setq notmuch-search-oldest-first nil)
(require 'notmuch)

;;;; nyan
(nyan-lite-mode)
(setq nyan-wavy-trail t)
enabling this makes highlight on buttons blink
(nyan-start-animation)

;;;; Font
;; WenYue GuDianMingChaoTi (Non-Commercial Use) W5
;; WenYue XHGuYaSong (Non-Commercial Use)
;; WenyueType GutiFangsong (Non-Commercial Use)
;; SiaoyiWangMingBold
;; FZQingKeBenYueSongS-R-GB
;; FZSongKeBenXiuKaiS-R-GB


;;;; flymake lighter
(defvar luna-flymake-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (vector 'mode-line mouse-wheel-up-event) #'flymake-goto-prev-error)
    (define-key map
      (vector 'mode-line mouse-wheel-down-event) #'flymake-goto-next-error)
    map))

(defun luna-flymake-mode-line ()
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
                   'help-echo (format "%d running backens
Scroll up/down: previous/next diagnose"
                                      (length running)))
                  (propertize pad 'face '(:foreground "gray"))))
               args))
            `((,(length (gethash :error diags-by-type))
               "%d " error "|")
              (,(length (gethash :warning diags-by-type))
               " %d " warning "|")
              (,(length (gethash :note diags-by-type))
               " %d" success ""))))))
;;;; Mode-line
;; vc-mode
;; (:eval (if (bound-and-true-p eyebrowse-mode)
;;            (eyebrowse-mode-line-indicator) ""))
;; ,spaces
;; (:eval (if (bound-and-true-p flymake-mode)
;;            (luna-flymake-mode-line) "OK"))
;; ,spaces
;; "%I"
;; ,spaces
;; "%l:%c"
;; ,spaces
;; (:eval (symbol-name buffer-file-coding-system))

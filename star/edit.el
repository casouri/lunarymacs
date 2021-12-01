;; -*- lexical-binding: t -*-
;;
;; Text editing, tools and everything.

(require 'utility)

;;; Key

(luna-def-key
 "C-x u"   #'vundo
 "C-h C-h" #'ghelp-describe
 "C-h r"   #'ghelp-resume
 "C-h o"   #'ghelp-describe-elisp
 "C-h f"   #'ghelp-describe-function
 "C-h v"   #'ghelp-describe-variable
 "C-h k"   #'ghelp-describe-key
 "C-x i"   #'luna-insert-special-symbol
 "C-c '"   #'separedit
 "C-/"     #'undo-only
 "C-."     #'undo-redo
 "C-="     #'er/expand-region
 "C--"     #'er/contract-region
 "M-q"     #'ftable-fill
 "C-c C-s" #'consult-line

 :leader
 "ha" #'hideshow-toggle-all
 "hb" #'hs-toggle-hiding
 "rr" #'vr/replace

 :---
 ;; :keymaps 'override
 ;; "C-j"     #'avy-goto-word-1
 :keymaps '(outline-minor-mode-map org-mode-map outline-mode-map)
 "s-i"     #'outline-cycle
 "C-s-i"   #'outline-cycle-buffer
 "M-]"     '("outline-next-heading"
             . (lambda () (interactive)
                 (outline-next-heading)
                 (recenter-top-bottom)))
 "M-["     '("outline-previous-heading"
             . (lambda () (interactive)
                 (outline-previous-heading)
                 (recenter-top-bottom)))
 :keymaps 'rime-active-mode-map
 "C-\\"    (lambda () (interactive)
             (rime--return)
             (toggle-input-method))
 ;; "<tab>" #'rime-inline-ascii
 :keymaps 'rime-mode-map
 "C-`" #'rime-send-keybinding
 :keymaps 'project-prefix-map
 "g" #'luna-project-find-regexp

 :keymaps 'consult-binded-mode-map
 "C-x C-b" #'consult-buffer
 "M-y"     #'consult-yank-pop
 :leader
 "ss"      #'consult-line
 "si"      #'consult-imenu)

;;; Package

;;;; Movement

;; (load-package avy
;;   :commands avy-goto-word-1)

(load-package expand-region
  :config
  ;; It interferes angel.el's region transient map. Specifically, the
  ;; region-deactive-hook doesn't run right after the region highlight
  ;; is off.
  (setq expand-region-fast-keys-enabled nil)
  (push #'er/tree-sitter-expand er/try-expand-list)
  :commands
  er/expand-region
  er/contract-region
  :init
  (defun tree-sitter-select-node (node)
    (let ((beg (tree-sitter-node-beginning node))
          (end (tree-sitter-node-end node)))
      (push-mark end)
      (activate-mark)
      (goto-char beg)))

  (defun er/tree-sitter-expand ()
    (interactive)
    (when (and (featurep 'tree-sitter)
               (car tree-sitter-parser-list))
      (if (region-active-p)
          (when-let ((node (tree-sitter-node-in-range
                            (region-beginning) (region-end))))
            (tree-sitter-select-node
             (or (tree-sitter-node-parent node) root)))
        (when-let ((node (tree-sitter-node-in-range
                          (point) (1+ (point)))))
          (tree-sitter-select-node node))))))

;;;; Structure

(load-package color-outline
  :hook (prog-mode-hook . color-outline-mode))

(load-package hideshow
  :hook (prog-mode-hook . hs-minor-mode)
  :init
  (defvar-local hideshow-hidden nil
    "Non-nil if the buffer has hiding on.")
  (defun hideshow-toggle-all ()
    "Toggle hideshow status for the current buffer."
    (interactive)
    (if hideshow-hidden
        (hs-show-all)
      (hs-hide-all))
    (setq hideshow-hidden (not hideshow-hidden))))

(with-eval-after-load 'project
  (setq project-vc-ignores '(".ccls-cache/"))
  (defun luna-project-find-regexp (regexp pattern)
    (interactive (list (project--read-regexp)
                       (read-from-minibuffer
                        "File Pattern: "
                        (concat "*." (or (file-name-extension
                                          (or (buffer-file-name)
                                              ""))
                                         "")))))
    (require 'xref)
    (require 'grep)
    (let* ((pr (project-current t))
           (default-directory (project-root pr))
           (files (project--files-in-directory
                   default-directory
                   (project-ignores pr default-directory)
                   pattern)))
      (xref--show-xrefs
       (apply-partially #'project--find-regexp-in-files regexp files)
       nil))))

;;;; Search & replace

(load-package visual-regexp
  :commands
  vr/replace
  vr/query-replace
  vr/mc-mark)

;;;; Automation

(load-package ftable
  :commands
  ftable-fill
  ftable-edit-cell
  ftable-reformat)

(load-package ws-butler
  ;; global mode interferes with magit
  :hook (prog-mode . ws-butler-mode))

(load-package separedit
  :commands separedit
  :config
  (setq separedit-default-mode 'text-mode
        separedit-remove-trailing-spaces-in-comment t))

;;;; Undo & history

(load-package vundo
  :commands vundo)

;; (load-package undohist
;;   :config
;;   (undohist-initialize)
;;   (push "COMMIT_EDITMSG" undohist-ignored-files))

;;;; Application

(load-package helpful)

(load-package ghelp
  :commands
  ghelp-describe
  ghelp-describe-function
  ghelp-describe-variable)

(load-package inspector
  :commands inspect-expression
  :config
  (setq inspector-end-column 70)
  (defun inspector-quit ()
    "Quit the Emacs inspector."
    (interactive)
    (setq inspector-history nil)
    (if (window-prev-buffers)
        (quit-window)
      (delete-window))))



(load-package tramp
  :defer
  :init
  (setq tramp-default-method "ssh")
  ;; `tramp-backup-directory-alist' is for remote backup on remote
  ;; machines, so it is not useful. The following is the real way:
  ;; REF: https://stackoverflow.com/questions/3893727/setting-emacs-tramp-to-store-local-backups
  (let ((backup-dir (expand-file-name
                     "var/tramp/backup" user-emacs-directory)))
    (unless (file-exists-p backup-dir)
      (mkdir backup-dir t))
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp backup-dir)))
  :config
  ;; Make sure TRAMP works with guix.
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path
                  "~/.guix-profile/bin" "~/.guix-profile/sbin"
                  "/run/current-system/profile/bin" 
                  "/run/current-system/profile/sbin"))))

(load-package rime
  :defer
  :config
  (luna-on "Brown"
    (setq rime-show-candidate 'posframe
          rime-posframe-style 'vertical
          rime-user-data-dir "/Users/yuan/Library/Rime"
          rime-librime-root "/opt/local"
          rime-show-preedit 'inline
          rime-emacs-module-header-root "~/emacs-head/src"
          rime-posframe-properties (list :font "Source Han Sans"
                                         :weight 'light
                                         :internal-border-width 10))
    (add-hook 'input-method-activate-hook
              (lambda () (interactive)
                (setq-local cursor-type 'hbar)))
    (add-hook 'input-method-inactivate-hook
              (lambda () (interactive)
                (kill-local-variable 'cursor-type)))))

;; -*- lexical-binding: t -*-
;;
;; Text editing, tools and everything.

(require 'outline+)

;;; Key

(luna-def-key
 "C-x u" #'vundo
 :leader
 "ha" #'hideshow-toggle-all
 "hb" #'hs-toggle-hiding
 "rr" #'vr/replace

 :---
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

 :keymaps 'override
 "C-j"     #'avy-goto-word-1
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

(load-package avy
  :commands avy-goto-word-1)


(load-package ws-butler
  ;; global mode interferes with magit
  :hook (prog-mode . ws-butler-mode))


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


(load-package diff-hl
  :hook (prog-mode-hook . diff-hl-mode)
  :config (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(load-package separedit
  :commands separedit
  :config
  (setq separedit-default-mode 'text-mode
        separedit-remove-trailing-spaces-in-comment t))


(load-package yasnippet
  :config
  (yas-global-mode)
  (yas--define-parents 'minibuffer-mode '(emacs-lisp-mode))
  (yas--define-parents 'minibuffer-inactive-mode '(emacs-lisp-mode))
  (with-eval-after-load 'hippie-exp
    (add-to-list 'hippie-expand-try-functions-list #'yas-expand)))


(load-package color-outline
  :hook (prog-mode-hook . color-outline-mode))


(load-package visual-regexp
  :commands
  vr/replace
  vr/query-replace
  vr/mc-mark)


(load-package ghelp
  :commands
  ghelp-describe
  ghelp-describe-function
  ghelp-describe-variable)


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


(load-package helpful)


(load-package iscroll
  :commands iscroll-mode)


(load-package vundo
  :commands vundo
  :config (push '(?└ . ?╰) vundo-translation-alist))


(load-package tramp
  :defer
  :init
  (setq tramp-default-method "ssh")
  ;; `tramp-backup-directory-alist' is for remote backup on remote
  ;; machines, so it is not useful. The following is the real way:
  ;; https://stackoverflow.com/questions/3893727/setting-emacs-tramp-to-store-local-backups
  (let ((backup-dir (expand-file-name
                     "var/tramp/backup" user-emacs-directory)))
    (unless (file-exists-p backup-dir)
      (mkdir backup-dir t))
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp backup-dir))))


(load-package ftable
  :commands
  ftable-fill
  ftable-edit-cell
  ftable-reformat)


(load-package company
  :hook (prog-mode-hook . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-tooltip-limit 15)
  ;; Company dabbrev is annoying, make sure not to include it.
  (setq-default company-backends
                '(company-capf company-files company-dabbrev-code))
  (setq-default company-search-filtering t)

  :init
  (defun company-complete-common-or-commit ()
    "Insert the common part of all candidates, or commit the selection."
    (interactive)
    (when (company-manual-begin)
      (let ((tick (buffer-chars-modified-tick)))
        (call-interactively 'company-complete-common)
        (when (eq tick (buffer-chars-modified-tick))
          (call-interactively 'company-complete-selection)))))

  (luna-def-key
   :keymaps '(company-active-map company-search-map)
   "C-p" #'company-select-previous
   "C-n" #'company-select-next
   "RET" nil
   "<return>" nil
   "="   #'company-complete-selection
   :keymaps 'company-search-map
   "<escape>" #'company-abort))


(load-package rime
  :defer
  :config
  (luna-on "Brown"
    (setq rime-show-candidate 'posframe
          rime-posframe-style 'vertical
          rime-user-data-dir "/Users/yuan/Library/Rime"
          rime-show-preedit 'inline
          rime-posframe-properties (list :font "Source Han Sans"
                                         :weight 'light
                                         :internal-border-width 10))
    (add-hook 'input-method-activate-hook
              (lambda () (interactive)
                (setq-local cursor-type 'hbar)))
    (add-hook 'input-method-inactivate-hook
              (lambda () (interactive)
                (kill-local-variable 'cursor-type)))))


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


(load-package consult
  :config
  (consult-binded-mode)
  (setq consult-preview-key (kbd "C-o"))
  :init
  ;; Bindings are defined at the top.
  (defvar consult-binded-mode-map (make-sparse-keymap)
    "Minor mode map for ‘console-minor-mode’.")

  (define-minor-mode consult-binded-mode
    "Minor mode enabling consult commands."
    :global t
    :keymap consult-binded-mode-map
    :group 'convenience
    (if consult-binded-mode
        (message ":-)")
      (message ";-)"))))


(load-package recentf-ext
  :config (recentf-mode))


(load-package vertico
  :config (vertico-mode))

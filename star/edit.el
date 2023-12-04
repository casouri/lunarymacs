;; -*- lexical-binding: t -*-
;;
;; Text editing, tools and everything.

;;; Key

(luna-key-def
 "C-x u"   #'vundo
 "C-x i"   #'luna-insert-special-symbol
 "C-c '"   #'separedit
 "C-/"     #'undo-only
 "C-."     #'undo-redo
 "C-="     #'expreg-expand
 "C--"     #'expreg-contract
 "M-q"     #'ftable-fill
 "C-x C-d" #'luna-open-directory

 :leader
 "ha" #'hideshow-toggle-all
 "hb" #'hs-toggle-hiding
 "rr" #'vr/replace
 "dg" #'deadgrep
 "ff" #'treesit-fold-toggle

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
 "si"      #'consult-imenu
 "ss"      #'consult-line)

;;; Package

;;;; Movement

;; (load-package avy
;;   :commands avy-goto-word-1)

(load-package expreg
  :commands expreg-expand expreg-contract)

;;;; Structure

(load-package color-outline
  :autoload-hook (prog-mode-hook . color-outline-mode))

(with-eval-after-load 'project
  (add-to-list 'project-vc-ignores ".ccls-cache/")
  (add-to-list 'project-vc-ignores "node_modules")
  (defvar project-language-aware-root-files
    '("tsconfig.json"
      "package.json"
      "Cargo.toml"
      "compile_commands.json"
      "project.clj"
      "compile_flags.txt"))
  (defun project-try-language-aware (dir)
    "Find a super-directory of DIR containing a root file."
    (let ((dir (cl-loop for pattern in project-language-aware-root-files
                        for result = (locate-dominating-file dir pattern)
                        if result return result)))
      (and dir (cons 'language-aware dir))))
  (cl-defmethod project-root ((project (head language-aware)))
    (cdr project))
  ;; Add it after vc. VC handles ignored files the much better. If
  ;; there are a ton of small files and they are not properly ignored,
  ;; project-files stucks and eglot start up freezes.
  (add-hook 'project-find-functions
	        #'project-try-language-aware 50))

(defun luna-project-find-regexp (regexp pattern)
  (interactive (list (project--read-regexp)
                     (read-from-minibuffer
                      "File Pattern: "
                      (concat "*." (or (file-name-extension
                                        (or (buffer-file-name)
                                            ""))
                                       "")))))
  (require 'grep)
  (let* ((proj (project-current t))
         (root (project-root proj))
         (grep-find-ignored-files
          (append (mapcar (lambda (pattern)
                            (concat "*" pattern))
                          project-vc-ignores)
                  grep-find-ignored-files)))
    (rgrep regexp pattern root)))

(load-package consult
  :config
  (autoload #'switch-to-buffer-in-tab "switch-to-buffer-in-tab.el")
  (setq consult-preview-key nil
        consult--buffer-display #'switch-to-buffer-in-tab)
  (consult-customize
   consult-line :preview-key 'any)

  (defvar consult-binded-mode-map (make-sparse-keymap))
  (define-minor-mode consult-binded-mode
    "Enabled consult bindings."
    :lighter ""
    :global t
    :keymap consult-binded-mode-map)
  (consult-binded-mode)

  (defun consult-imenu--set-imenu-alist (&rest _)
    "Populate ‘imenu--index-alist’ for other packages to use."
    (setq imenu--index-alist consult-imenu--cache))
  (advice-add #'consult-imenu :after #'consult-imenu--set-imenu-alist))


(load-package treesit-fold
  :commands treesit-fold-toggle)

;;;; Search & replace

(load-package visual-regexp
  :commands
  vr/replace
  vr/query-replace
  vr/mc-mark)

(load-package wgrep :defer)

(load-package deadgrep :commands deadgrep)

;;;; Automation

(load-package ftable
  :commands
  ftable-fill
  ftable-edit-cell
  ftable-reformat)

(load-package ws-butler
  ;; Global mode interferes with magit.
  :autoload-hook
  (prog-mode-hook . ws-butler-mode)
  (text-mode-hook . ws-butler-mode))

(load-package separedit
  :commands separedit
  :config
  (setq separedit-default-mode 'text-mode
        separedit-remove-trailing-spaces-in-comment t))

(load-package after-save
  :autoload-hook (prog-mode-hook . after-save-mode))

(add-hook 'prog-mode-hook #'electric-quote-local-mode)
(add-hook 'text-mode-hook #'electric-quote-local-mode)

;;;; Undo & history

(load-package vundo
  :commands vundo
  :config
  (setf (alist-get 'selected-node vundo-glyph-alist) ?X
        (alist-get 'node vundo-glyph-alist) ?O)
  (set-face-font 'vundo-default "PragmataPro Mono"))

;; (load-package undo-hl
;;   :autoload-hook (prog-mode-hook . undo-hl-mode)
;;   :config (setq undo-hl-mininum-edit-size 10)
;;   (add-to-list 'undo-hl-undo-commands 'vundo-forward)
;;   (add-to-list 'undo-hl-undo-commands 'vundo-backward))

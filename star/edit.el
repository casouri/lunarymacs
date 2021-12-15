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

(add-hook 'prog-mode-hook #'electric-quote-local-mode)
(add-hook 'text-mode-hook #'electric-quote-local-mode)

;;;; Undo & history

(load-package vundo
  :commands vundo)

;; (load-package undohist
;;   :config
;;   (undohist-initialize)
;;   (push "COMMIT_EDITMSG" undohist-ignored-files))




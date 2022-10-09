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
 "C-="     #'er/expand-region
 "C--"     #'er/contract-region
 "M-q"     #'ftable-fill

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
  :autoload-hook (prog-mode-hook . color-outline-mode))

(load-package hideshow
  :autoload-hook (prog-mode-hook . safe-hs-minor-mode)
  :config
  (defun safe-hs-minor-mode ()
    "Enable ‘hs-minor-mode’ but don’t signal error."
    (when (and comment-start comment-end)
      (hs-minor-mode)))
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
  (add-to-list 'project-vc-ignores ".ccls-cache/"))

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

;;;; Search & replace

(load-package visual-regexp
  :commands
  vr/replace
  vr/query-replace
  vr/mc-mark)

;; C-g doesn’t seem to work right.
;; (load-package ctrlf
;;   :config
;;   (ctrlf-mode)
;;   (setq ctrlf-auto-recenter t
;;         ctrlf-default-search-style 'fuzzy
;;         ctrlf-show-match-count-at-eol nil
;;         ctrlf-highlight-current-line nil)
;;   (set-face-attribute 'ctrlf-highlight-active nil
;;                       :inherit 'highlight)
;;   (set-face-attribute 'ctrlf-highlight-passive nil
;;                       :inherit 'default :weight 'bold))

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
  (set-face-attribute 'vundo-default nil :family "PragmataPro Mono"))

(load-package undo-hl
  :autoload-hook (prog-mode-hook . undo-hl-mode)
  :config (setq undo-hl-mininum-edit-size 10)
  (add-to-list 'undo-hl-undo-commands 'vundo-forward)
  (add-to-list 'undo-hl-undo-commands 'vundo-backward))

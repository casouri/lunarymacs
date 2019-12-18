;;; -*- lexical-binding: t -*-

(require 'luna-f)

;;; Key

(with-eval-after-load 'luna-general-config
  (luna-default-leader
    ;; Outshine
    "tl" #'luna-toggle-left-margin
    "iu" #'insert-char
    "sr" #'color-rg-search-input
    "C-o" #'outline-toggle-children
    ;; minimap
    "tm" #'minimap-mode
    ;; avy
    "k" #'avy-goto-char-timer)

  (general-define-key
   :keymaps 'override
   ;; this is binded by default,
   ;; but flyspell mode shadows it
   "C-M-i" #'outshine-cycle-buffer)

  (general-define-key
   ;; Hungry delete
   [remap backward-delete-char-untabify] #'luna-hungry-delete
   ;; helpful
   "C-h C-h" #'ghelp-describe-symbol
   "C-h C-r" #'ghelp-resume
   "C-h k" #'helpful-key
   "C-h e" #'ghelp-describe-as-in-emacs-lisp-mode
   "C-h r" #'ghelp-resume-as-in-emacs-lisp-mode)

  (general-define-key
   :prefix "C-x"
   ;; C-y is too uncomfortable to reach
   ;; so C-p here we go
   "C-p" #'luna-kill-ring-select
   "i" #'luna-insert-special-symbol))

;;; Package

;;;; Edit

(load-package ws-butler
  :defer 3
  :config (ws-butler-global-mode))

(load-package expand-region
  :config
  ;; it interferes angel.el's region transient map
  ;; specifically, the region-deactive-hook
  ;; doesn't run right after the region highlight is off
  (setq expand-region-fast-keys-enabled nil)
  :commands
  er/expand-region
  er/mark-defun
  er/mark-word
  er/mark-symbol
  er/mark-inside-quotes
  er/mark-outside-quotes
  er/mark-inside-pairs
  er/mark-outside-pairs
  er/contract-region)

(load-package isolate
  :commands (isolate-quick-add
             isolate-quick-change
             isolate-quick-delete
             isolate-long-add
             isolate-long-change
             isolate-long-delete))

(load-package undo-tree
  :config (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(luna-f-join luna-cache-dir "undo-tree")))))

;;;; Code Structure

(load-package outshine
  :commands outshine-cycle-buffer
  :init
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (defvar outline-minor-mode-prefix (kbd "C-c o"))
  :config
  (define-key outshine-mode-map
    (kbd "C-c i") #'outshine-cycle))


(load-package color-rg
  :init
  (define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg)
  :commands (isearch-toggle-color-rg
             color-rg-search-input
             color-rg-search-symbol
             color-rg-search-project
             color-rg-search-project-rails))



(load-package visual-regexp
  :commands (vr/replace
             vr/query-replace
             vr/mc-mark))


;;;; Help

(add-to-list 'luna-package-list 'helpful)

;;; smart-delete

(defvar luna-hungry-delete-black-list '(org-mode text-mode fundamental-mode markdown-mode)
  "A list of major mode in where `luna-hungry-delete' should behave like normal delete.")

(defun luna-hungry-delete ()
  "Smart and clean delete.
If we are at the beginning of a line, backspace
deletes all whitespace before and after point
and moves point to the previous line."
  (interactive)
  (require 'cl-lib)
  (cl-labels ((normal-delete () (if (region-active-p)
                                    (delete-region (region-beginning) (region-end))
                                  (if (and (not (eql (point) (point-max)))
                                           (eql (alist-get (char-before) '((?{ . ?}) (?\[ . ?\])
                                                                           (?\( . ?\)) (?\" . ?\")
                                                                           (?\' . ?\') (?“ . ?”) (?‘ . ?’)))
                                                (char-after)))
                                      ;; if we are in the middle of a empty pair, i.e., "|" or (|)
                                      ;; delete both
                                      (progn (forward-char)
                                             (backward-delete-char 2))
                                    (call-interactively #'backward-delete-char-untabify)))))
    (if (or (region-active-p)
            (<= (car (syntax-ppss)) 0)
            (minibufferp (current-buffer)))
        ;; if we are at top-level
        ;; do normal delete
        (normal-delete)
      ;; if the point is not before the line but inside it, do normal delete
      ;; otherwise do hungry delete
      ;;
      ;; 1. we first delete all white spaces, then insert newline and indent properly
      ;; 2. but if there is only a closing delimiter, i.e., } or ),
      ;;    we don't insert new line.
      ;; 3. if we ends up in the same place before hungry delete,
      ;;    that means the user is trying to delete back to the previous line,
      ;;    then do that.
      (let* ((point (point)) ; staring point
             (bolt (save-excursion
                     ;; `beginning-of-line-text' seems to ignore comment for some reason,
                     (beginning-of-line)
                     (skip-chars-forward " \t")
                     (point)))
             ;; beginning of the region that we are to delete
             (beg (save-excursion (while (member (char-before) '(?\n ?\s ?\t))
                                    (backward-char))
                                  (point)))
             ;; end of that region
             (end (save-excursion (goto-char bolt)
                                  (while (member (char-after) '(?\n ?\s ?\t))
                                    (forward-char))
                                  (point))))
        (if (<= point bolt)
            ;; actually decide to delete stuff
            (progn
              (delete-region beg end)
              (unless (eql (char-after) ?\))
                (call-interactively #'newline))
              ;; so we did all this and ends up not changing anything
              ;; why? because the user doesn't want to delete excess white space and add newline
              ;; but to delete back to previous line! do that.
              (when (eql (point) end)
                (delete-region beg end)
                (unless (eql (char-before) ?\()
                  (insert ?\s))))
          ;; not at beginning of text, just do normal delete
          (normal-delete))))))

;;; Other functions

(defun luna-insert-current-date ()
  "insert date for blog composing"
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


(defun luna-insert-semi-at-eol ()
  "Insert semicolon at end of line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))


(defun luna-jump-newline-below ()
  "create new line above/below without breaking current line"
  (interactive)
  (end-of-line)
  (newline-and-indent))


(defun luna-jump-newline-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line))


(defun luna-scroll-down-reserve-point ()
  (interactive)
  (scroll-up 2)
  (next-line 2))


(defun luna-scroll-up-reserve-point ()
  (interactive)
  (scroll-down 2)
  (previous-line 2))


(defun luna-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(defvar luna-autoinsert-template (luna-f-join (luna-this-file-directory)
                                              "autoinsert-template.el")
  "The template file.")

(defun luna-autoinsert (description)
  "Autoinsert what auto-insert inserts."
  (interactive "MDescription: ")
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (year (format-time-string "%Y"))
         (feature (file-name-base (buffer-file-name)))
         (buf (find-file luna-autoinsert-template))
         (template (with-current-buffer buf
                     (buffer-string))))
    (kill-buffer buf)
    (insert (format template
                    filename description feature filename))))

(defvar luna-special-symbol-alist '(("(c)" . "©")
                                    ("tm" . "™")
                                    ("p" . " ")
                                    ("s" . "§")
                                    ("---" . "—") ; em dash
                                    ("--" . "–") ; en dash
                                    ("..." . "…")
                                    ("<" . "⃖")
                                    (">" . "⃗")
                                    ("^" . "ꜛ")
                                    ("v" . "ꜜ")
                                    ("<<" . "←")
                                    (">>" . "→")
                                    ("^^" . "↑")
                                    ("vv" . "↓")
                                    ("l" . "‘")
                                    ("r" . "’")
                                    ("ll" . "“")
                                    ("rr" . "”")
                                    (" " . " ") ; non-breaking space
                                    ("hand" . "☞"))
  "Alist used by `luna-insert-special-symbol'.")

(defun luna-insert-special-symbol (surname)
  "Insert special symbol at point, SURNAME is used to search for symbol.
E.g. SURNAME (c) to symbol ©."
  (interactive "MAbbrev: ")
  (insert (catch 'ret
            (dolist (elt luna-special-symbol-alist)
              (when (equal (car elt) surname)
                (throw 'ret (cdr elt)))
              ""))))

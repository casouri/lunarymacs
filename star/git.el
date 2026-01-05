;; -*- lexical-binding: t -*-

(luna-key-def
 :leader
 "g"  '("git")
 "gs" #'magit-status
 "gw" #'pick-magit-status
 "gb" #'magit-recent-branches
 "gf" '("file")
 "gfc" #'magit-file-checkout
 "gfl" #'magit-log-buffer-file
 :---
 :keymaps 'magit-mode-map
 "<tab>" #'magit-section-toggle)

(load-package magit
  :commands magit-status
  :hook (git-commit-setup-hook . setup-magit-commit-buffer)
  ;; Speed up log buffer: disable --graph, set -n to 128, C-x C-s to
  ;; save preference.
  :config
  ;; Speed up magit in large repos.
  (setq magit-refresh-status-buffer nil
        ;; Don’t show refined hunk for performance, but we can use D t
        ;; in the diff buffer to toggle it.
        magit-diff-refine-hunk nil)
  ;; The tags header is slow when there are 10k+ headers (1+ sec).
  (remove-hook 'magit-status-headers-hook #'magit-insert-tags-header))

(defun setup-magit-commit-buffer ()
  "Setup magit commit message buffer."
  (setq fill-column 64)
  (electric-quote-local-mode -1))

(load-package git-link
  :commands
  git-link
  git-link-commit
  git-link-homepage
  github-link
  github-link-commit
  github-link-homepage
  :init
  (defalias 'github-link 'git-link)
  (defalias 'github-link-commit 'git-link-commit)
  (defalias 'github-link-homepage 'git-link-hoempage))

(load-package difftastic
  :extern "difftastic"
  :after magit
  :config
  (setq difftastic-requested-window-width-function
        #'frame-width))

(luna-note-extern "difftastic"
  "brew install difftastic")

;; (load-package magit-delta
;;   :hook (magit-diff-mode-hook . magit-delta-mode))

(luna-note-extern "delta" "port install delta")

(defun pick-magit-status ()
  "Pick a currently open magit status buffer."
  (interactive)
  (require 'seq)
  (let* ((buf-list
          (mapcar #'buffer-name
                  (seq-filter (lambda (buffer)
                                (eq (buffer-local-value 'major-mode buffer)
                                    'magit-status-mode))
                              (buffer-list))))
         (choice (completing-read "Pick: " buf-list nil t)))
    (switch-to-buffer choice)))

;;; Recent branches

(defvar magit-recent-branches-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'magit-recent-branches-checkout)
    (define-key map (kbd "g") #'magit-recent-branches-refresh)
    map))

(define-derived-mode magit-recent-branches-mode tabulated-list-mode "Recent Branches"
  "Major mode for displaying recent git branches."
  (setq tabulated-list-format [("Branch" 30 t)
                               ("Time" 18 t)
                               ("Subject" 0 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun magit-recent-branches-refresh ()
  "Refresh the recent branches list."
  (interactive)
  (let ((output (shell-command-to-string
                 "git for-each-ref --sort=-committerdate refs/heads/ --format='%(refname:short)|%(committerdate:relative)|%(subject)'")))
    (setq tabulated-list-entries
          (mapcar (lambda (line)
                    (let ((parts (split-string line "|")))
                      (list (nth 0 parts)
                            (vector (propertize (or (nth 0 parts) "") 'face 'magit-branch-local)
                                    (propertize (or (nth 1 parts) "") 'face 'shadow)
                                    (or (nth 2 parts) "")))))
                  (split-string output "\n" t))))
  (tabulated-list-print t))

(defun magit-recent-branches ()
  "Display recently updated branches in a popup buffer."
  (interactive)
  (let ((root (magit-toplevel)))
    (pop-to-buffer (get-buffer-create "*Recent Branches*"))
    (setq default-directory root)
    (magit-recent-branches-mode)
    (magit-recent-branches-refresh)))

(defun magit-recent-branches-checkout ()
  "Checkout branch on current line."
  (interactive)
  (let ((branch (tabulated-list-get-id)))
    (quit-window)
    (magit-checkout branch)))

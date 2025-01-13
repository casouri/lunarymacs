;; -*- lexical-binding: t -*-

(luna-key-def
 :leader
 "g"  '("git")
 "gs" #'magit-status
 "gw" #'pick-magit-status
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

(load-package magit-delta
  :hook (magit-mode-hook . magit-delta-mode))

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

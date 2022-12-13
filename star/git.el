;; -*- lexical-binding: t -*-

(luna-key-def
 :leader
 "g"  '("git")
 "gs" #'magit-status
 "gf" '("file")
 "gfc" #'magit-file-checkout
 "gfl" #'magit-log-buffer-file
 :---
 :keymaps 'magit-mode-map
 "<tab>" #'magit-section-toggle)

(load-package magit
  :commands magit-status
  :config
  ;; Speed up magit in large repos.
  (setq magit-refresh-status-buffer nil
        ;; Don’t show refined hunk for performance, but we can use D t
        ;; in the diff buffer to toggle it.
        magit-diff-refine-hunk nil)
  ;; The tags header is slow when there are 10k+ headers (1+ sec).
  (remove-hook 'magit-status-headers-hook #'magit-insert-tags-header)
  ;; Speed up log buffer: disable --graph, set -n to 128, C-x C-s to
  ;; save preference.
  ;;
  ;; Profile with ‘magit-toggle-verbose-refresh’.

  (defun electric-quote-local-mode-advice (fn &rest args)
    "Disable electric-quote-mode in commit message buffer."
    (unless (equal (buffer-name) "COMMIT_EDITMSG")
      (apply fn args)))
  (advice-add 'electric-quote-local-mode :around
              #'electric-quote-local-mode-advice)
  ;; Patch
  (defun magit-patch-apply-buffer (buffer &rest args)
    "Apply the patch buffer BUFFER."
    (interactive
     (list (read-buffer "Apply patch in buffer: " nil t)
           (transient-args 'magit-patch-apply)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer)
      (magit-run-git-with-input "apply" args "-")
      (magit-refresh)))
  (require 'magit-patch)
  (transient-append-suffix 'magit-patch-apply "a"
    '("b" "Apply patch in buffer" magit-patch-apply-buffer)))

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

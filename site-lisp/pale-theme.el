;;; pale-theme.el --- Light theme      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; A quieter theme with ink-on-paper feel. It uses more bold, italic,
;; and gray for contrast in place of different colors.

;;; Code:

(require 'theme-util)

(deftheme pale
  "Pale theme.")

(theme-util-deffaces
 'magit-heading-selection
 'magit-heading-highlight)

;; (FACE INHERIT FOREGROUND BACKGROUND UNDERLINE WEIGHT SLANT REST-ATTR)
(theme-util-set-faces 'pale
  (cl-flet ((darken #'theme-util-darken)
            (brighten #'theme-util-brighten)
            (overlay #'theme-util-color-overlay)
            (box-pad (lambda (width color)
                       `( :line-width (,width . ,width)
                          :color ,color))))
    (let* ((bg "#ffffff")
           (bg-tooltip "gray95")
           (bg-block "gray92")
           (bg-region "gray90")

           (fg  "#1c1c1c")
           (fg-weak "#9a9ea2")

           (hl-weak bg-tooltip)
           (hl-normal "#F5F573")
           (hl-strong "#e8e800")

           ;; Colors indented for foreground are less pronounced.
           (hl-fg "#8C6E14")
           (red-fg "DarkRed")
           (blue-fg "#004f9b")
           (green-fg "#007345")

           ;; Warning and error are brighter to catch attention.x
           (warning "#DA7A48")
           (err     "#E04E49")
           (ok      "#489446")

           ;; Note that this is not a cons cell.
           (tty       '((type nil)))
           ;; Pre-defined templates.
           (bold '(nil nil nil nil semi-bold))
           (underline '(nil nil nil t))
           (italic '(nil nil nil nil nil italic))
           (light '(nil nil nil nil semi-light)))
;;; Builtin faces
      `((default     (nil ,fg ,bg))
        (region      (nil nil ,bg-region))
        (highlight   (nil nil ,hl-strong))
        (cursor      (nil "white" "black"))
        (match       (nil ,ok nil nil bold))
        (error       (nil ,err))
        (warning     (nil ,warning))
        (success     (nil ,ok))
        (tooltip     (nil nil ,bg-tooltip))
        (fringe      ())
        (shadow      (nil ,fg-weak))
        (vertical-border (nil ,bg-tooltip ,bg-tooltip) nil ,tty)
        (link        (nil nil ,hl-weak))
        (link-visited (link))
        (bold ,bold)
        (underline ,underline)
        (italic ,italic)

        (vertical-border     (nil nil "black"))
        (window-divider      (nil "gray85"))
        (minibuffer-prompt   ,bold)
        (isearch             (nil nil ,hl-strong))
        (lazy-highlight      (nil nil ,hl-strong))
        (isearch-fail        (error))
        (show-paren-match    ,bold)
        (trailing-whitespace (nil nil ,err))

        (widget-field        (nil nil ,bg-block))
        (widget-inactive     (nil ,fg-weak))

        ;; See also builtin-config.el (Customize) where I increase line
        ;; spacing and change font.
        (custom-button
         (nil nil ,bg-tooltip)
         (:box (:line-width 3 :color ,bg-tooltip)))
        (custom-button-mouse
         (custom-button nil ,hl-strong)
         (:box (:line-width 3 :color ,hl-strong)))
        (custom-button-pressed
         (custom-button nil ,bg-region)
         (:box (:line-width 3 :color ,bg-region)))

        (custom-changed        ,bold)
        (custom-comment-tag    (font-lock-comment-face))
        (custom-documentation  ())
        (custom-variable-tag   ,italic)
        (custom-face-tag       (custom-variable-tag))
        (custom-group-subtitle (custom-variable-tag))
        (custom-group-tag      (nil))
        (custom-invalid        (nil ,err))
        (custom-modified       (nil ,warning))
        (custom-rogue          (nil ,warning))
        (custom-set            (nil ,ok))
        (custom-state          ())
        (custom-themed         (nil ,ok))

        (font-lock-builtin-face              ,italic)
        (font-lock-comment-face              (nil ,fg-weak))
        (font-lock-comment-delimiter-face    (font-lock-comment-face))
        (font-lock-doc-face                  (shadow))
        (font-lock-constant-face             ,italic)
        (font-lock-keyword-face              (nil ,red-fg))
        (font-lock-string-face               ())
        ;; (font-lock-type-face                 (nil ,hl-fg))
        (font-lock-type-face                 (nil ,blue-fg))
        ;; (font-lock-type-face                 (nil ,green-fg))
        (font-lock-variable-name-face        ,bold)
        (font-lock-function-name-face        ,bold)
        (font-lock-warning-face              (error))
        (font-lock-negation-char-face        (warning))
        (font-lock-preprocessor-face         ())
        (font-lock-preprocessor-char-face    ())
        (font-lock-regexp-grouping-backslash ,bold)
        (font-lock-regexp-grouping-construct ,bold)
        (elisp-shorthand-font-lock-face      ,bold)

        (sh-quoted-exec (nil ,red-fg))

        (mode-line (nil nil ,bg-tooltip))
        (mode-line-inactive (mode-line nil ,bg-block))
        (header-line (mode-line) (:height 150))

        (tab-bar (mode-line nil ,bg) (:height 140))
        (tab-bar-tab          (tab-bar nil ,bg)
                              (:box (:line-width 1 :color ,blue-fg)))
        (tab-bar-tab-inactive (tab-bar ,fg-weak ,bg)
                              (:box (:line-width 1 :color "gray90")))
        (tab-bar-tab-highlight (tab-bar nil ,bg)
                               (:box (:line-width 2 :color ,blue-fg)))
        (tab-bar-tab-group-current  (tab-bar-tab nil nil nil bold))
        (tab-bar-tab-group-inactive (tab-bar-tab-inactive))
        (tab-bar-tab-ungrouped      (tab-bar-tab-inactive))

        (completions-common-part (shadow))
        (completions-first-difference (bold))

;;; Packages

        (flyspell-duplicate () (:underline
                                (:style wave :color ,hl-fg)))
        (flyspell-incorrect () (:underline
                                (:style wave :color ,hl-fg)))

        (company-tooltip                     (tooltip))
        (company-tooltip-annotation          (company-tooltip))
        (company-tooltip-annotation-selection (company-tooltip-selection))
        (company-tooltip-common ((company-tooltip bold)))
        (company-tooltip-common-selection ((company-selection bold)))
        (company-tooltip-mouse ((company-tooltip underline)))
        (company-tooltip-selection (company-tooltip nil ,hl-strong))
        (company-scrollbar-bg                (company-tooltip))
        (company-scrollbar-fg                (company-tooltip nil ,bg-region))
        (company-preview                     (shadow))
        (company-preview-common              (company-preview))
        (company-preview-search              (company-preview))

        (corfu-default (tooltip))
        (corfu-current (completions-highlight))
        (corfu-border  (nil nil ,fg-weak))

        (orderless-match-face-0 (shadow))
        (orderless-match-face-1 (orderless-match-face-0))
        (orderless-match-face-2 (orderless-match-face-0))
        (orderless-match-face-3 (orderless-match-face-0))

        (magit-heading-highlight (nil nil ,bg-tooltip) (:extend t))
        (magit-heading-selection (nil nil ,bg-block) (:extend t))

        (magit-section-heading           ,bold)
        (magit-section-highlight         (magit-heading-highlight))
        (magit-section-heading-selection (magit-heading-selection))
        (magit-section-secondary-heading
         (magit-section-heading nil nil nil light))

        (magit-diff-file-heading           (nil ,blue-fg nil nil semi-bold))
        (magit-diff-file-heading-highlight (magit-heading-highlight))
        (magit-diff-file-heading-selection (magit-heading-selection))

        (magit-diff-hunk-heading           (nil nil "#bde4fc")
                                           (:box ,(box-pad 3 "#bde4fc")))
        (magit-diff-hunk-heading-highlight (magit-diff-hunk-heading))
        (magit-diff-hunk-heading-selection (magit-diff-hunk-heading))
        ;; selected hunk region
        (magit-diff-hunk-region            (region))
        ;; this also determines the hunk region boundary
        (magit-diff-lines-heading          (nil ,bg ,err))
        (magit-diff-revision-summary       ,bold)
        ;; Don’t inherit ‘magit-diff-hunk-heading’.
        (magit-diff-conflict-heading       (nil))

        (magit-bisect-bad        (error))
        (magit-bisect-good       (success))
        (magit-bisect-skip       (warning))
        (magit-blame-date        (nil ,hl-fg))
        (magit-blame-heading     (magit-heading-highlight))
        (magit-branch-current    (nil ,red-fg))
        (magit-branch-local      ,bold)
        (magit-branch-remote     ,bold)
        (magit-cherry-equivalent (success))
        (magit-cherry-unmatched  (error))
        (magit-tag               (nil ,hl-fg))
        (magit-filename          ())

        (magit-diff-added            (nil ,ok ,(overlay bg ok 0.1)))
        (magit-diff-added-highlight  (nil ,ok ,(overlay bg ok 0.2)))
        (diff-refine-added           (nil ,ok "#99ff99"))

        (magit-diff-removed          (nil ,err ,(overlay bg err 0.1)))
        (magit-diff-removed-highlight (nil ,err ,(overlay bg err 0.2)))
        (diff-refine-removed         (nil ,err "#ffaaaa"))

        (magit-diff-base           (nil ,warning ,(overlay bg warning 0.1)))
        (magit-diff-base-highlight (nil ,warning ,(overlay bg warning 0.2)))

        (magit-diff-context           (default))
        (magit-diff-context-highlight (nil ,fg ,bg-block))

        (magit-diffstat-added            (nil ,ok))
        (magit-diffstat-removed          (nil ,err))
        (magit-dimmed                    (shadow))
        (magit-hash                      (nil ,hl-fg))
        (magit-header-line               ,bold)
        (magit-log-author                (default))
        (magit-log-date                  (default))
        (magit-log-graph                 (default))
        (magit-process-ng                (error))
        (magit-process-ok                (success))
        ;; (magit-reflog-amend              (nil ,violet2))
        ;; (magit-reflog-checkout           (nil ,blue1))
        ;; (magit-reflog-cherry-pick        (nil ,green))
        ;; (magit-reflog-commit             (nil ,green))
        ;; (magit-reflog-merge              (nil ,green))
        ;; (magit-reflog-other              (nil ,blue1))
        ;; (magit-reflog-rebase             (nil ,violet2))
        ;; (magit-reflog-remote             (nil ,blue1))
        ;; (magit-reflog-reset              (error))
        ;; (magit-refname                   (nil ,fg-weak))
        ;; (magit-sequence-drop             (nil ,red))
        ;; (magit-sequence-head             (nil ,blue1))
        ;; (magit-sequence-part             (nil ,orange))
        ;; (magit-sequence-stop             (nil ,green))
        ;; (magit-signature-bad             (error))
        ;; (magit-signature-error           (error))
        ;; (magit-signature-expired         (nil ,orange))
        ;; (magit-signature-good            (success))
        ;; (magit-signature-revoked         (nil ,orange))
        ;; (magit-signature-untrusted       (nil ,orange))

        (rainbow-delimiters-unmatched-face (nil ,err))
        (rainbow-delimiters-mismatched-face
         (rainbow-delimiters-unmatched-face))

        (smerge-lower   (magit-diff-added))
        (smerge-upper   (magit-diff-removed))
        (smerge-base    (magit-diff-base))
        (smerge-markers (nil nil nil nil bold))

        (which-key-key-face                   (nil nil ,hl-strong))
        (which-key-group-description-face     ())
        (which-key-command-description-face   ())
        (which-key-local-map-description-face ())

        (hl-paren-face (nil "red" nil nil bold))

        (info-title-1 (info-title-2) (:height 1.3))
        (info-title-2 (info-title-4) (:height 1.2))
        (info-title-3 (info-title-4))
        (info-title-4 (variable-pitch))
        (outline-1 (info-title-1))
        (outline-2 (info-title-2))
        (outline-3 (info-title-3))
        (outline-4 (info-title-4))
        (outline-5 (outline-4))
        (outline-6 (outline-4))
        (outline-7 (outline-4))
        (outline-8 (outlint-4))

        (org-level-1 (info-title-1))
        (org-level-2 (info-title-2))
        (org-level-3 (info-title-3))
        (org-level-4 (info-title-4))
        (org-document-title (info-title-1))
        (org-meta-line (shadow nil nil nil nil italic))
        (org-document-info (org-meta-line))
        (org-document-info-keyword (org-meta-line))

        (org-verbatim         (fixed-pitch))
        (org-code             (org-verbatim))
        (org-block            (org-verbatim nil ,bg-block) (:extend t))
        (org-block-begin-line ((org-block org-meta-line)))
        (org-block-end-line   ((org-block org-meta-line)))
        (org-formula          (fixed-pitch))
        (org-quote            (org-block))

        (org-table             (nil))
        (org-todo              (highlight))
        (org-time-grid         (nil ,hl-fg))
        (org-upcoming-deadline (nil ,err))

        (helpful-heading (info-title-3))

        (ghelp-entry-title (info-title-2))

        (diff-hl-change (nil ,warning ,(overlay bg warning 0.1)))

        (line-number              ((fixed-pitch)))
        (line-number-current-line (nil nil ,bg-block))
        (line-number-major-tick   (line-number))
        (line-number-minor-tick   (line-number))

        (rime-default-face (tooltip) (:height 150))
        (rime-highlight-candidate-face ((bold rime-default-face)))
        (rime-code-face (rime-default-face))

        (selectrum-prescient-primary-highlight (nil nil ,hl-normal))
        (selectrum-prescient-secondary-highlight (nil nil ,hl-normal))

        (consult-bookmark (consult-buffer))
        (consult-file (consult-file))

        (erc-notice-face (nil ,red-fg)) ; Get rid of bold.
        (erc-timestamp-face (nil ,ok)) ; Use darker green.
        (erc-current-nic-face (nil ,red-fg)) ; Use darker blue.

        (dictionary-word-definition-face (variable-pitch))
        (dictionary-word-entry-face (variable-pitch nil nil nil bold))
        (dictionary-reference-face ((variable-pitch link)))

        (gnus-summary-normal-unread (variable-pitch))
        (gnus-summary-normal-read
         (gnus-summary-normal-unread ,ok))
        (gnus-summary-normal-undownloaded
         (gnus-summary-normal-unread ,red-fg))
        (gnus-summary-normal-ticked
         (gnus-summary-normal-unread ,hl-fg))
        (gnus-summary-normal-ancient
         (gnus-summary-normal-unread ,blue-fg))

        (debbugs-gnu-new (nil ,err))
        (debbugs-gnu-handled (nil ,ok))
        (debbugs-gnu-tagged (nil ,err))
        ))))

(provide-theme 'pale)

;;; pale-theme.el ends here

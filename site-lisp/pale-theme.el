;;; pale-theme.el --- Light theme      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'theme-util)

(deftheme pale
  "Pale theme.")

(theme-util-deffaces
 'block
 'magit-heading-selection
 'magit-heading-highlight)

;; (FACE INHERIT FOREGROUND BACKGROUND UNDERLINE WEIGHT SLANT REST-ATTR)
(theme-util-set-faces 'pale
  (cl-flet ((darken #'theme-util-darken)
            (brighten #'theme-util-brighten)
            (overlay #'theme-util-color-overlay))
    (let* ((bg "#ffffff")
           (bg-tooltip "gray92")
           (bg-block "gray90")
           (bg-region "gray85")

           (fg  "#111111")
           (fg-weak "#9a9ea2")

           (hl-weak bg-tooltip)
           (hl-normal "#ffffb4")
           (hl-strong "#e8e800")
           (hl-fg "#987816")

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
      `(;; builtin faces
        (default     (nil ,fg ,bg))
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
        (block       (nil nil ,bg-block))
        (bold ,bold)
        (underline ,underline)
        (italic ,italic)

        (vertical-border     (nil nil "black"))
        (minibuffer-prompt   ,bold)
        (isearch             (nil nil ,hl-strong))
        (lazy-highlight      (nil nil ,hl-strong))
        (isearch-fail        (error))
        (show-paren-match    ,bold)
        (trailing-whitespace (nil nil ,err))

        (widget-field        (nil nil ,bg-block))
        (widget-inactive     (nil ,fg-weak))



        ;; See also builin-config.el (Customize) where I increase line
        ;; spacing and default face.
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
        (custom-group-tag      ((nil info-title-3)))
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
        (font-lock-keyword-face              (nil "darkred"))
        (font-lock-string-face               ())
        (font-lock-type-face                 (nil "#987816"))
        (font-lock-variable-name-face        ,bold)
        (font-lock-function-name-face        ,bold)
        (font-lock-warning-face              (error))
        (font-lock-negation-char-face        (warning))
        (font-lock-preprocessor-face         ())
        (font-lock-preprocessor-char-face    ())
        (font-lock-regexp-grouping-backslash ,bold)
        (font-lock-regexp-grouping-construct ,bold)

        (mode-line (nil nil ,bg-tooltip))
        (mode-line-inactive (mode-line nil ,bg-block))
        (header-line (mode-line) (:height 150))

        ;; completion
        (completions-common-part ,bold)

        ;; package faces

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

        ;; (ivy-current-match                   (current-selection))
        ;; (ivy-minibuffer-match-face-1         (nil ,bg ,green))
        ;; (ivy-minibuffer-match-face-2         (nil ,bg ,orange))
        ;; (ivy-minibuffer-match-face-3         (nil ,bg ,orange))
        ;; (ivy-minibuffer-match-face-4         (nil ,bg ,orange))
        ;; (ivy-minibuffer-match-highlight      (ivy-current-match))
        ;; (ivy-virtual                         (default))
        ;; (ivy-subdir                          (default))
        ;; (ivy-remote                          (default))
        ;; (ivy-org                             (default))

        (magit-heading-highlight (nil nil ,bg-tooltip) (:extend t))
        (magit-heading-selection (nil nil ,bg-block) (:extend t))

        (magit-section-heading           ,bold)
        (magit-section-highlight         (magit-heading-highlight))
        (magit-section-heading-selection (magit-heading-selection))
        (magit-section-secondary-heading
         (magit-section-heading nil nil nil light))

        (magit-diff-file-heading           ,bold)
        (magit-diff-file-heading-highlight (magit-heading-highlight))
        (magit-diff-file-heading-selection (magit-heading-selection))

        (magit-diff-hunk-heading           ,italic)
        (magit-diff-hunk-heading-highlight (magit-heading-highlight))
        (magit-diff-hunk-heading-selection (magit-heading-selection))
        ;; selected hunk region
        (magit-diff-hunk-region            (region))
        ;; this also determines the hunk region boundary
        (magit-diff-lines-heading          (nil ,bg ,err))
        (magit-diff-revision-summary       ,bold)

        (magit-bisect-bad        (error))
        (magit-bisect-good       (success))
        (magit-bisect-skip       (warning))
        (magit-blame-date        (nil ,hl-fg))
        (magit-blame-heading     (magit-heading-highlight))
        (magit-branch-current    (warning))
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


        ;; (rainbow-delimiters-depth-1-face (nil ,blue2))
        ;; (rainbow-delimiters-depth-2-face (nil ,violet2))
        ;; (rainbow-delimiters-depth-3-face (nil ,green))
        ;; (rainbow-delimiters-depth-4-face (nil ,orange))
        ;; (rainbow-delimiters-depth-5-face (nil ,violet2))
        ;; (rainbow-delimiters-depth-6-face (nil ,yellow))
        ;; (rainbow-delimiters-depth-7-face (nil ,blue2))
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
        (org-block            ((org-verbatim block)) (:extend t))
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

        ;; (avy-lead-face      (nil ,bg ,red))
        ;; (avy-lead-face-0    (nil ,bg ,green))
        ;; (avy-lead-face-1    (nil ,bg ,orange))
        ;; (avy-lead-face-2    (nil ,bg ,blue2))

        (table-cell ())

        ;; (tab-line              (mode-line-inactive))
        ;; (tab-line-tab          (tab-line))
        ;; (tab-line-tab-inactive (tab-line-tab))
        ;; (tab-line-highlight
        ;;  (tab-line nil ,(darken bg 0.15))
        ;;  (:box (:line-width 3 :color ,(darken bg 0.15))))
        ;; (tab-line-tab-current
        ;;  (tab-line nil ,bg) (:box (:line-width 3 :color ,bg)))

        (rime-default-face (tooltip) (:height 150))
        (rime-highlight-candidate-face ((bold rime-default-face)))
        (rime-code-face (rime-default-face))

        (selectrum-prescient-primary-highlight (nil nil ,hl-normal))
        (selectrum-prescient-secondary-highlight (nil nil ,hl-normal))

        (consult-bookmark (consult-buffer))
        (consult-file (consult-file))

        (erc-notice-face (nil "SlateBlue")) ; Get rid of bold.
        (erc-timestamp-face (nil ,ok)) ; Use darker green.
        (erc-current-nic-face (nil "SlateBlue")) ; Use darker blue.
        ))))

(provide-theme 'pale)

;;; pale-theme.el ends here

;;; cyberpunk-theme.el --- Cyberpunk theme      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'theme-util)

(deftheme cyberpunk
  "Cuberpunk theme.")

(theme-util-deffaces
 'block
 'custom-default
 'highlight-fg-only-2
 'highlight-fg-only-1
 'red-bg-hl
 'red-bg
 'yellow-bg-hl
 'yellow-bg
 'green-bg-hl
 'green-bg
 'comp-scroll-bar
 'comp-mouse
 'selection-common
 'comp-common
 'current-selection
 'magit-heading-selection
 'magit-heading-highlight)

(theme-util-set-faces 'cyberpunk
  (cl-flet ((darken #'theme-util-darken)
            (brighten #'theme-util-brighten)
            (overlay #'theme-util-color-overlay))
    (let* ((bg        "#282a36")
           ;; lighter than region
           (bg-alt    (brighten bg 0.1))
           (fg        "#C5DEED")
           (fg-weak   "#778CB8")
           ;; (fg-strong "#ABC4E5")
           (blue1     "#79BEEF")
           (blue2     "#46EAFF")
           (green     "#52DEA1")
           (orange    "#FF8743")
           (red       "#FF414A")
           (yellow    "#FFF100")
           (violet1   "#F975D4")
           (violet2   "#582EBF")
           ;; Note that this is not a cons cell.
           (tty       '((type nil))))
      `(;; builtin faces
        (default     (nil ,fg ,bg))
        (region      (nil nil ,(overlay bg blue1 0.1)))
        (highlight   (nil ,bg ,blue2))
        (cursor      (nil ,bg ,blue2))
        (link        (nil ,blue2 nil nil))
        (match       (nil ,green nil nil bold))
        (error       (nil ,red))
        (warning     (nil ,yellow))
        (success     (nil ,green))
        (tooltip     (nil ,fg ,bg-alt))
        (fringe      (default))
        (shadow      (nil ,fg-weak))
        (vertical-border (nil ,bg-alt ,bg-alt) nil ,tty)
        (link-visited    (link ,violet1))
        (block       (nil nil ,bg-alt))
        
        (variable-pitch () (:family "Charter" :height 155))
        (fixed-pitch    () (:family "SF Mono" :height 130))
        
        (vertical-border      (nil nil "black"))
        (lazy-highlight       (nil "white" nil nil bold))
        (highlight-fg-only-1  (nil ,blue2))
        (highlight-fg-only-2  (nil ,violet1))
        (minibuffer-prompt    (highlight-fg-only-1))
        (secondary-selection  (nil nil ,(overlay bg blue1 0.3)))
        (custom-face-tag      (nil ,blue2))
        (isearch              (bold))
        (isearch-fail         (error))
        (show-paren-match     (bold))
        (trailing-whitespace  (nil nil ,red))

        (widget-field        (nil nil ,bg-alt))
        (widget-inactive     (widget-field ,fg-weak))
        
        ;; see also builin-config.el (Customize) where I increase line
        ;; spacing and default face.
        (custom-default        () (:family "SF Pro Text" :height 140))
        (custom-button
         (custom-default ,(brighten fg 0.2) ,bg-alt)
         (:box (:line-width 3 :color ,bg-alt)))
        (custom-button-mouse
         (custom-button nil ,(darken bg-alt 0.15))
         (:box (:line-width 3 :color ,(darken bg-alt 0.15))))
        (custom-button-pressed
         (custom-button "white" ,(darken bg-alt 0.15))
         (:box (:line-width 3 :color ,(darken bg-alt 0.15))))

        (custom-button-unraised (link))
        (custom-button-pressed-unraised (link ,violet1))
        (custom-changed        (custom-default ,orange))
        (custom-comment-tag    ((custom-default font-lock-comment-face)))
        (custom-documentation  (custom-default))
        (custom-face-tag       (custom-default ,blue2))
        (custom-group-subtitle (custom-default))
        (custom-group-tag      ((custom-default info-title-3)))
        (custom-group-tag-1    (custom-group-tag))
        (custom-invalid        (custom-default ,red))
        (custom-modified       (custom-default ,orange))
        (custom-rogue          (custom-default ,orange))
        (custom-set            (custom-default ,green))
        (custom-state          (custom-default ,green))
        (custom-themed         (custom-default ,blue2))
        (custom-variable-button   (custom-default))
        (custom-variable-obsolete (custom-default))
        (custom-variable-tag      (custom-default))

        (font-lock-builtin-face              (nil ,blue2))
        (font-lock-comment-face              (nil ,fg-weak))
        (font-lock-comment-delimiter-face    (font-lock-comment-face))
        (font-lock-doc-face                  (font-lock-comment-face))
        (font-lock-constant-face             (nil ,yellow))
        (font-lock-function-name-face        (nil ,violet1))
        (font-lock-keyword-face              (nil ,blue2))
        (font-lock-string-face               (nil ,green))
        (font-lock-type-face                 (nil ,yellow))
        (font-lock-variable-name-face        (nil ,violet1))
        (font-lock-warning-face              (warning))
        (font-lock-negation-char-face        (nil ,blue2))
        (font-lock-preprocessor-face         (nil ,blue2))
        (font-lock-preprocessor-char-face    (nil ,blue2))
        (font-lock-regexp-grouping-backslash (nil ,blue2))
        (font-lock-regexp-grouping-construct (nil ,blue2))

        (mode-line (nil nil ,violet2))
        (mode-line-inactive
         (mode-line nil ,(overlay bg violet2 0.7) nil nil nil))
        (header-line (mode-line-inactive))

        ;; completion
        (current-selection  (nil ,bg ,blue2))
        (comp-common        (nil ,violet1))
        (selection-common   (current-selection ,bg))
        (comp-mouse         (nil ,bg ,violet1))
        
        ;; package faces

        (company-tooltip                  (tooltip))
        (company-tooltip-annotation       (company-tooltip))
        (company-tooltip-annotation-selection (company-tooltip-selection))
        (company-tooltip-common           ((comp-common company-tooltip)))
        (company-tooltip-common-selection
         ((selection-common company-tooltip)))
        (company-tooltip-mouse            ((comp-mouse company-tooltip)))
        (company-tooltip-selection
         ((current-selection company-tooltip)))
        (company-scrollbar-bg             (company-tooltip))
        (company-scrollbar-fg             (company-tooltip nil ,blue2))
        (company-preview                  (highlight-fg-only-1))
        (company-preview-common           (company-preview))
        (company-preview-search           (company-preview))
        
        (ivy-current-match                   (current-selection))
        (ivy-minibuffer-match-face-1         (nil ,bg ,violet1))
        (ivy-minibuffer-match-face-2         (nil ,bg ,yellow))
        (ivy-minibuffer-match-face-3         (nil ,bg ,yellow))
        (ivy-minibuffer-match-face-4         (nil ,bg ,yellow))
        (ivy-minibuffer-match-highlight      (ivy-current-match))
        (ivy-virtual                         (default))
        (ivy-subdir                          (default))
        (ivy-remote                          (default))
        (ivy-org                             (default))

        (magit-heading-highlight (nil nil ,(brighten bg 0.08)))
        (magit-heading-selection (nil ,bg ,(overlay bg yellow 0.8)))
        (magit-bisect-bad (nil ,red))
        (magit-bisect-good (nil ,green))
        (magit-bisect-skip (nil ,orange))
        (magit-blame-date (nil ,blue2))
        (magit-blame-heading (magit-heading ,orange))
        (magit-branch-current (nil ,blue2))
        (magit-branch-local (nil ,blue2))
        (magit-branch-remote (nil ,green))
        (magit-cherry-equivalent (nil ,violet1))
        (magit-cherry-unmatched (nil ,blue2))
        (magit-tag (nil ,yellow))
        (magit-filename (nil ,violet1))
        
        (magit-diff-added (nil ,green ,(overlay bg green 0.1)))
        (magit-diff-added-highlight (nil ,green ,(overlay bg green 0.2)))

        (magit-diff-removed (nil ,red ,(overlay bg red 0.1)))
        (magit-diff-removed-highlight (nil ,red ,(overlay bg red 0.2)))

        (magit-diff-base (nil ,orange ,(overlay bg orange 0.1)))
        (magit-diff-base-highlight (nil ,orange ,(overlay bg orange 0.2)))

        (magit-diff-context (default))
        (magit-diff-context-highlight (nil nil ,(brighten bg 0.05)))
        
        (magit-diff-file-heading (default))
        (magit-diff-file-heading-highlight (magit-heading-highlight))
        (magit-diff-file-heading-selection (magit-heading-selection))
        
        (magit-diff-hunk-heading (nil nil ,(overlay bg violet2 0.8)))
        (magit-diff-hunk-heading-highlight (nil nil ,violet2))
        (magit-diff-hunk-heading-selection (magit-heading-selection))
        ;; selected hunk region
        (magit-diff-hunk-region (italic))
        ;; this also determines the hunk region boundary
        (magit-diff-lines-heading (nil ,bg ,yellow))
        
        (magit-section-heading (nil ,blue2))
        (magit-section-highlight (magit-heading-highlight))
        (magit-section-heading-selection (magit-heading-selection))

        
        (magit-diffstat-added (nil ,green))
        (magit-diffstat-removed (nil ,red))
        (magit-dimmed (nil ,fg-weak))
        (magit-hash   (nil ,fg-weak))
        (magit-header-line (outline-3))
        (magit-log-author (nil ,yellow))
        (magit-log-date (nil ,blue2))
        (magit-log-graph (nil ,fg-weak))
        (magit-process-ng (error))
        (magit-process-ok (success))
        (magit-reflog-amend (nil ,violet1))
        (magit-reflog-checkout (nil ,blue2))
        (magit-reflog-cherry-pick (nil ,green))
        (magit-reflog-commit (nil ,green))
        (magit-reflog-merge (nil ,green))
        (magit-reflog-other (nil ,blue2))
        (magit-reflog-rebase (nil ,violet1))
        (magit-reflog-remote (nil ,blue2))
        (magit-reflog-reset (error))
        (magit-refname (nil ,fg-weak))
        (magit-sequence-drop (nil ,red))
        (magit-sequence-head (nil ,blue2))
        (magit-sequence-part (nil ,orange))
        (magit-sequence-stop (nil ,green))
        (magit-signature-bad (error))
        (magit-signature-error (error))
        (magit-signature-expired (nil ,orange))
        (magit-signature-good (success))
        (magit-signature-revoked (nil ,orange))
        (magit-signature-untrusted (nil ,orange))
        (magit-section-secondary-heading (nil ,violet1))

        (rainbow-delimiters-depth-1-face (nil ,green))
        (rainbow-delimiters-depth-2-face (nil ,yellow))
        (rainbow-delimiters-depth-3-face (nil ,violet1))
        (rainbow-delimiters-depth-4-face (nil ,blue2))
        (rainbow-delimiters-depth-5-face (nil ,violet1))
        (rainbow-delimiters-depth-6-face (nil ,yellow))
        (rainbow-delimiters-depth-7-face (nil ,blue2))
        (rainbow-delimiters-unmatched-face (nil ,red))
        (rainbow-delimiters-mismatched-face
         (rainbow-delimiters-unmatched-face))

        (smerge-lower (magit-diff-added))
        (smerge-upper (magit-diff-removed))
        (smerge-base  (magit-diff-base))
        (smerge-markers (nil nil nil nil bold))

        (which-key-key-face                   (nil ,green))
        (which-key-group-description-face     (nil ,violet1))
        (which-key-command-description-face   (nil ,blue2))
        (which-key-local-map-description-face (nil ,violet2))

        (hl-paren-face (nil "green" nil nil bold))

        (outline-1 (info-title-2))
        (outline-2 (info-title-3 ,(brighten fg 0.3)))
        (outline-3 (info-title-4))
        (outline-4 (outline-3 ,(brighten fg 0.3)))
        (outline-5 (outline-3))
        (outline-6 (outline-4))
        (outline-7 (outline-3))
        (outline-8 (outlint-4))

        (org-level-1 (info-title-3))
        (org-level-2 (info-title-4 ,(brighten fg 0.3)) (:height 1.1))
        (org-document-title (info-title-1))
        (org-meta-line (shadow nil nil nil nil italic))
        (org-document-info (org-meta-line))
        (org-document-info-keyword (org-meta-line))

        (org-verbatim (fixed-pitch))
        (org-code (org-verbatim))
        (org-block            ((org-verbatim block)) (:extend t))
        (org-block-begin-line ((org-block org-meta-line)))
        (org-block-end-line   ((org-block org-meta-line)))
        (org-formula        (fixed-pitch))
        (org-quote            (nil nil ,bg-alt) (:extend t))

        ;; (org-table (default))
        (org-todo (highlight-fg-only-1))
        (org-time-grid (nil ,yellow))
        (org-upcoming-deadline (nil ,red))
        
        (helpful-heading (info-title-3))

        (ghelp-entry-title (info-title-2))

        (diff-hl-change (nil ,orange ,(overlay bg orange 0.1)))

        (line-number              ((fixed-pitch default)))
        (line-number-current-line (nil nil ,bg-alt))
        (line-number-major-tick   (line-number))
        (line-number-minor-tick   (line-number))

        (avy-lead-face      (nil ,bg ,yellow))
        (avy-lead-face-0    (nil ,bg ,violet1))
        (avy-lead-face-1    (nil ,bg ,green))
        (avy-lead-face-2    (nil ,bg ,blue2))

        (table-cell (defualt))

        (tab-line (mode-line nil ,(brighten bg 0.1))
                  (:box (:line-width 3 :color ,(brighten bg 0.1))))
        (tab-line-tab (tab-line nil ,(brighten bg 0.15))
                      (:box (:line-width 3 :color ,(brighten bg 0.15))))
        (tab-line-highlight
         (tab-line nil ,(brighten bg 0.2))
         (:box (:line-width 3 :color ,(brighten bg 0.2))))
        (tab-line-tab-current
         (tab-line nil ,bg) (:box (:line-width 3 :color ,bg)))
        (tab-line-tab-inactive (tab-line-tab))

        (rime-default-face (tooltip) (:height 160))
        (rime-highlight-candidate-face ((bold rime-default-face)))
        (rime-code-face ((variable-pitch rime-default-face)))))))

(provide-theme 'cyberpunk)

;;; cyberpunk-theme.el ends here

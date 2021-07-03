(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action 'accept)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#f0f0f0" "#e45649" "#50a14f" "#986801"
    "#4078f2" "#a626a4" "#0184bc" "#1b2229"])
 '(apropos-do-all t)
 '(auto-save-timeout 5)
 '(backup-by-copying t nil nil "Prevent Emacs from breaking hard links.")
 '(blink-cursor-mode nil)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(confirm-nonexistent-file-or-buffer t)
 '(create-lockfiles nil)
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes '())
 '(default-input-method "rime")
 '(delete-auto-save-files nil)
 '(delete-by-moving-to-trash t)
 '(dired-omit-files "\\`[.]?#\\|\\`[.][.]?" nil nil "Hide all dot files")
 '(display-line-numbers-width 3)
 '(doc-view-resolution 300)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-display-truncation-message nil)
 '(eldoc-echo-area-display-truncation-message nil)
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-echo-area-use-multiline-p 3)
 '(eldoc-idle-delay 0.1)
 '(electric-pair-mode t)
 '(electric-quote-mode t)
 '(enable-recursive-minibuffers t)
 '(fast-but-imprecise-scrolling t)
 '(global-so-long-mode t)
 '(help-window-select t)
 '(history-delete-duplicates t)
 '(history-length 500)
 '(idle-update-delay 0.5)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-ring-max 100)
 '(minibuffer-electric-default-mode t)
 '(mouse-scroll-delay 0)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(0.03))
 '(mouse-wheel-tilt-scroll t)
 '(ns-pop-up-frames nil nil nil "This way emacsclient doesn't open new frames.")
 '(org-fontify-quote-and-verse-blocks t)
 '(org-hide-emphasis-markers nil)
 '(org-image-actual-width '(300))
 '(package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(geiser-guile
     geiser-racket adaptive-wrap vundo matlab-emacs
     grammarbot beginner-guide zmq yaml-mode writegood-mode
     async which-key web-mode visual-regexp toc-org s flycheck tide
     ivy swiper sly dash-functional selectrum popup posframe rime rg
     restclient request rainbow-delimiters quickrun pyvenv proof-general
     pdf-tools package-lint org-download olivetti minions markdown-mode
     f hl-todo magit magit-patch-changelog lua-mode ht lsp-mode keycast
     jupyter highlight-parentheses elisp-refs helpful haskell-mode
     goto-chg google-translate git-timemachine git-link geiser
     flymake-grammarly fish-mode evil eldoc-box diff-hl debbugs
     counsel company auctex eglot company-pollen pollen-mode elpher
     org org-web-tools yasnippet ws-butler undo-tree srefactor smex
     separedit sage-shell-mode rainbow-mode pinyinlib nyan-mode
     mips-mode memory-usage magit-todos latex-preview-pane langtool
     keyfreq ivy-xref iscroll htmlize expand-region dired-rsync ccls
     buffer-move benchmark-init avy auto-pause aio aggressive-indent))
 '(recentf-max-saved-items 1000)
 '(ring-bell-function 'ignore)
 '(savehist-additional-variables
   '(tablist-named-filter extended-command-history))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 4)
 '(scroll-preserve-screen-position nil)
 '(send-mail-function 'mailclient-send-it)
 '(sentence-end-double-space nil)
 '(split-height-threshold nil)
 '(split-width-threshold 90)
 '(tool-bar-mode t)
 '(use-dialog-box nil)
 '(use-package-hook-name-suffix "")
 '(user-full-name "Yuan Fu")
 '(user-mail-address "casouri@gmail.com")
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil)
 '(visible-cursor nil)
 '(word-wrap-by-category t)
 '(xref-prompt-for-identifier
   '(not xref-find-references xref-find-definitions
         xref-find-definitions-other-window
         xref-find-definitions-other-frame)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ghelp-entry ((t :inherit nil)))
 '(info-body ((t (:inherit variable-pitch :height 1.1 :family "Charter"))))
 '(org-table ((t nil))))

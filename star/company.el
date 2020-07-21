;; -*- lexical-binding: t -*-

(defvar luna-company-manual nil
  "If t, invoke company manually.")


(when luna-company-manual
  ;; we secretly swap tab and C-o so
  ;; default bindings of tab (folding, indent, etc) goes to C-o
  ;; (define-key input-decode-map (kbd "<tab>") (kbd "C-o"))
  ;; (define-key input-decode-map (kbd "C-o") (kbd "<tab>"))
  (global-set-key (kbd "<C-i>") #'company-complete)
  (global-set-key (kbd "M-o") #'company-complete))
(luna-def-key
 :keymaps '(company-active-map company-search-map)
 "C-p" #'company-select-previous
 "C-n" #'company-select-next
 :keymaps 'company-search-map
 "<escape>" #'company-abort)

(load-package company
  :hook (prog-mode-hook . company-mode)
  :config
  (if luna-company-manual
      (setq company-idle-delay nil
            company-auto-complete t)
    (setq company-idle-delay 0.1))
  (setq company-minimum-prefix-length 1
        company-dabbrev-downcase nil
        company-tooltip-limit 15)
  (setq-default company-search-filtering t))


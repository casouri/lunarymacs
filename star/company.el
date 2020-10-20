;; -*- lexical-binding: t -*-

(luna-def-key
 :keymaps '(company-active-map company-search-map)
 "C-p" #'company-select-previous
 "C-n" #'company-select-next
 :keymaps 'company-search-map
 "<escape>" #'company-abort)

(load-package company
  :hook (prog-mode-hook . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1
        company-dabbrev-downcase nil
        company-tooltip-limit 15)
  ;; company dabbrev is annoying, make sure not to include it.
  (setq-default company-backends
                '(company-capf company-files company-dabbrev-code))
  (setq-default company-search-filtering t))


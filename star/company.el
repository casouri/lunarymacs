;;; -*- lexical-binding: t -*-

(load-package company
  :commands (company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-dabbrev-downcase nil
        company-tooltip-limit 15)
  (setq-default company-search-filtering t))

(luna-with-eval-after-load 'key.general
  (general-define-key
   :keymaps '(company-active-map
              company-search-map)
   "C-j"      #'company-search-candidates)
  (general-define-key
   :keymaps 'company-search-map
   "<escape>" #'company-abort))

(load-package company-box
  ;; don't enable by default
  :config
  (setq company-box-enable-icon nil)
  (setq company-box-doc-delay 0.3)
  :commands company-box-mode)

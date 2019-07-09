;;; -*- lexical-binding: t -*-

(load-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-dabbrev-downcase nil
        company-tooltip-limit 15)
  (setq-default company-search-filtering t)
  (global-company-mode 1))

(luna-with-eval-after-load 'key.general
  (general-define-key
   :keymaps '(company-active-map
              company-search-map)
   "C-j"      #'company-search-candidates
   "C-p"      #'company-select-previous
   "C-n"      #'company-select-next)
  (general-define-key
   :keymaps 'company-search-map
   "<escape>" #'company-abort))

(load-package company-box
  ;; don't enable by default
  :config
  (setq company-box-enable-icon nil)
  (setq company-box-doc-delay 0.3)
  :commands company-box-mode)

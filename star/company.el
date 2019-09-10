;;; -*- lexical-binding: t -*-

(defvar luna-company-manual t
  "If t, invoke company manually.")

(load-package company
  :commands (company-mode)
  :config
  (if luna-company-manual
      (setq company-idle-delay 9999
            company-auto-complete t)
    (setq company-idle-delay 0.1))
  (setq company-minimum-prefix-length 1
        company-dabbrev-downcase nil
        company-tooltip-limit 15)
  (setq-default company-search-filtering t))

(luna-with-eval-after-load 'key.general
  (when luna-company-manual
    (general-define-key
     "C-o" #'company-complete)
    (general-define-key
     :keymaps '(company-active-map
                company-search-map)
     "C-p" #'company-select-previous
     "C-n" #'company-select-next))
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

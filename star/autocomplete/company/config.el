(use-package
  company
  :hook prog-mode
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil)
  )

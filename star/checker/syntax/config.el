(use-package| flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  )

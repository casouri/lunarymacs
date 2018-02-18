;;; -*- lexical-binding: t -*-

(use-package| flyspell
  :commands moon/toggle-spell-check
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)))


(post-config| general
  (default-leader
    "ts" #'moon/toggle-spell-check))


(use-package| flyspell-correct
  :hook (flyspell-mode . (lambda () (require 'flyspell-correct)))
  :defer t
  :config
  (require 'flyspell-correct-ivy)
  (default-leader
    "Ef" #'flyspell-correct-previous-word-generic))

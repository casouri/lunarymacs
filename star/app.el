;; -*- lexical-binding: t -*-
;;
;; Standalone applications.

;;; Package

(with-eval-after-load 'erc
  (setq erc-nick "casouri"
        erc-nickserv-passwords
        '((freenode (("casouri" . "XF234567ic"))))
        erc-prompt-for-nickserv-password nil)
  (require 'erc-services)
  (erc-services-mode))

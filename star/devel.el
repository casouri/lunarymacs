;; -*- lexical-binding: t -*-

;;; Packages

(load-package package-demo
  :defer t)


(load-package esup
  :defer t)

(defun esup-with-dump ()
  "Esup with dump file."
  (interactive)
  (esup nil (format "--dump=%s" luna-dump-file)))

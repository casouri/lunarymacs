;; -*- lexical-binding: t -*-

(load-package pollen-mode
  ;; Set parent to text-mode so electric-quote-mode works.
  :config
  (put 'pollen-mode 'derived-mode-parent 'text-mode)
  (add-hook 'pollen-mode-hook 'visual-line-mode)
  (require 'company-pollen))

(load-package company-pollen
  :defer)

(defvar luna-rock-src "~/p/casouri/rock/day/src/"
  "Source directory for Rock/Day.")

(defun luna-new-rock ()
  (interactive)
  (let ((day-count (length (seq-filter
                            (lambda (name)
                              (string-match ".html.pm$" name))
                            (directory-files
                             luna-rock-src)))))
    (find-file (expand-file-name
                (format "day-%d.html.pm" (1+ day-count))
                luna-rock-src))
    (insert "#lang pollen

◊define-meta[date]{}

◊head{
◊cover{◊cover-img{}}
◊artist{}
◊title{}
◊album{}
◊year{}
}

◊body{

◊lyrics{

} ◊;lyrics
} ◊;body")))

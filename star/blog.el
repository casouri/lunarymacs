;; -*- lexical-binding: t -*-

(luna-key-def
 :keymaps 'pollen-mode-map
 "<tab>" #'indent-for-tab-command)

(load-package company-pollen :defer t)

(load-package pollen-mode
  :mode "\\.pm$"
  :config
  ;; Set parent to text-mode so electric-quote-mode works.
  (put 'pollen-mode 'derived-mode-parent 'text-mode)
  (add-hook 'pollen-mode-hook 'setup-pollen)
  (require 'company-pollen))

(defun setup-pollen ()
  "Enable a bunch of modes for Pollen mode."
  (flyspell-mode)
  (electric-quote-local-mode 1)
  (electric-indent-local-mode)
  (visual-line-mode)
  (setq-local indent-line-function
              #'pollen-indent-function)
  (setq-local comment-start "◊; ")
  (setq-local outline-regexp "◊\\(sub\\)?section{"
              outline-minor-mode-cycle t
              outline-minor-mode-highlight 'append)
  (variable-pitch-mode)
  (when (featurep 'highlight-parentheses)
    (highlight-parentheses-mode -1))
  (outline-minor-mode)
  (font-lock-add-keywords
   nil `((,(rx (seq "◊" (+ (not (any "[" "{" " " "," "."))))) 0 'fixed-pitch prepend)
         (,(rx (or "[" "]" "{" "}")) 0 'fixed-pitch prepend))
   'append)
  (jit-lock-register #'pollen-fontify-code))

(defun pollen-indent-function ()
  "Indent function for Pollen source."
  (let ((mark (point-marker)))
    (if (eq ?\( (char-after (cadr (syntax-ppss))))
        ;; If in parenthesis, indent like lisp.
        (lisp-indent-line)
      (beginning-of-line)
      ;; Skip white-spaces.
      (skip-syntax-forward "-")
      (let ((indent (if (looking-at "}")
                        (1- (car (syntax-ppss)))
                      (car (syntax-ppss)))))
        (when (> indent 0)
          (indent-line-to (* 2 indent))
          (goto-char mark)
          (when (eq (point) (line-beginning-position))
            (skip-syntax-forward "-")))))))

(defun pollen-fontify-code (start end)
  "Fontify ◊code and ◊bcode with fixed-pitch."
  (goto-char start)
  (while (re-search-forward "◊b?code{" end t)
    (unless (or (nth 3 (syntax-ppss))
                (nth 4 (syntax-ppss)))
      (let ((start (point))
            (depth (car (syntax-ppss))))
        (while (and (search-forward "}" nil t)
                    ;; Skip "}" in strings.
                    (or (nth 3 (syntax-ppss))
                        ;; Skip "}" in comments.
                        (nth 4 (syntax-ppss))
                        ;; Skip nested "}". (syntax-ppss (1- (point)))
                        ;; moves point back, so we use (point) rather
                        ;; than (1- (point)).
                        (not (eq (car (syntax-ppss (point)))
                                 (1- depth))))))
        (when (looking-back "}" 1)
          (put-text-property start (1- (point))
                             'font-lock-face 'fixed-pitch)
          (put-text-property start (1- (point)) 'face 'fixed-pitch)
          (put-text-property start (1- (point))
                             'jit-lock-defer-multiline t)))))
  `(jit-lock-bounds ,start . ,(point)))


;;; Notes

(defvar luna-note-root "~/p/casouri/note/"
  "Root directory for Notes.")

(defun luna-new-note (dir-name)
  "Create a new post for Notes.
The post in placed under <year>/DIR-NAME."
  (interactive "MDirectory name: ")
  (let* ((year (substring (current-time-string) 20))
         (year-path (expand-file-name
                     year luna-note-root))
         (dir-path (expand-file-name dir-name year-path))
         (file-path (expand-file-name "index.html.pm" dir-path)))
    ;; Create the post’s dir and org file and insert basic information.
    (unless (file-exists-p year-path)
      (mkdir year-path))
    (mkdir dir-path)
    (find-file file-path)
    (insert "#lang pollen

◊define-meta[date]{}
◊define-meta[uuid]{}
◊define-meta[tags]{}
◊define-meta[lang]{en}

◊meta{
  ◊title{}
}

")
    (save-buffer)))

(defvar luna-rock-src "~/p/casouri/rock/day/collection"
  "Source directory for Rock/Day.")

(defun luna-new-rock ()
  "Create a new post for Rock/day."
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
◊define-meta[uuid]{}

◊meta{
  ◊day{}
  ◊cover{◊cover-img{}}
  ◊artist{}
  ◊title{}
  ◊album{}
  ◊year{}
}

◊lyrics{

}◊;lyrics
")))

(defun luna-open-album-dir ()
  "Open ~/p/casouri/rock/day/album/."
  (interactive)
  (shell-command-to-string (format "open ~/p/casouri/rock/day/album/")))

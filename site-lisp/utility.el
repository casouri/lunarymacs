;;; utility.el --- Utilities      -*- lexical-binding: t; -*-

(require 'lunary)

;;; Buffer

(defun luna-kill-other-buffer ()
  "Kill all other buffers (besides the current one).

If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  ;; copied from doom-emacs
  (interactive)
  (let ((buffers (buffer-list))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (luna-kill-buffer-and-window buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

(defun luna-kill-buffer-and-window (buffer)
  ;; copied from doom-emacs
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

(defun switch-buffer-same-major-mode ()
  "Switch buffer among those who have the same major mode as the current one."
  (interactive)
  (switch-to-buffer
   (completing-read
    "Buffer: "
    (mapcar #'buffer-name
            (cl-remove-if-not (lambda (buf)
                                (provided-mode-derived-p
                                 (buffer-local-value 'major-mode buf)
                                 major-mode))
                              (buffer-list))))))

;;; File

(defun luna-rename-file ()
  ;; https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun luna-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun luna-find-file (&optional arg)
  "Find file. If called with ARG, find file in project."
  (interactive "p")
  (call-interactively
   (if (eq arg 4)
       #'project-find-file
     #'find-file)))

;;; Package mirror

(defvar luna-package-mirror-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    `(,(cons 'melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
      ,(cons 'melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
      ,(cons 'emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
      ,(cons 'netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
      ,(cons 'tencent
             `(,(cons "gnu"   (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
      ,(cons 'tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))
  "Each mirror can be used for ‘package-archives’.")

(defun luna-change-mirror (mirror)
  "Change mirror."
  (interactive (list (completing-read
                      "Mirror: "
                      (mapcar #'car luna-package-mirror-alist)
                      nil t)))
  (require 'package)
  (setq package-archives
        (alist-get mirror luna-package-mirror-alist))
  (package-refresh-contents))

;;; ENV

(defun luna-load-env ()
  "Load PATH and CPATH from a file."
  (interactive)
  (condition-case err
      (progn (load "~/.emacsenv")
             (setq exec-path (split-string (getenv "PATH") ":")))
    (error (message (error-message-string err)))))

;;; Insert

(defvar luna-special-symbol-alist '(("(c)" . "©")
                                    ("tm" . "™")
                                    ("p" . " ")
                                    ("s" . "§")
                                    ("---" . "—") ; em dash
                                    ("--" . "–") ; en dash
                                    ("..." . "…")
                                    ("<" . "⃖")
                                    (">" . "⃗")
                                    ("^" . "ꜛ")
                                    ("v" . "ꜜ")
                                    ("<<" . "←")
                                    (">>" . "→")
                                    ("^^" . "↑")
                                    ("vv" . "↓")
                                    ("l" . "‘")
                                    ("r" . "’")
                                    ("ll" . "“")
                                    ("rr" . "”")
                                    (" " . " ") ; non-breaking space
                                    ("hand" . "☞"))
  "Alist used by `luna-insert-special-symbol'.")

(defun luna-insert-special-symbol (surname)
  "Insert special symbol at point, SURNAME is used to search for symbol.
E.g. SURNAME (c) to symbol ©."
  (interactive "MAbbrev: ")
  (insert (catch 'ret
            (dolist (elt luna-special-symbol-alist)
              (when (equal (car elt) surname)
                (throw 'ret (cdr elt)))
              ""))))

;;; Navigation

(defun luna-scroll-down-reserve-point ()
  (interactive)
  (scroll-up 2)
  (next-line 2))


(defun luna-scroll-up-reserve-point ()
  (interactive)
  (scroll-down 2)
  (previous-line 2))

;;; Auto insert

(defvar luna-autoinsert-template (luna-f-join (luna-this-file-directory)
                                              "autoinsert-template.el")
  "The template file.")

(defun luna-autoinsert (description)
  "Autoinsert what auto-insert inserts."
  (interactive "MDescription: ")
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (year (format-time-string "%Y"))
         (feature (file-name-base (buffer-file-name)))
         (buf (find-file luna-autoinsert-template))
         (template (with-current-buffer buf
                     (buffer-string))))
    (kill-buffer buf)
    (insert (format template
                    filename description feature filename))))

;;; smart-delete

(defvar luna-hungry-delete-black-list '(org-mode text-mode fundamental-mode markdown-mode)
  "A list of major mode in where `luna-hungry-delete' should behave like normal delete.")

(defun luna-hungry-delete ()
  "Smart and clean delete.
If we are at the beginning of a line, backspace
deletes all whitespace before and after point
and moves point to the previous line."
  (interactive)
  (require 'cl-lib)
  (cl-labels ((normal-delete () (if (region-active-p)
                                    (delete-region (region-beginning) (region-end))
                                  (if (and (not (eql (point) (point-max)))
                                           (eql (alist-get (char-before) '((?{ . ?}) (?\[ . ?\])
                                                                           (?\( . ?\)) (?\" . ?\")
                                                                           (?\' . ?\') (?“ . ?”) (?‘ . ?’)))
                                                (char-after)))
                                      ;; if we are in the middle of a empty pair, i.e., "|" or (|)
                                      ;; delete both
                                      (progn (forward-char)
                                             (backward-delete-char 2))
                                    (call-interactively #'backward-delete-char-untabify)))))
    (if (or (region-active-p)
            (<= (car (syntax-ppss)) 0)
            (minibufferp (current-buffer)))
        ;; if we are at top-level
        ;; do normal delete
        (normal-delete)
      ;; if the point is not before the line but inside it, do normal delete
      ;; otherwise do hungry delete
      ;;
      ;; 1. we first delete all white spaces, then insert newline and indent properly
      ;; 2. but if there is only a closing delimiter, i.e., } or ),
      ;;    we don't insert new line.
      ;; 3. if we ends up in the same place before hungry delete,
      ;;    that means the user is trying to delete back to the previous line,
      ;;    then do that.
      (let* ((point (point)) ; staring point
             (bolt (save-excursion
                     ;; `beginning-of-line-text' seems to ignore comment for some reason,
                     (beginning-of-line)
                     (skip-chars-forward " \t")
                     (point)))
             ;; beginning of the region that we are to delete
             (beg (save-excursion (while (member (char-before) '(?\n ?\s ?\t))
                                    (backward-char))
                                  (point)))
             ;; end of that region
             (end (save-excursion (goto-char bolt)
                                  (while (member (char-after) '(?\n ?\s ?\t))
                                    (forward-char))
                                  (point))))
        (if (<= point bolt)
            ;; actually decide to delete stuff
            (progn
              (delete-region beg end)
              (unless (eql (char-after) ?\))
                (call-interactively #'newline))
              ;; so we did all this and ends up not changing anything
              ;; why? because the user doesn't want to delete excess white space and add newline
              ;; but to delete back to previous line! do that.
              (when (eql (point) end)
                (delete-region beg end)
                (unless (eql (char-before) ?\()
                  (insert ?\s))))
          ;; not at beginning of text, just do normal delete
          (normal-delete))))))


(provide 'utility)

;;; utility.el ends here
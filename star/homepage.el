;;; deprecated
;;
;;; Var
;;

(defvar luna-do-draw-footer nil
  "Whether to draw footer.")

(defvar luna-do-draw-image-luna nil
  "Whether to draw image luna.")

(defvar luna-image-luna "luna-300.xpm"
  "Image luna file name.")

(defface luna-face
  '((((background  dark)) . (:foreground "#52DEA1"))
    (t . (:foreground "black")))
  "The face of luna.")

(defvar luna-banner "
███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
████╗ ████║██╔═══██╗██╔═══██╗████╗  ██║    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
██╔████╔██║██║   ██║██║   ██║██╔██╗ ██║    █████╗  ██╔████╔██║███████║██║     ███████╗
██║╚██╔╝██║██║   ██║██║   ██║██║╚██╗██║    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
██║ ╚═╝ ██║╚██████╔╝╚██████╔╝██║ ╚████║    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
                                                                                      ")

(defvar luna-small-banner "
███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗
████╗ ████║██╔═══██╗██╔═══██╗████╗  ██║
██╔████╔██║██║   ██║██║   ██║██╔██╗ ██║
██║╚██╔╝██║██║   ██║██║   ██║██║╚██╗██║
██║ ╚═╝ ██║╚██████╔╝╚██████╔╝██║ ╚████║
╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝
                                       ")

(defvar luna-log-news nil
  "Whether to show news on homepage.")

(defun luna-draw ()
  "draw a pixel."
  (interactive)
  (insert "██"))

(defvar luna-luna-banner-1 "
          ████████
      ██████████████
   █████████         █
  ████████
████████
████████
████████
████████
████████
  ████████            █
  ██████████        ██
      ██████████████
          ████████
")

(defvar luna-luna-banner-2 "
           ████
      ████████████
   ███████         █
  ██████
 █████
██████
██████
██████
 █████
  ██████            █
   ███████        ██
      ████████████
          █████
")

(defvar luna-footer-1 "
                                                            ██████████████████
                                                        ██████████████████████████
                                                    ████████████████████████████████
                                                  ██████████████████████████████████████                          ██████
                      ██████████████          ████████████████████████████████████████████                    ██████████████            ████████████
                ██████████████████████████      ████████████████████████████████████████████                ██████████████████      ██████████████████████
            ██████████████████████████████████    ██████████████████████████████████████████████        ████████████████████    ████████████████████████████
        ██████████████████████████████████████████    ████████████████████████████████████████████    ████████████████████    ████████████████████████████████
    ████████████████████████████████████████████████      ██████████████████████████████████████    ██████████████████    ██████████████████████████████████████
████████████████████████████████████████████████████████      ██████████████████████████████      ████████████████      ██████████████████████████████████████████ ")

(defvar luna-footer-2 "
                    ████████████
                ████████████████████
            ██████████████████████████
          ██████████████████    ██████
      ████████████████████        ████
    ██████████████████████      ██████
    ██████████████████████        ██
  ████████████████████████
████████████████████████████
████████████████████████████
██████████████████████████████                                                                                                ██████
████████████████████████████████                                        ████████████                                    ████████████████
██████████████████████████████████                                ████████████████████                                ████████████████████
██████████████████████████████████████                          ████████████████████████                            ██████████████████████
████████████████████████████████████████                      ██████████████████    ████                        ██████████████████    ████
████████████████████████████████████████████                ████████████████      ██████                      ██████████████████    ██████
██████████████████████      ██████████████████████        ██████████████████        ██                      ████████████████████      ██
████████████████████    ██    ████████████████████████  ████████████████████                              ████████████████████████
████████████████      ██████      ██████████████████    ██████████████████████                          ████████████████████████████
████████████      ██████████████      ████████████    ██████████████████████████████      ██████      ████████████████████████████████                      ████
████████      ██████████████████████      ██████    ██████████████████████████████      ██████████    ████████████████████████████████████            ██████████
██        ██████████████████████████████          ██████████████████████████████    ████████████████      ████████████████████████████████████      ████████████
  ██████████████████████████████████████████    ██████████████████████████████    ██████████████████████        ██████████████████████████████████      ████████
██████████████████████████████████████████    ████████████████████████████      ██████████████████████████████        ████████████████████████████████        ██
██████████████████████████████████████      ████████████████████████        ████████████████████████████████████████          ██████████████████████████████
██████████████████████████████████      ████████████████████████      ██████████████████████████████████████████████████████      ██████████████████████████████
")

(defvar luna-long-banner luna-luna-banner-1
  "The longer banner.")

(defvar luna-short-banner luna-luna-banner-1
  "The short banner.")

(defvar luna-homepage-footer luna-footer-1
  "Footer for homepage.")

;;
;;; Func
;;


;; deprecate
(defun luna-draw-star (num)
  "Draw NUM stars randomly"
  (interactive))

(defun luna-draw-footer (footer &optional shift)
  "Draw the FOOTER at the bottom of buffer and shift up SHIFT lines."
  (interactive)
  (let* ((height (window-height))
         (current-eof (string-to-number
                       (replace-regexp-in-string "^Line " "" (what-line))))
         (footer-list (split-string footer "\n"))
         (footer-height (length footer-list))
         (width (window-width)))
    (insert (make-string (- height current-eof footer-height (or shift 0)) ?\n))
    (dolist (line footer-list)
      (insert (if (> (length line) width)
                  (substring line 0 width)
                line))
      (insert ?\n))))

(defun luna-draw-image-luna (beg end)
  "Put an image luna as overlay from BEG to END."
  (overlay-put (make-overlay beg end) 'display (create-image (expand-file-name luna-image-luna (concat luna-star-dir "basic/homepage")))))

(defun luna-draw-luna (banner short-banner)
  "Draw luna."
  (interactive)
  (let* ((banner (if (>= (window-width) 86)
                     banner
                   short-banner))
         (banner-list (split-string (car banner) "\n"))
         (banner-width (or (nth 1 banner) (length (nth 1 banner-list))))
         (pad-length (let ((total-extra (- (window-width) banner-width)))
                       (if (wholenump total-extra)
                           (/ total-extra 2)
                         0))))
    (let ((space-to-insert
	   (make-string pad-length ?\s)))
      (dolist (line banner-list)
	(insert space-to-insert)
	(insert line)
	(insert "\n")))))

(defun luna-log-news ()
  "Log core changes since last pull."
  (interactive)
  (when luna-log-news
    (insert "\n\n\n* Core changes\n\n")
    (insert (shell-command-to-string (concat "cd " luna-emacs-d-dir " ; git log --pretty=format:'** %h - %s%n%n%b' --grep ':core:'")))))

;;;; Final function

(defun luna-draw-homepage ()
  "Draw LUNA EMACS or LUNA on the middle of current buffer.

Update: now it draws a real luna.

LUNA is used when buffer's width is less than 86."
  (interactive)
  (unless noninteractive
    (insert (make-string 5 ?\n))

    (if (and window-system luna-do-draw-image-luna)
        (progn
          (insert (make-string (/ (- (window-width) 50) 2) ?\s))
          (luna-draw-image-luna (1- (point)) (point)))
      (insert (make-string 5 ?\n))
      (luna-draw-luna `(,luna-long-banner 40) `(,luna-short-banner 20)))
    (goto-char (point-max))
    (when luna-do-draw-footer
      (luna-draw-footer luna-footer-2 (if (and luna-do-draw-image-luna window-system) 20 0)))
    (luna-log-news))
  (put-text-property (point-min) (point-max) 'face 'luna-face)
  (goto-char (point-min)))

;;
;;; Config
;;


;; Homepage
;; (add-hook 'luna-startup-hook-1 #'luna-draw-homepage t)
;; (add-hook 'luna-startup-hook-2 #'luna-display-benchmark t)

;; splash screen

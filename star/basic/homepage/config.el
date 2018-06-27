;;
;; Var
;;

(defvar moon-banner "
███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
████╗ ████║██╔═══██╗██╔═══██╗████╗  ██║    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
██╔████╔██║██║   ██║██║   ██║██╔██╗ ██║    █████╗  ██╔████╔██║███████║██║     ███████╗
██║╚██╔╝██║██║   ██║██║   ██║██║╚██╗██║    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
██║ ╚═╝ ██║╚██████╔╝╚██████╔╝██║ ╚████║    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
                                                                                      ")

(defvar moon-short-banner "
███╗   ███╗ ██████╗  ██████╗ ███╗   ██╗
████╗ ████║██╔═══██╗██╔═══██╗████╗  ██║
██╔████╔██║██║   ██║██║   ██║██╔██╗ ██║
██║╚██╔╝██║██║   ██║██║   ██║██║╚██╗██║
██║ ╚═╝ ██║╚██████╔╝╚██████╔╝██║ ╚████║
╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝
                                       ")

(defvar moon-log-news nil
  "Whether to show news on homepage.")

;;
;; Func
;;


(defun moon/draw-homepage ()
  "Draw MOON EMACS or MOON on the middle of current buffer.

MOON is used when buffer's width is less than 86."
  (interactive)
  (unless noninteractive
    (let ((banner-list (if (>= (window-width) 86)
                           (split-string moon-banner "\n")
                         (split-string moon-short-banner "\n"))))
      (let ((space-to-insert
	     (make-string
	      (/ (let ((free-length
                        (- (window-width)
                           ;; first line could be empty, use second line for length
                           (length (nth 1 banner-list)))))
                   (if (wholenump free-length)
                       free-length
                     0)) 2) ?\s)))
        (insert (make-string 10 ?\n))
        (dolist (line banner-list)
	  (insert space-to-insert)
	  (insert line)
	  (insert "\n")))))
  (moon/log-news)
  (goto-char (point-min)))

(defun moon/log-news ()
  "Log core changes since last pull."
  (interactive)
  (when moon-log-news
    (insert "\n\n\n* Core changes\n\n")
    (insert (shell-command-to-string (concat "cd " moon-emacs-d-dir " ; git log --pretty=format:'** %h - %s%n%n%b' --grep ':core:'")))))

;;
;; Config
;;

;; Homepage
(add-hook 'moon-init-hook #'moon/draw-homepage t)
(add-hook 'moon-post-init-hook #'moon-display-benchmark t)


;; visual line mode
;; visual line mode makes swiper very slow
;; so I'll disable it for now
;; (global-visual-line-mode 1)

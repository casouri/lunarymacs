;;
;; Var
;;

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

;;
;; Config
;;

(cond (IS-MAC
       (when (display-graphic-p)
	 (add-to-list 'exec-path "/usr/local/bin")
	 (add-to-list 'exec-path (expand-file-name "~/bin"))
	 )
       )
      )

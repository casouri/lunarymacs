(use-package| ob-ipython
  :defer t
  :init
  (add-hook
   'org-mode-hook
   (lambda ()
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((ipython . t)))
     )))

(add-hook
 'org-mode-hook
 (lambda ()
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((shell . t)))
   ))

(defvar moon-enable-org-bullets nil
  "Whether to enable org-bullet")

(use-package| org-bullets
  :defer t
  :init (add-hook
         'org-mode-hook
         (lambda ()
           (when moon-enable-org-bullets
             (org-bullets-mode 1)))))


(use-package| ox-hugo
  :after ox)


;;
;; Config
;;

;;
;; org-capture

(post-config| general
  (default-leader
    "uo" '(:ignore t :which-key "org")
    "uoc" #'org-capture
    ))



(after-load| org-capture
  (setq org-default-notes-file "~/note/index.org")
  (setq org-capture-templates
        (append org-capture-templates
                `(("l" "Links")
                  ("la" "Design related links." item (file+olp "~/note/index.org" "Links" "Design links") "- %^L :: ")
                  ("lp" "programming related links." item (file+olp "~/note/index.org" "Links" "Programming links") "- %^L :: ")
                  ("lw" "Web related links." item (file+olp "~/note/index.org" "Links" "Web links") "- %^L :: ")
                  ("lb" "Blog links." item (file+olp "~/note/index.org" "Links" "Blog links") "- %^L :: ")
                  ("lu" "Utility links." item (file+olp "~/note/index.org" "Links" "Utility links") "- %^L :: ")
                  ("n" "Notes")
                  ("nr" "Random notes" entry (file+olp "~/note/index.org" "Notes" "Random notes") "*** %?")
                  ("nv" "Vim notes" entry (file+olp "~/note/index.org" "Notes" "Vim notes") "*** %? ")
                  ("ne" "Emacs notes" entry (file+olp "~/note/index.org" "Notes" "Emacs notes") "*** %?")
                  ("nt" "Tutorial notes" entry (file+olp "~/note/index.org" "Notes" "Tutorial notes") "*** %?")
                  ))))

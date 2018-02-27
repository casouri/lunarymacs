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

(use-package| org-bullets
  :defer t
  :init (add-hook
         'org-mode-hook
         (lambda ()
           (org-bullets-mode 1))))

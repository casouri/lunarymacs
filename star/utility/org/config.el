(delay-use-package| ob-ipython
                    :hook (org-mode
                           .
         (lambda ()
           (require 'ob-ipython)
           (org-babel-do-load-languages
            'org-babel-load-languages
            '((ipython . t)))
           )))

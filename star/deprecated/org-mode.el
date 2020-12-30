;;; Org Agenda config

(defvar luna-todo-file "~/note/todo.org")
(setq org-agenda-files (list luna-todo-file))
(setq org-todo-keywords
      '((sequence "TODO"
                  "NEXT"
                  "START"
                  "WAIT"
                  "DEFER"
                  "|"
                  "DONE"
                  "CANCEL")))
(setq org-agenda-custom-commands
      '(("d" "Default Agenda View"
         ((agenda "")
          (todo ""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                 (org-agenda-overriding-header "Unscheduled/deadline tasks:")))))))

(setq org-priority-faces
      '((?A . (:inherit font-lock-warning-face))
        (?B . (:inherit default))
        (?C . (:inherit font-lock-comment-face))))

(setq org-todo-keyword-faces
      '(("DEFER" . (:inherit default :weight bold))))

;;; Org Capture config

(with-eval-after-load 'org-capture
  (setq org-default-notes-file "~/note/index.org")
  (setq org-capture-templates
        (append org-capture-templates
                `(("t" "TODOs")
                  ("te" "Emacs" entry
                   (file+olp "~/note/todo.org" "Emacs") "*** TODO %?")
                  ("th" "Homework" entry
                   (file+olp "~/note/todo.org" "Homework") "*** TODO %?")
                  ("to" "Other" entry
                   (file+olp "~/note/todo.org" "Other") "*** TODO %?")
                  ("ts" "School" entry
                   (file+olp "~/note/todo.org" "School") "*** TODO %?")
                  ("tr" "Readlist" entry
                   (file+olp "~/note/todo.org" "Readlist") "*** TODO %?")
                  ))))

;;; luna-tests.el --- Tests for luna functions      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'ert)
(require 'luna-load-package)

;;; luna-load-package

(ert-deftest luna-split-command-args-test ()
  (should (equal (luna-split-command-args
                  '(:command1 arg1 arg2 arg3 :command2 arg1 arg2))
                 '((:command1 arg1 arg2 arg3) (:command2 arg1 arg2)))))
(ert-deftest luna-load-package--handle-hook-test ()
  (should (equal (luna-load-package--handle-hook
                  '((hook1 . func1) ((hook21 hook22) . func2)
                    (hook3 . (func31 func32)))
                  'pkg)
                 '((autoload #'func1 "pkg" nil t)
                   (add-hook 'hook1 #'func1)
                   (autoload #'func2 "pkg" nil t)
                   (add-hook 'hook21 #'func2)
                   (add-hook 'hook22 #'func2)
                   (autoload #'func31 "pkg" nil t)
                   (add-hook 'hook3 #'func31)
                   (autoload #'func32 "pkg" nil t)
                   (add-hook 'hook3 #'func32)))))
(ert-deftest luna-load-package-test ()
  (should (equal (macroexpand-1 '(luna-load-package pkg
                                   :init (init1) (init2)
                                   :config (config1) (cnofig2)
                                   :hook (hook . (func1 func2))))
                 '(condition-case err
                      (progn (add-to-list 'luna-package-list 'pkg)
                             (init1) (init2)
                             (with-eval-after-load 'pkg
                               (config1) (cnofig2))
                             (autoload #'func1 "pkg" nil t)
                             (add-hook 'hook #'func1)
                             (autoload #'func2 "pkg" nil t)
                             (add-hook 'hook #'func2)
                             nil)
                    ((debug error)
                     (warn "Error when loading %s: "
                           package (error-message-string err)))))))

(provide 'luna-tests)

;;; luna-tests.el ends here

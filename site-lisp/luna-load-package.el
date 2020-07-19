;;; luna-load-package.el --- use-package for Lunarymacs      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'pcase)
(require 'cl-lib)

(defun luna-split-command-args (args)
  "Split args into commands and args.
ARGS: (:command args args args :command args)
Return: ((:command . (args args args)) (:command . (args)))."
  (let (ret-list arg-list command)
    (dolist (token (append args '(:finish)))
      (if (keywordp token)
          ;; Finish previous command
          (progn (if command (push (cons command (reverse arg-list))
                                   ret-list))
                 (setq arg-list nil)
                 ;; Start new command
                 (setq command token))
        (push token arg-list)))
    (reverse ret-list)))

;; test: (luna-split-command-args '(:command1 arg1 arg2 arg3 :command2 arg1 arg2))

(defun luna-load-package--handle-hook (arg-list package)
  "Handle hook arguments.
PACKAGE is the package you are loading.
Each ARG in ARG-LIST is a cons (HOOK . FUNCTION).
HOOK can be either a single hook or a list of hooks.
FUNCTION can also be either a single function or a list of them."
  (let (ret-list hook-list func-list)
    (dolist (arg arg-list)
      (let ((hook (car arg))
            (func (cdr arg)))
        ;; Normalize to lists.
        (setq hook-list
              (if (symbolp hook) (list hook) hook))
        (setq func-list
              (if (symbolp func) (list func) func)))
      ;; Produce add-hook forms.
      (dolist (func func-list)
        (push `(autoload #',func ,(symbol-name package) nil t) ret-list)
        (dolist (hook hook-list)
          (push `(add-hook ',hook #',func) ret-list))))
    (reverse ret-list)))

(defmacro luna-load-package (package &rest args)
  "Like ‘use-package’.
PACKAGE is the package you are loading.
ARGS contains commands and arguments.
Available commands:

  :init        Same as use-package
  :config      Same as use-package
  :hook        Each arguments is (HOOK . FUNC)
               HOOK and FUNC can be a symbol or a list of symbols.
  :load-path   Load paths to add.
  :mode        Same as use-package
  :commands    Same as use-package
  :interpreter Same as use-package
  :after       Same as use-package"
  (declare (indent 1))
  ;; Group commands and arguments together.
  (let* ((arg-list (luna-split-command-args args))
         ;; Translate commands & arguments to valid
         ;; config code.
         (body
          (mapcan
           (lambda (arg)
             (let ((command (car arg))
                   (arg-list (cdr arg)))
               (pcase command
                 (:init arg-list)
                 (:config `((with-eval-after-load ',package
                              ,@arg-list)))
                 (:hook (luna-load-package--handle-hook
                         arg-list package))
                 (:load-path
                  (mapcar (lambda (path)
                            `(add-to-list 'load-path ,path))
                          arg-list))
                 (:mode
                  (mapcar (lambda (pattern)
                            `(add-to-list 'auto-mode-alist ,pattern))
                          arg-list))
                 (:interpreter
                  (mapcar (lambda (pattern)
                            `(add-to-list 'interpreter-mode-alist
                                          ,pattern))
                          arg-list))
                 (:commands
                  (mapcar (lambda (cmd)
                            `(autoload ',cmd ,(symbol-name package) nil t))
                          arg-list))
                 (:after
                  (mapcar (lambda (pkg)
                            `(with-eval-after-load ',pkg
                               (require ',package)))
                          arg-list)))))
           arg-list))
         ;; In which case we don’t require the package.
         (defer-p (let ((commands (mapcar #'car arg-list)))
                    (or (memq :defer commands)
                        (memq :commands commands)
                        (memq :after commands)
                        (memq :mode commands)
                        (memq :hook commands)))))
    `(progn
       (add-to-list 'luna-package-list ',package)
       ,@body
       ,(unless defer-p `(require ',package)))))

(defalias 'load-package 'luna-load-package)

(provide 'luna-load-package)

;;; luna-load-package.el ends here

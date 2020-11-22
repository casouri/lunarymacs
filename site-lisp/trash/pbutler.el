;;; pbutler.el --- Package butler      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'cl-lib)

(defvar pbutler-major-mode-recommendation-list
  '((("\\.sage\\'") . sage-shell-mode)
    (("\\.yaml\\'") . yaml-mode)
    (("\\.md\\'") . markdown-mode)
    (("\\.hs\\'") . haskell-mode)
    (("\\.mips\\'") . mips-mode)
    (("\\.phtml\\'"
      "\\.tpl\\.php\\'"
      "\\.[agj]sp\\'"
      "\\.as[cp]x\\'"
      "\\.erb\\'"
      "\\.mustache\\'"
      "\\.djhtml\\'"
      "\\.html?\\'") . web-mode)
    (("\\.lua\\'") . lua-mode)
    ;; TODO other scheme variants
    ("\\.rkt\\'" "\\.scm\\'") . geiser-mode)
  "A list of recommended major modes for each type of file.")

(defvar pbutler-minor-mode-recommendation-list
  ;; TODO more for eglot
  '(((python-mode javascript-mode c-mode) . eglot)
    ((python-mode javascript-mode c-mode) . company))
  "A list of recommended minor modes for each major mode.")

(defcustom pbutler-blacklist nil
  "Packages in this list are not prompted."
  :type 'list
  :group 'pbutler)

(defun pbutler-add-to-blacklist (package)
  "Add PACKAGE to blacklist, so it’s never prompted again."
  (push package pbutler-blacklist)
  ;; TODO does this work?
  (custom-set-variables `(pbutler-blacklist ,pbutler-blacklist)))

(defun pbutler-in-blacklist (package)
  "Return t if PACKAGE is in the blacklist."
  (memq package pbutler-blacklist))

(defun pbutler-potential-major-mode-package (file-name)
  "Return a list of potentially useful packages for FILE-NAME."
  (let ((package
         (cl-loop for elt in pbutler-major-mode-recommendation-list
                  if (cl-loop for re in (car elt)
                              if (string-match re file-name)
                              return t)
                  return (cdr elt))))
    (when (and (not (package-installed-p package))
               (not (pbutler-in-blacklist package)))
      package)))

;;; Prompt

(defun pbulter-prompt-recommendation ()
  "Prompt user to install recommended packages."
  (when-let ((major-mode-package
              (pbutler-potential-major-mode-package buffer-file-name)))
    (pcase (read-char "Major mode package %s available, do you want to install? [y]es/[S]kip for now/[n]ever prompt me again")
      (?y (install-package major-mode-package))
      (?n (pbutler-add-to-blacklist major-mode-package)))))


(provide 'pbutler)

;;; pbutler.el ends here

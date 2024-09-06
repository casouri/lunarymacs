;;; query-builder-tests.el --- Tests for query-builder  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'query-builder)

(ert-deftest query-builder-ui-state ()
  "Test UI state utilities."
  (should (equal (query-builder--get-state '("a" "b") 'expanded) nil))
  (query-builder--set-state '("a" "b") 'expanded 1)
  (should (equal (query-builder--get-state '("a" "b") 'expanded) 1))
  (query-builder--set-state '("a" "b") 'expanded 2)
  (should (equal (query-builder--get-state '("a" "b") 'expanded) 2))
  (query-builder--set-state '("a" "c") 'expanded 3)
  (should (equal (query-builder--get-state '("a" "c") 'expanded) 3)))

(ert-deftest query-builder-alist-get ()
  "Test ‘query-builder-alist-get’."
  (should (equal (query-builder--alist-get '(a b c)
                                           '((a . ((b . ((c . 1)))))))
                 1)))

;;; query-builder-tests.el ends here

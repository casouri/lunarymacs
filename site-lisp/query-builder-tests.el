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
                 1))
  (should (equal (query-builder--alist-get nil '((a . ((b . ((c . 1)))))))
                 '((a . ((b . ((c . 1)))))))))

(ert-deftest query-builder-build-queriy-object ()
  "Test for building query."
  (let ((field-paths '(("alerts")
                       ("alerts" "alerts")
                       ("alertName" "alerts" "alerts")
                       ("books")
                       ("author" "books")
                       ("feed")
                       ("feed" "feed")
                       ("chart" "feed")
                       ("types" "chart" "feed")
                       ("events" "feed" "feed")
                       ("uuid" "events" "feed" "feed"))))
    (should (equal (query-builder--construct-query-object field-paths nil)
                   '(("alerts" ("alerts" ("alertName")))
                     ("books" ("author"))
                     ("feed"
                      ("feed" ("events" ("uuid")))
                      ("chart" ("types"))))))))

(ert-deftest query-builder-serialize ()
  "Test for query serializer."
  (should (equal (query-builder--serialize-query-object
                  '(("alerts" ("alerts" ("alertName")))
                    ("books" ("author"))
                    ("feed"
                     ("feed" ("events" ("uuid")))
                     ("chart" ("types")))))
                 "alerts { alerts { alertName } } books { author } feed { feed { events { uuid } } chart { types } }"))
  (should (equal (query-builder--serialize-query-object
                  '(("alerts" ("alerts" ("alertName")))
                    ("books" ("author"))
                    ("feed"
                     ("feed" ("events" ("uuid")))
                     ("chart" ("types"))))
                  0)
                 "alerts {
  alerts {
    alertName
  }
}
books {
  author
}
feed {
  feed {
    events {
      uuid
    }
  }
  chart {
    types
  }
}
")))

;;; query-builder-tests.el ends here

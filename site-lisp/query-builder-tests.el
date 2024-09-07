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
    (should (equal (query-builder--construct-query-object field-paths nil nil)
                   '(( :name "alerts"
                       :fields (( :name "alerts"
                                  :fields (( :name "alertName"
                                             :fields nil
                                             :args nil))
                                  :args nil))
                       :args nil)
                     ( :name "books"
                       :fields (( :name "author"
                                  :fields nil
                                  :args nil))
                       :args nil)
                     ( :name "feed"
                       :fields (( :name "feed"
                                  :fields (( :name "events"
                                             :fields (( :name "uuid"
                                                        :fields nil
                                                        :args nil))
                                             :args nil))
                                  :args nil)
                                ( :name "chart"
                                  :fields (( :name "types"
                                             :fields nil
                                             :args nil))
                                  :args nil))
                       :args nil))))))

(ert-deftest query-builder-serialize ()
  "Test for query serializer."
  (let ((query-object '(( :name "alerts"
                          :fields (( :name "alerts"
                                     :fields (( :name "alertName"
                                                :fields nil
                                                :args nil))
                                     :args nil))
                          :args nil)
                        ( :name "books"
                          :fields (( :name "author"
                                     :fields nil
                                     :args nil))
                          :args nil)
                        ( :name "feed"
                          :fields (( :name "feed"
                                     :fields (( :name "events"
                                                :fields (( :name "uuid"
                                                           :fields nil
                                                           :args nil))
                                                :args nil))
                                     :args nil)
                                   ( :name "chart"
                                     :fields (( :name "types"
                                                :fields nil
                                                :args nil))
                                     :args nil))
                          :args nil))))
    (should (equal (query-builder--serialize-query-object query-object)
                   "alerts { alerts { alertName } } books { author } feed { feed { events { uuid } } chart { types } }"))


    (should (equal (query-builder--serialize-query-object query-object 0)
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
"))))

(ert-deftest query-builder--construct-query-object-with-args ()
  "Test constructing query object with args mixed in."
  (let ((field-paths '(("feed") ("feed" "feed") ("offset" "feed" "feed")))
        (args '(( :path ("filter" "feed" "feed")
                  :arg-val nil)
                ( :path ("crqNumber" "filter" "feed" "feed")
                  :arg-val "aaabbb")
                ( :path ("crqStatus" "filter" "feed" "feed")
                  :arg-val "bbbccc"))))
    (should (equal (query-builder--construct-query-object
                    field-paths arg-values nil)
                   '(( :name "feed"
                       :fields (( :name "feed"
                                  :fields (( :name "offset"
                                             :fields nil
                                             :args nil))
                                  :args (( :name "filter"
                                           :fields (( :name "crqNumber"
                                                      :val "aaabbb")
                                                    ( :name "crqStatus"
                                                      :val "bbbccc"))))))
                       :args nil))))))

(ert-deftest query-builder--serialize-arg ()
  "Test serializing arg object."
  (should (equal (query-builder--serialize-arg-object
                  '( :name "filter"
                     :fields (( :name "crqNumber"
                                :val "aaabbb")
                              ( :name "crqStatus"
                                :val "bbbccc"))))
                 "filter: { crqNumber: \"aaabbb\", crqStatus: \"bbbccc\" }")))

(ert-deftest query-builder--serialize-query-with-args ()
  "Test serializing queries with args."
  (should (equal (query-builder--serialize-query-object
                  '( :name "filter"
                     :fields (( :name "crqNumber"
                                :val "aaabbb")
                              ( :name "crqStatus"
                                :val "bbbccc")))
                  0)
                 "feed {
  feed(filter: { crqNumber: \"aaabbb\", crqStatus: \"bbbccc\" }) {
    offset
  }
}
")))

;;; query-builder-tests.el ends here

;;; -*- lexical-binding: t -*-

(defvar moon-format-on-save-func-book '(python-mode moon/toggle-python-format-on-save javascript-mode moon/toggle-js-format-on-save)
  "Function to toggle format on save for each major mode.
Each function should toggle format-on-save base on `moon-format-on-save'.")

(defvar moon-format-on-save nil
  "Whether to format on save.")

(defun strip-text-properties(text)
  "Return TEXT without any properties."
(set-text-properties 0 (length text) nil text)
    text)

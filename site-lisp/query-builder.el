;;; query-builder.el --- Query builder for GraphQL  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'plz)
(require 'url-parse)

(defgroup query-builder nil
  "GraphQL query builder.")

(defface query-builder-field-name-face
  (let ((display t)
        (atts '(:family "SF Pro Text" :height 1.2)))
    `((,display . ,atts)))
  "Face for the name of each field in a query.")

(defface query-builder-field-type-face
  (let ((display t)
        (atts '(:inherit shadow)))
    `((,display . ,atts)))
  "Face for the type shown after each field.")

(defvar query-builder-query-and-mutation-query
  "

query QueryAndMutation {
    __schema {
        queryType {
            name
        }
        mutationType {
            name
        }
        types {
            name
            fields {
                name
                type {
                    name
                    kind
                    ofType {
                        name
                        kind
                        ofType {
                            name
                            kind
                            ofType {
                                name
                                kind
                            }
                        }
                    }
                }
            }
        }
    }
}
"
  "Query we use to get all the queries and mutations.")

;;;; UI state

(defvar-local query-builder--ui-state '()
  "Expansion state and other states for each query field in the current buffer.
The value is a alist, where each key is the path to the field,
like (\"fieldB\" \"fieldA\"), and the value is another alist mapping state keys
to state values.

In terms of JSON, (fieldA fieldB) corresponds to

{
  \"fieldA\": {
    \"fieldB\": ...
  }
}")

(defsubst query-builder--get-state (field-path key)
  "Get the value for the GraphQL field at FIELD-PATH.
Each field has many states, KEY specifies the particular state we want."
  (alist-get key (alist-get field-path query-builder--ui-state
                            nil nil #'equal)))

(defsubst query-builder--set-state (field-path key val)
  "Set state pair KEY VAL for the GraphQL specified by FIELD-PATH."
  (setf (alist-get key (alist-get field-path query-builder--ui-state
                                  nil t #'equal))
        val))

(defsubst query-builder--get-state-at-point (key)
  "Return the UI state for KEY of the field at point.
Return nil if no state exists."
  (when-let ((field-path (get-text-property (point) 'query-builder-field-path)))
    (query-builder--get-state field-path key)))

(defsubst query-builder--set-state-at-point (key val)
  "Set the UI state for KEY to VAL for the field at point."
  (when-let ((field-path (get-text-property (point) 'query-builder-field-path)))
    (query-builder--set-state field-path key val)))

;;;; Utilities

(defsubst query-builder--alist-get (chain alist)
  "Chained ‘alist-get’ call on ALIST.
CHAIN should be a list of symbols like (KEY1 KEY2 KEY 3).
Return (alist-get KEY3 (alist-get KEY2 (alist-get KEY1 alist)))."
  (dolist (key chain alist)
    (setq alist (alist-get key alist))))

;;;; Retreiving and inspecting schema
;;
;; FIELD := (Field FIELD-NAME FIELD-TYPE)
;; FIELD-NAME := <string>
;; FIELD-TYPE := (Type <string>) | (List FIELD-TYPE) | (Non-null FIELD-TYPE)
;;
;; Notes:
;;
;; - Queries and mutations are also fields. Queries are fields of a
;;   special type "Query". (By convention it’s "Query", but schema can
;;   choose another value by assining the queryType field), likewise
;;   for mutations.
;;
;; - If FIELD-TYPE is a (Type <string>), it’s either a named type, a
;;   scalar, an enum, or a union.

(defvar-local query-builder--schema nil
  "The schema object.")

(defvar-local query-builder--endpoint nil
  "The endpoint URL.")

(defun query-builder--decode-type (type)
  "Decode TYPE into our internal structure FIELD-TYPE.

See comments in the source file for the definition of FIELD-TYPE.
TYPE is a JSON object from the schema."
  (pcase (alist-get 'kind type)
    ("LIST" `(List ,(query-builder--decode-type
                     (alist-get 'ofType type))))
    ("NON_NULL" `(Non-null ,(query-builder--decode-type
                             (alist-get 'ofType type))))
    ((or "OBJECT" "SCALAR" "ENUM" "UNION") (alist-get 'name type))
    (kind (signal 'query-builder-schema-error
                  (list "Unexpected kind of a type" kind type)))))

(defun query-builder--get-schema (url)
  "Reuturn the schema at URL as a JSON object."
  (plz 'post url
    :headers `(("Accept" . "*/*")
               ("Content-Type" . "application/json")
               ("Host" . ,(url-host (url-generic-parse-url url))))
    :body (json-serialize
           `(:query ,query-builder-query-and-mutation-query))
    :as #'json-read))

(defun query-builder--get-all-queries (schema)
  "Get the list of queries in SCHEMA.

SCHEMA is a JSON object returned from ‘queery-builder--get-schema’.
Return each query in the form of (Field FIELD-NAME FIELD-TYPE)."
  (let ((query-type-name
         (query-builder--alist-get '(data __schema queryType name) schema)))
    (query-builder--get-fields-for-type schema query-type-name)))

(defun query-builder--get-fields-for-type (schema type-name)
  "Get the list of fields for TYPE-NAME in SCHEMA.

SCHEMA is a JSON object returned from ‘queery-builder--get-schema’.
Return each field in the form of (Field FIELD-NAME FIELD-TYPE)."
  (let* ((type-obj
          (seq-find (lambda (type)
                      (equal (query-builder--alist-get '(name) type)
                             type-name))
                    (query-builder--alist-get '(data __schema types) schema)))
         (fields (query-builder--alist-get '(fields) type-obj)))
    (seq-map
     (lambda (field)
       (let ((name (query-builder--alist-get '(name) field))
             (type (query-builder--decode-type
                    (query-builder--alist-get '(type) field))))
         `(Field ,name ,type)))
     fields)))

;;;; UI: drawing UI, toggling fields

(defvar query-builder-field-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'query-builder-toggle-field)
    map)
  "Local keymap for when point is on a field.")

;; (define-button-type 'query-builder-field-button
;;   'action #'query-builder-toggle-field
;;   'follow-link t
;;   'face nil)

(defun query-builder--render-type (type &optional base)
  "Return a string that represents TYPE.

TYPE has the shape of FIELD-TYPE. List types are rendered as [TYPE],
Non-null types are rendered as TYPE!. If BASE is non-nil, don’t add the
[] and ! to the type name."
  (pcase type
    (`(List ,inner-type)
     (if base
         (query-builder--render-type inner-type base)
       (format "[%s]" (query-builder--render-type inner-type base))))
    (`(Non-null ,inner-type)
     (if base
         (query-builder--render-type inner-type base)
       (format "%s!" (query-builder--render-type inner-type base))))
    ((pred stringp) type)
    (_ (signal 'query-builder-render-error
               (list "Unexpect shape for a FIELD-TYPE" type)))))

(defun query-builder--insert-fields (fields indent-level parent-field-path)
  "Insert FIELDS at point.
Each field in FIELDS should be for the form

    (Field FIELD-NAME FIELD-TYPE)

INDENT-LEVEL is the nesting level of the fields. PARENT-FIELD-PATH is the
field path to the parent of fields. It’s used for constructing the field
path of each field in FIELDS. Specifically, each fields field path
is (cons FIELD-NAME PARENT-FILED-PATH)."
  (pcase-dolist (`(Field ,name ,type) fields)
    (insert (propertize
             (concat
              (make-string (* 2 indent-level) ?\s)
              "* "
              (propertize (or name "N/A") 'face 'query-builder-field-name-face)
              " "
              (propertize (query-builder--render-type (or type "N/A"))
                          'face 'query-builder-field-type-face))
             'query-builder-field-path (cons name parent-field-path)
             'query-builder-field-name name
             'query-builder-field-type type
             'query-builder-indent-level indent-level
             'keymap query-builder-field-map)
            "\n")))

(defun query-builder--remove-fields-after-point (indent-level)
  "Remove fields after point that has an indent-level higher than INDENT-LEVEL.

Remove fields starting from the next line, regardless of whether the
current line satisfies the requirement."
  (let* ((beg (pos-bol 2))
         (match (text-property-search-forward
                 'query-builder-indent-level indent-level
                 (lambda (indent-level level-at-point)
                   (and level-at-point
                        (<= level-at-point indent-level)))
                 t))
         (end (if match (prop-match-beginning match) (point-max))))
    (delete-region beg end)))

(defun query-builder-toggle-field (&rest _args)
  "Toggle expanding the field at point."
  (interactive)
  (let ((inhibit-read-only t)
        (orig-point (point)))
    (let ((expanded (query-builder--get-state-at-point 'expanded))
          (indent-level (get-text-property (point) 'query-builder-indent-level))
          (field-type (query-builder--render-type
                       (get-text-property (point) 'query-builder-field-type)
                       t))
          (field-path (get-text-property (point) 'query-builder-field-path)))
      (if expanded
          (progn
            (query-builder--set-state-at-point 'expanded nil)
            (when indent-level
              (query-builder--remove-fields-after-point indent-level)))
        (query-builder--set-state-at-point 'expanded t)
        (when (and indent-level field-path field-type query-builder--schema)
          (let ((fields (query-builder--get-fields-for-type
                         query-builder--schema field-type)))
            (forward-line 1)
            (query-builder--insert-fields
             fields (1+ indent-level) field-path)))))
    (goto-char orig-point)))


;;;; Major mode

(defvar query-builder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'query-builder-refresh)
    map)
  "Mode map for ‘query-builder-mode’.")

(define-derived-mode query-builder-mode special-mode "QueryBuilder"
  "Major mode for building GraphQL queries."
  :keymap query-builder-mode-map
  (font-lock-mode -1))

(defun query-builder (url)
  "Start or resume a query builder session with GraphQL endpoint at URL."
  (interactive "sEndpoint: ")
  (switch-to-buffer
   (get-buffer-create (format "<query builder for %s>" url)))
  (query-builder-mode)
  (unless query-builder--schema
    (setq query-builder--schema (query-builder--get-schema url)))
  (setq query-builder--endpoint url)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (save-excursion
      (if (null query-builder--schema)
          (insert "Can’t retrieve schema from " url "\n")
        (query-builder--insert-fields
         (query-builder--get-all-queries query-builder--schema) 0 nil)))))

(defun query-builder-refresh ()
  "Refresh schema and buffer."
  (interactive)
  (setq query-builder--schema
        (query-builder--get-schema query-builder--endpoint))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (save-excursion
      (if (null query-builder--schema)
          (insert "Can’t retrieve schema from " url "\n")
        (query-builder--insert-fields
         (query-builder--get-all-queries query-builder--schema) 0 nil)))))

(provide 'query-builder)

;;; query-builder.el ends here

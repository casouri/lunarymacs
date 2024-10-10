;;; query-builder.el --- Query builder for GraphQL  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; GraphQL reference: https://spec.graphql.org/October2021/#sec-Introspection
;;
;; TODO:
;; 1. Add support union

;;; Code:

(require 'plz)
(require 'url-parse)
(require 'rx)

(defgroup gql-builder nil
  "GraphQL query builder.")

(defface gql-builder-field-name
  (let ((display t)
        (atts nil))
    `((,display . ,atts)))
  "Face for the name of each field in a query.")

(defface gql-builder-marked-field-name
  (let ((display t)
        (atts '(:inherit gql-builder-field-name :weight bold)))
    `((,display . ,atts)))
  "Face for the name of a marked field in a query.")

(defface gql-builder-field-type
  (let ((display t)
        (atts '(:inherit shadow)))
    `((,display . ,atts)))
  "Face for the type shown after each field.")

(defface gql-builder-arg-marker
  (let ((display t)
        (atts '(:inherit warning)))
    `((,display . ,atts)))
  "Face for the ARG marker in front of a arg for a field.")

(defvar gql-builder-marker-marked "[*] "
  "A string used to mark a marked field.")

(defvar gql-builder-marker-unmarked "[ ] "
  "A string used to mark an unmarded field.")

(defvar gql-builder-query-and-mutation-query
  "query QueryAndMutation {
    __schema {
        queryType {
            name
        }
        mutationType {
            name
        }
        types {
            name
            inputFields {
                name
                type {
                    ...TypeRef
                }
            }
            fields(includeDeprecated: true) {
                args {
                    name
                    type {
                        name
                        kind
                        ...TypeRef
                    }
                }
                name
                isDeprecated
                type {
                    name
                    kind
                    ofType {
                        name
                        kind
                        ...TypeRef
                    }
                }
            }
            interfaces {
                ...TypeRef
            }
            enumValues(includeDeprecated: true) {
                name
                description
                isDeprecated
                deprecationReason
            }
            possibleTypes {
                ...TypeRef
            }
        }
    }
}
fragment TypeRef on __Type {
    kind
    name
    ofType {
        kind
        name
        ofType {
            kind
            name
            ofType {
                kind
                name
            }
        }
    }
}"
  "Query we use to get all the queries, mutations, and types.")

;;;; UI state

(defvar-local gql-builder--ui-state '()
  "Expansion state and other states for each query field in the current buffer.
The value is a alist, where each key is the path to the field,
like (\"fieldB\" \"fieldA\"), and the value is another alist mapping state keys
to state values.

In terms of JSON, (\"fieldB\" \"fieldA\") corresponds to

{
  \"fieldA\": {
    \"fieldB\": <- This field.
  }
}

Right now we have the following state keys:
- expanded: Whether the field is expanded.
- arg-expanded: Whether the arg is expanded.
- marked: Whether the field is marked.
- arg-marked: Whether the arg is marked.
- arv-val: Arg value for the arg.

I’m not sure if fields and args can have the same name without conflict
in GraphQL; just to be safe, I used different keys for fields and args.")

(defsubst gql-builder--get-state (field-path key)
  "Get the value for the GraphQL field at FIELD-PATH.
Each field has many states, KEY specifies the particular state we want."
  (alist-get key (alist-get field-path gql-builder--ui-state
                            nil nil #'equal)))

(defsubst gql-builder--set-state (field-path key val)
  "Set state pair KEY VAL for the GraphQL specified by FIELD-PATH."
  (setf (alist-get key (alist-get field-path gql-builder--ui-state
                                  nil t #'equal))
        val))

(defsubst gql-builder--get-state-at-point (key)
  "Return the UI state for KEY of the field at point.
Return nil if no state exists."
  (when-let ((field-path (get-text-property (point) 'gql-builder-field-path)))
    (gql-builder--get-state field-path key)))

(defsubst gql-builder--set-state-at-point (key val)
  "Set the UI state for KEY to VAL for the field at point."
  (when-let ((field-path (get-text-property (point) 'gql-builder-field-path)))
    (gql-builder--set-state field-path key val)))

;;;; Utilities

(defsubst gql-builder--alist-get (chain alist)
  "Chained ‘alist-get’ call on ALIST.

CHAIN should be a list of symbols like (KEY1 KEY2 KEY 3).
Return (alist-get KEY3 (alist-get KEY2 (alist-get KEY1 alist))). If
chain is nil, return ALIST."
  (dolist (key chain alist)
    (setq alist (alist-get key alist))))

;;;; Retreiving and inspecting schema
;;
;; FIELD := (Field FIELD-NAME FIELD-TYPE ARGS)
;; FIELD-NAME := <string>
;; FIELD-TYPE := (Type <string>) | (List FIELD-TYPE) | (Non-null FIELD-TYPE)
;; ARGS := [INPUT-FIELD]
;; INPUT-FIELD := (InputField FIELD-NAME FIELD-TYPE)
;;
;; Notes:
;;
;; - Queries and mutations are also fields. Queries are fields of a
;;   special type "Query". (By convention it’s "Query", but schema can
;;   choose another value by assining the queryType field), likewise
;;   for mutations.
;;
;; - If FIELD-TYPE is a (Type <string>), it’s either a named type, a
;;   scalar, an enum, a union, or an interface.

(defvar gql-builder--schema-cache nil
  "An alist mapping GraqphQL endpoint URL to schema JSON object.")

(defvar-local gql-builder--schema nil
  "The schema object.")

(defvar-local gql-builder--endpoint nil
  "The endpoint URL.")

(defvar-local gql-builder--initial-body nil
  "The initial GraphQL request body the builder started with.")

(defvar-local gql-builder--restclient-state nil
  "The states of restclient request that initiated this query builder.
The value should be an alist with the following keys:

  - ‘body’: The request body.
  - ‘buffer’: The restclient buffer
  - ‘point’: The point.")

(defvar-local gql-builder--orig-window-config nil
  "The window configuration before we popped the query builder buffer.")

(defun gql-builder--decode-type (type)
  "Decode TYPE into our internal structure FIELD-TYPE.

See comments in the source file for the definition of FIELD-TYPE.
TYPE is a JSON object from the schema."
  (pcase (alist-get 'kind type)
    ("LIST" `(List ,(gql-builder--decode-type
                     (alist-get 'ofType type))))
    ("NON_NULL" `(Non-null ,(gql-builder--decode-type
                             (alist-get 'ofType type))))
    ((or "OBJECT" "INPUT_OBJECT" "SCALAR" "ENUM" "UNION" "INTERFACE")
     (alist-get 'name type))
    (kind (signal 'gql-builder-schema-error
                  (list "Unexpected kind of a type" kind type)))))

(defun gql-builder--check-type (type target)
  "Return t if TYPE is a TARGET type.
TARGET can be \"String\", \"Int\", or \"Boolean\"."
  (pcase type
    ((pred stringp) (equal type target))
    (`(List ,inner-type) (gql-builder--type-string-p inner-type))
    (`(Non-null ,inner-type) (gql-builder--type-string-p inner-type))))

(defun gql-builder--get-schema (url &optional headers new)
  "Reuturn the schema at URL as a JSON object.

If HEADERS is non-nil, add those headers. It should be an alist that
looks like ((\"Content-Type\" . \"application/json\")).

If NEW is non-nil, skip the schema cache and always get from remote."
  (dolist (header `(("Accept" . "*/*")
                    ("Content-Type" . "application/json")
                    ("Host" . ,(url-host (url-generic-parse-url url)))))
    (unless (alist-get (car header) headers nil nil #'equal)
      (push header headers)))

  (or (and (not new)
           (alist-get url gql-builder--schema-cache nil t #'equal))
      (let ((schema
             (plz 'post url
               :headers headers
               :body (json-serialize
                      `(:query ,gql-builder-query-and-mutation-query))
               :as (lambda ()
                     (json-parse-buffer
                      :object-type 'alist :null-object nil)))))
        (setf (alist-get url gql-builder--schema-cache nil t #'equal)
              schema)
        schema)))

(defun gql-builder--get-all-queries (schema)
  "Get the list of queries in SCHEMA.

SCHEMA is a JSON object returned from ‘queery-builder--get-schema’.
Return each query in the form of (Field FIELD-NAME FIELD-TYPE ARGS)."
  (let ((query-type-name
         (gql-builder--alist-get '(data __schema queryType name) schema)))
    (gql-builder--get-fields-for-type schema query-type-name)))

(defun gql-builder--make-field (field)
  "Create a (Field FIELD-NAME FIELD-TYPE ARGS) from FIELD.
FIELD is an alist with ‘name’, ‘type’ as its keys."
  (let* ((name (gql-builder--alist-get '(name) field))
         (type (gql-builder--decode-type
                (gql-builder--alist-get '(type) field)))
         (args (gql-builder--alist-get '(args) field)))
    `(Field ,name ,type ,(when args
                           (mapcar #'gql-builder--make-field args)))))

(defun gql-builder--make-possible-type-field (possible-type)
  "Create a (Field FIELD-NAME FIELD-TYPE ARGS) from POSSIBLE-TYPE.
FIELD is an alist with ‘name’, ‘kind’ as its keys."
  (let* ((name (gql-builder--alist-get '(name) possible-type)))
    `(Field ,(format "... on %s" name) ,name)))

(defun gql-builder--get-fields-for-type
    (schema type-name &optional input-fields)
  "Get the list of fields for TYPE-NAME in SCHEMA.

SCHEMA is a JSON object returned from ‘queery-builder--get-schema’.
Return each field in the form of (Field FIELD-NAME FIELD-TYPE).

If input-fields is non-nil, get input fields instead."
  (let* ((type-obj
          (seq-find (lambda (type)
                      (equal (gql-builder--alist-get '(name) type)
                             type-name))
                    (gql-builder--alist-get '(data __schema types) schema)))
         (fields (gql-builder--alist-get (if input-fields '(inputFields)
                                           '(fields))
                                         type-obj))
         (possible-types
          (and (not input-fields)
               (gql-builder--alist-get '(possibleTypes) type-obj))))
    (cond
     (fields
      (seq-map #'gql-builder--make-field fields))
     ((and (null input-fields) possible-types)
      (seq-map #'gql-builder--make-possible-type-field possible-types)))))

;;;; Building query

(defun gql-builder--get-all-marked-field-paths (ui-state)
  "Get all the field paths that are marked in UI-STATE.

Return a list of field paths, each field path is like (FIELD-NAME ...),
eg, (\"A\" \"B\")."
  (let (results)
    (pcase-dolist (`(,field-path . ,states) ui-state)
      (when (alist-get 'marked states)
        (push field-path results)))
    results))

(defun gql-builder--get-all-marked-arg-values (ui-state)
  "Get all the arg values with their field path that are marked in UI-STATE.
Return a list of (:path FIELD-PATH :arg-val VALUE). Value of :arg-val
could be nil if the field path is of an input that is an input object."
  (let (results)
    (pcase-dolist (`(,field-path . ,states) ui-state)
      (when (alist-get 'arg-marked states)
        (push `(:path ,field-path :arg-val ,(alist-get 'arg-val states))
              results)))
    results))

(defun gql-builder--construct-query-object (field-paths args root)
  "Build a JSON object rooted ar ROOT from FIELD-PATHS.

ROOT is a field-path, we want to construct a subgraph rooted at the
field that ROOT represents.

ARGS is a list of (:path FIELD-PATH :arg-val VAL).

FIELD-PATHS: (\"A\") (\"B\" \"A\") (\"C\" \"A\") (\"D\")
Return: JSON object that encodes { A: { B: nil, C: nil }, D: nil }

Reuturn a plist (:name FIELD-NAME :fields FIELDS-OF-FIELD :args
ARGS-OF-FIELD), where FIELDS-OF-FIELD is a list of the above plist.
ARGS-OF-FIELD is a list of (:name ARG-NAME :fields FIELDS-OF-ARG),
or (:name ARG-NAME :val ARG-VAL) if the arg is a leaf arg."
  ;; TODO: Stratify FIELD-PATHS by length.
  (let* ((root-len (if root (length root) 0))
         (field-name (car root))
         (immediate-children
          (if (eq root-len 0)
              (seq-filter (lambda (field-path) (eq (length field-path) 1))
                          field-paths)
            (seq-filter (lambda (field-path)
                          (equal (cdr field-path) root))
                        field-paths)))
         ;; If there’s no immediate children, SUBGRAPHS would be nil,
         ;; if there are, SUBGRAPHS will be an alist, where each
         ;; immediate children is the key, and their subgraph are the
         ;; values.
         (subgraphs (mapcar (lambda (child)
                              (gql-builder--construct-query-object
                               field-paths args child))
                            immediate-children))
         ;; Does this field has args? If it does, construct the arg
         ;; object.
         (args-of-this-field (seq-filter (lambda (arg)
                                           (equal (cdr (plist-get arg :path))
                                                  root))
                                         args))
         (arg-objects (mapcar (lambda (arg)
                                (gql-builder--construct-args-object
                                 args arg))
                              args-of-this-field)))
    (if field-name
        (list :name field-name :fields subgraphs :args arg-objects)
      subgraphs)))

(defun gql-builder--construct-args-object (args root)
  "Construct a JSON object rooted at ROOT from ARGS.

ROOT has the form (:path FIELD-PATH :arg-val VAL).

Similar to ‘gql-builder--construct-query-object’, this function builds
a subgraph for ROOT, but for args.

Return (:name ARG-NAME :fields SUBGRAPH), where SUBGRAPH is an alist
of (:name ARG-NAME :fields SUBGRAPH), or for leaf args, (:name ARG-NAME
:val ARG-VAL)."
  (let* ((root-path (plist-get root :path))
         (root-len (length root-path))
         (root-arg-name (car root-path))
         (immediate-children
          ;; ARG = (:path FIELD-PATH :arg-val VAL)
          (seq-filter (lambda (arg)
                        (equal (cdr (plist-get arg :path)) root-path))
                      args))
         ;; CHILD = (:path FIELD-PATH :arg-val VAL)
         (subgraphs (mapcar (lambda (child)
                              (gql-builder--construct-args-object
                               args child))
                            immediate-children)))
    (if subgraphs
        (list :name root-arg-name :fields subgraphs)
      (list :name root-arg-name :val (plist-get root :arg-val)))))

(defun gql-builder--serialize-query-object (query-object &optional indent)
  "Serialize QUERY-OBJECT to a GraphQL query string.

If INDENT is non-nil, it should be the indent level, a number, and this
function will pretty print the query."
  (let ((indent-string (if indent (make-string (* 2 indent) ?\s) nil)))
    (string-join
     (mapcar
      (lambda (child)
        (let* ((name (plist-get child :name))
               (sub-fields (plist-get child :fields))
               (args (plist-get child :args))
               (serialized-args
                (if args
                    (concat
                     "("
                     (string-join
                      (mapcar #'gql-builder--serialize-arg-object args)
                      ", ")
                     ")")
                  "")))
          (if sub-fields
              (if indent-string
                  (concat indent-string name serialized-args " {\n"
                          (gql-builder--serialize-query-object
                           sub-fields (and indent (1+ indent)))
                          indent-string "}\n")
                (format "%s%s { %s }"
                        name
                        serialized-args
                        (gql-builder--serialize-query-object
                         sub-fields)))
            (if indent-string
                (concat indent-string name "\n")
              name))))
      query-object)
     (if indent nil " "))))

(defun gql-builder--serialize-arg-object (arg-object)
  "Serialize ARG-OBJECT to a GraphQL arg.

ARG-OBJECT should has the form of (:name ARG-NAME :fields ARG-FIELDS)
or (:name ARG-NAME :val ARG-VAL). ARG-FIELDS has the same form as
ARG-OBJECT.

Return a string that looks like “NAME: VAL”."
  (let* ((fields (plist-get arg-object :fields))
         (serialized-fields (when fields
                              (concat "{ "
                                      (string-join
                                       (mapcar #'gql-builder--serialize-arg-object
                                               fields)
                                       ", ")
                                      " }")))
         (val (plist-get arg-object :val)))
    (format "%s: %s" (plist-get arg-object :name)
            (or serialized-fields val "null"))))

;;;; UI: drawing UI, toggling fields

(defvar gql-builder-field-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'gql-builder-toggle-expanded)
    (define-key map (kbd "m") #'gql-builder-mark)
    (define-key map (kbd "u") #'gql-builder-unmark)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Local keymap for when point is on a field.")

;; (define-button-type 'gql-builder-field-button
;;   'action #'gql-builder-toggle-field
;;   'follow-link t
;;   'face nil)

(defun gql-builder--redraw-top-level-field (field-name)
  "Redraw the section of the top-level field with FIELD-NAME."
  (let ((orig-pos (point))
        (schema gql-builder--schema)
        (inhibit-read-only t))
    (goto-char (point-min))
    (when-let ((match (text-property-search-forward
                       'gql-builder-field-path (list field-name) #'equal )))
      (goto-char (prop-match-beginning match))
      ;; Remove child fields.
      (gql-builder--remove-fields-after-point 0)
      ;; Remove the field itself.
      (goto-char (prop-match-beginning match))
      (let ((beg (pos-bol)))
        (forward-line 1)
        (delete-region beg (point)))
      (gql-builder--insert-fields
       (list (seq-find (lambda (field)
                         ;; Field is (Field FIELD-NAME FIELD-TYPE ARGS).
                         (equal (nth 1 field) field-name))
                       (gql-builder--get-all-queries schema)))
       0 nil)
      (goto-char orig-pos))))

(defun gql-builder--render-type (type &optional base)
  "Return a string that represents TYPE.

TYPE has the shape of FIELD-TYPE. List types are rendered as [TYPE],
Non-null types are rendered as TYPE!. If BASE is non-nil, don’t add the
[] and ! to the type name."
  (pcase type
    (`(List ,inner-type)
     (if base
         (gql-builder--render-type inner-type base)
       (format "[%s]" (gql-builder--render-type inner-type base))))
    (`(Non-null ,inner-type)
     (if base
         (gql-builder--render-type inner-type base)
       (format "%s!" (gql-builder--render-type inner-type base))))
    ((pred stringp) type)
    (_ (signal 'gql-builder-render-error
               (list "Unexpect shape for a FIELD-TYPE" type)))))

(defun gql-builder--sort-by-marked (fields parent-field-path)
  "Sort FIELDS by putting marked ones in the front.
FIELDS and PARENT-FIELD-PATH are the same as in
‘gql-builder--insert-fields’."
  (seq-sort (lambda (a b)
              (let ((a-marked (gql-builder--get-state
                               (cons (nth 1 a) parent-field-path) 'marked))
                    (b-marked (gql-builder--get-state
                               (cons (nth 1 b) parent-field-path) 'marked)))
                (cond
                 ((and a-marked (not b-marked)) t)
                 ((and b-marked (not a-marked)) nil)
                 (t (string< (nth 1 a) (nth 1 b))))))
            fields))

(defun gql-builder--insert-fields
    (fields indent-level parent-field-path &optional arg-p)
  "Insert FIELDS at point.
Each field in FIELDS should be for the form

    (Field FIELD-NAME FIELD-TYPE ARGS)

INDENT-LEVEL is the nesting level of the fields. PARENT-FIELD-PATH is the
field path to the parent of fields. It’s used for constructing the field
path of each field in FIELDS. Specifically, each fields field path
is (cons FIELD-NAME PARENT-FIELED-PATH).

If ARG-P is non-nil, FIELDS are actually args, insert ARG marker in
front of each field."
  (pcase-dolist (`(Field ,name ,type ,args)
                 (gql-builder--sort-by-marked fields parent-field-path))
    (let* ((field-path (cons name parent-field-path))
           (marked (if arg-p
                       (gql-builder--get-state field-path 'arg-marked)
                     (gql-builder--get-state field-path 'marked)))
           (arg-val (and arg-p
                         (gql-builder--get-state field-path 'arg-val))))
      (insert (propertize
               (concat
                (make-string (* 2 indent-level) ?\s)
                (if marked
                    gql-builder-marker-marked
                  gql-builder-marker-unmarked)
                (if arg-p
                    (propertize "ARG " 'face 'gql-builder-arg-marker)
                  "")
                (propertize (or name "N/A")
                            'face (if marked
                                      'gql-builder-marked-field-name
                                    'gql-builder-field-name))
                " "
                (if (string-match-p (rx bol "...") name)
                    ""
                  (propertize (gql-builder--render-type (or type "N/A"))
                              'face 'gql-builder-field-type))
                (if arg-val (gql-builder--format-arg-val arg-val) ""))
               'gql-builder-field-path field-path
               'gql-builder-field-name name
               'gql-builder-field-type type
               'gql-builder-indent-level indent-level
               'gql-builder-arg-p arg-p
               'gql-builder-args args
               'keymap gql-builder-field-map)
              "\n")
      ;; Insert subfields if this field is expanded.
      (when (and gql-builder--schema
                 (gql-builder--get-state field-path
                                         (if arg-p 'arg-expanded 'expanded)))
        ;; Insert args.
        (gql-builder--insert-fields args (1+ indent-level) field-path t)
        ;; Insert subfields.
        (gql-builder--insert-fields
         (gql-builder--get-fields-for-type
          gql-builder--schema
          (gql-builder--render-type type t) arg-p)
         (1+ indent-level)
         field-path arg-p)))))

(defun gql-builder--remove-fields-after-point (indent-level)
  "Remove fields after point that has an indent-level higher than INDENT-LEVEL.

Remove fields starting from the next line, regardless of whether the
current line satisfies the requirement."
  (let* ((beg (pos-bol 2))
         (match (text-property-search-forward
                 'gql-builder-indent-level indent-level
                 (lambda (indent-level level-at-point)
                   (and level-at-point
                        (<= level-at-point indent-level)))
                 t))
         (end (if match (prop-match-beginning match) (point-max))))
    (delete-region beg end)))

(defun gql-builder-toggle-expanded (&optional flag)
  "Toggle expanding the field at point.
If FLAG is 1 or -1, expand or collapse regardless of current expansion
state."
  (interactive)
  (let* ((inhibit-read-only t)
         (orig-point (point))
         (flag (or flag 0))
         (arg-p (get-text-property (point) 'gql-builder-arg-p))
         (args (get-text-property (point) 'gql-builder-args))
         (expanded (gql-builder--get-state-at-point
                    (if arg-p 'arg-expanded 'expanded)))
         (indent-level (get-text-property (point) 'gql-builder-indent-level))
         (field-type (gql-builder--render-type
                      (get-text-property (point) 'gql-builder-field-type)
                      t))
         (field-path (get-text-property (point) 'gql-builder-field-path)))
    ;; Collapse.
    (when (or (< flag 0)
              (and (eq flag 0) expanded))
      (gql-builder--set-state-at-point (if arg-p 'arg-expanded 'expanded) nil)
      (when indent-level
        (gql-builder--remove-fields-after-point indent-level)))
    ;; Expand.
    (when (or (> flag 0)
              (and (eq flag 0) (not expanded)))
      (gql-builder--set-state-at-point (if arg-p 'arg-expanded 'expanded) t)
      (when (and indent-level field-path field-type gql-builder--schema)
        (let ((fields (gql-builder--get-fields-for-type
                       gql-builder--schema field-type arg-p)))
          ;; Only show message when this command is called
          ;; interactively.
          (when (and (eq flag 0) (null fields))
            (message "Can’t find any fields for %s" field-type))
          (forward-line 1)
          ;; Insert args.
          (gql-builder--insert-fields
           args (1+ indent-level) field-path t)
          ;; Insert fields, if this field is an arg, then its fielld
          ;; must be args too.
          (gql-builder--insert-fields
           fields (1+ indent-level) field-path arg-p))))
    (goto-char orig-point)))

;; If a field is marked, it will be included in the final query that
;; we build. We store the marked/unmkared state in the UI state, like
;; we do for expanded state.
(defun gql-builder-mark ()
  "Mark the field at point."
  (interactive)
  (let* ((arg-p (get-text-property (point) 'gql-builder-arg-p))
         (marked (gql-builder--get-state-at-point
                  (if arg-p 'arg-marked 'marked)))
         (inhibit-read-only t)
         (props nil)
         (orig-pos (point)))
    (when (not marked)
      (gql-builder--set-state-at-point (if arg-p 'arg-marked 'marked) t)
      (forward-line 0)
      (setq props (text-properties-at (point)))
      (when (search-forward gql-builder-marker-unmarked nil t)
        (replace-match (apply #'propertize gql-builder-marker-marked
                              props)))
      (when-let ((match (text-property-search-forward
                         'face 'gql-builder-field-name #'eq)))
        (put-text-property (prop-match-beginning match)
                           (prop-match-end match)
                           'face 'gql-builder-marked-field-name)))
    (gql-builder-toggle-expanded 1)
    (goto-char orig-pos))
  (forward-line 1))

(defun gql-builder-unmark ()
  "Unmark the field at point."
  (interactive)
  (save-excursion
    (let* ((arg-p (get-text-property (point) 'gql-builder-arg-p))
           (marked (gql-builder--get-state-at-point
                    (if arg-p 'arg-marked 'marked)))
           (inhibit-read-only t)
           (props nil))
      (when marked
        (gql-builder--set-state-at-point (if arg-p 'arg-marked 'marked) nil)
        (when arg-p
          (gql-builder--set-state-at-point 'arg-val nil))
        (forward-line 0)
        (setq props (text-properties-at (point)))
        (when (search-forward gql-builder-marker-marked nil t)
          (replace-match (apply #'propertize gql-builder-marker-unmarked
                                props)))
        (when-let ((match (text-property-search-forward
                           'face 'gql-builder-marked-field-name #'eq)))
          (put-text-property (prop-match-beginning match)
                             (prop-match-end match)
                             'face 'gql-builder-field-name))
        (when-let ((match (text-property-search-forward
                           'face 'gql-builder-field-type #'eq)))
          (delete-region (prop-match-end match) (pos-eol))))))
  (forward-line 1))

(defsubst gql-builder--format-arg-val (val)
  "Format value of an arg, VAL.
VAL can be a string, a number, t, or :false."
  (format " [%s]" val))

(defun gql-builder-set-arg ()
  "Set the value for the arg at point."
  (interactive)
  (save-excursion
    (let* ((arg-p (get-text-property (point) 'gql-builder-arg-p))
           (type (get-text-property (point) 'gql-builder-field-type))
           (inhibit-read-only t)
           (props nil)
           (old-val (gql-builder--get-state-at-point 'arg-val)))
      (when arg-p
        (if (not (string-match-p (rx bos
                                     (or "String" "Int" "Boolean" "Float" "ID")
                                     (* "!")
                                     eos)
                                 (gql-builder--render-type type)))
            (message "Editing array arg is not supported: %s"
                     (gql-builder--render-type type))
          (let* ((val (read-string "Value: " old-val)))
            (gql-builder--set-state-at-point 'arg-val val)
            (forward-line 0)
            (setq props (text-properties-at (point)))
            (when-let ((match (text-property-search-forward
                               'face 'gql-builder-field-type #'eq)))
              (delete-region (prop-match-end match) (pos-eol))
              (insert (apply #'propertize (gql-builder--format-arg-val val)
                             props)))))))))

(defun gql-builder-toggle-marked-all ()
  "Mark/unmark all the fields under this field."
  (interactive)
  (ignore 'todo))

;;;; Major mode

(defvar gql-builder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'gql-builder-refresh)
    (define-key map (kbd "r") #'gql-builder-reorder)
    (define-key map (kbd "C-c C-c") #'gql-builder-save-and-quit)
    (define-key map (kbd "v") #'gql-builder-set-arg)
    (define-key map (kbd "E") #'gql-builder-show-query)
    map)
  "Mode map for ‘gql-builder-mode’.")

(define-derived-mode gql-builder-mode special-mode "QueryBuilder"
  "Major mode for building GraphQL queries."
  :keymap gql-builder-mode-map
  (font-lock-mode -1))

(defun gql-builder (url &optional headers)
  "Start or resume a query builder session with GraphQL endpoint at URL.
If HEADERS is non-nil, add those headers. It should be an alist that
looks like ((\"Content-Type\" . \"application/json\"))."
  (interactive "sEndpoint: ")
  (pop-to-buffer
   (get-buffer-create (format "<query builder for %s>" url)))
  (gql-builder-mode)
  (let ((inhibit-read-only t))
    (condition-case err
        (progn
          (unless gql-builder--schema
            (setq gql-builder--schema (gql-builder--get-schema url headers)))
          (setq gql-builder--endpoint url)
          (erase-buffer)
          (save-excursion
            (gql-builder--insert-fields
             (gql-builder--get-all-queries gql-builder--schema) 0 nil)))
      (plz-http-error
       (erase-buffer)
       (insert "Can’t retrieve schema from " url ":\n")
       (print err (current-buffer))))))

(defun gql-builder-refresh ()
  "Refresh schema and buffer."
  (interactive)
  (setq gql-builder--schema
        (gql-builder--get-schema gql-builder--endpoint nil t))
  (gql-builder-reorder))

(defun gql-builder-clear-schema-cache ()
  "Clear the schema cache."
  (interactive)
  (setq gql-builder--schema-cache nil))

(defun gql-builder-reorder ()
  "Reorder fields so marked ones come first."
  (interactive)
  (let ((inhibit-read-only t)
        (orig-point (point)))
    (erase-buffer)
    (if (null gql-builder--schema)
        (insert "Can’t retrieve schema from " url "\n")
      (gql-builder--insert-fields
       (gql-builder--get-all-queries gql-builder--schema) 0 nil))
    (goto-char (min orig-point (point-max)))))

(defun gql-builder-show-query ()
  "Show the GraphQL query this builder will generate."
  (interactive)
  (let ((inner-query (gql-builder--serialize-query-object
                      (gql-builder--construct-query-object
                       (gql-builder--get-all-marked-field-paths
                        gql-builder--ui-state)
                       (gql-builder--get-all-marked-arg-values
                        gql-builder--ui-state)
                       nil)
                      0)))
    (pop-to-buffer "*gql-builder show query*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert inner-query)
      (goto-char (point-min))
      (insert "  ")
      (while (eq 0 (forward-line 1))
        (unless (looking-at (rx (* whitespace) eol))
          (insert "  ")))
      (insert "}")
      (goto-char (point-min))
      (insert "{\n"))
    (special-mode)))

;;; Restclient integration

(defvar gql-builder--data-store-location
  (if (file-exists-p (expand-file-name "var" user-emacs-directory))
      (expand-file-name "var/gql-builder/gql-builder-data.el"
                        user-emacs-directory)
    (expand-file-name "gql-builder-data.el" user-emacs-directory))
  "Location of the data store.")

(defvar gql-builder--data-store nil
  "Maps GraphQL query string to the UI-state for the query builder.
The value is an alist mapping (ENDPOINT-URL QUERY-STRING) to UI
states. QUERY-STRING doesn’t have trailing whitespace/newline.")

(defun gql-builder--load-data-store ()
  "Load cached queries form data store file."
  (when (file-exists-p gql-builder--data-store-location)
    (with-temp-buffer
      (insert-file-contents gql-builder--data-store-location)
      (goto-char (point-min))
      (setq gql-builder--data-store (read (current-buffer))))))

(defun gql-builder--save-ui-state ()
  "Save the current UI state to query cache."
  (unless gql-builder--endpoint
    (signal 'gql-builder-error '("Current buffer doesn’t have a saved GraphQL endpoint (‘gql-builder--endpoint’)")))
  (unless gql-builder--initial-body
    (signal 'gql-builder-error '("Current buffer doesn’t have a saved initial query (‘gql-builder--initial-body’)")))

  (setf (alist-get (list gql-builder--endpoint gql-builder--initial-body)
                   gql-builder--data-store nil t #'equal)
        gql-builder--ui-state))

(defun gql-builder--save-data-store ()
  "Save cached queries into data store file."
  (when gql-builder--data-store-location
    (let ((dir (file-name-directory gql-builder--data-store-location)))
      (unless (file-exists-p dir)
        (mkdir dir t)))
    (with-temp-buffer
      (print gql-builder--data-store (current-buffer))
      (write-region (point-min) (point-max) gql-builder--data-store-location))))

(defun gql-builder--restclient-show-gql-builder
    (method url headers body pos)
  "Show query builder for this request with METHOD, URL, HEADERS, BODY.

POS is the position of point when user invoked ‘restclient-gql-builder’.

This function is supposed to be called by
‘restclient-http-parse-current-and-do’."
  (unless (equal method "GQL")
    (signal 'gql-builder-error '("GraphQL request should have GQL for the method")))

  (unless gql-builder--data-store
    (gql-builder--load-data-store))

  (let ((buf (current-buffer))
        (window-config (current-window-configuration))
        ;; This is a bit hacky, but it’s the only way to get the
        ;; original body before restclient substitutes variables in
        ;; it.
        (unsubstituted-body
         (let ((cmax (restclient-current-max)))
           (restclient-parse-body
            (buffer-substring (min (point) cmax) cmax) nil))))

    (gql-builder url headers)
    (setq gql-builder--orig-window-config window-config)
    (setq gql-builder--restclient-state (list (cons 'body unsubstituted-body)
                                              (cons 'buffer buf)
                                              (cons 'point pos)))
    (when (and unsubstituted-body (not (equal unsubstituted-body "")))
      (let ((ui-state (alist-get (list url (string-trim unsubstituted-body))
                                 gql-builder--data-store
                                 nil t #'equal)))
        (setq gql-builder--initial-body (string-trim unsubstituted-body))
        (if (null ui-state)
            (message "Can’t resume the query, query builder can only resume query built by itself, it can’t parse an existing query")
          (setq gql-builder--ui-state ui-state)
          (gql-builder-reorder))))))

(defun restclient-gql-builder ()
  "Popup a query builder buffer to edit the query in the current request."
  (interactive)
  (restclient-http-parse-current-and-do
   #'gql-builder--restclient-show-gql-builder
   (point)))

(defun gql-builder-save-and-quit ()
  "Save the query in the query builder to the original restclient buffer.
And quit the query builder."
  (interactive)
  (let* ((body (alist-get 'body gql-builder--restclient-state))
         (buffer (alist-get 'buffer gql-builder--restclient-state))
         (pos (alist-get 'point gql-builder--restclient-state))
         (query (concat "{\n"
                        (gql-builder--serialize-query-object
                         (gql-builder--construct-query-object
                          (gql-builder--get-all-marked-field-paths
                           gql-builder--ui-state)
                          (gql-builder--get-all-marked-arg-values
                           gql-builder--ui-state)
                          nil)
                         1)
                        "}"))
         (new-body query))
    (setf (alist-get (list gql-builder--endpoint (string-trim new-body))
                     gql-builder--data-store nil t #'equal)
          gql-builder--ui-state)
    (gql-builder--save-data-store)
    (when buffer
      (with-current-buffer buffer
        (goto-char pos)
        (goto-char (restclient-current-min))
        (if (or (null body) (equal body ""))
            ;; Empty body
            (if (not (re-search-forward
                      restclient-method-url-regexp (point-max) t))
                (signal 'gql-builder-error
                        '("Can’t find the original request header"))
              (forward-line)
              (while (or (looking-at restclient-response-hook-regexp)
                         (and (looking-at restclient-header-regexp) (not (looking-at restclient-empty-line-regexp)))
                         (looking-at restclient-use-var-regexp))
                (forward-line))
              (insert "\n" new-body))
          ;; Non-empty body
          (if (search-forward body nil t)
              (progn
                (replace-match new-body t t)
                (insert "\n"))
            (if (yes-or-no-p "Can’t find the original request body, insert at the end? ")
                (progn
                  (goto-char (restclient-current-max))
                  (insert new-body "\n"))
              (signal 'gql-builder-error '("Can’t find the original request body"))))))))

  (when gql-builder--orig-window-config
    (set-window-configuration gql-builder--orig-window-config)))

(provide 'gql-builder)

;;; gql-builder.el ends here

;;; query-builder.el --- Query builder for GraphQL  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; GraphQL reference: https://spec.graphql.org/October2021/#sec-Introspection
;;

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

(defface query-builder-marked-field-name-face
  (let ((display t)
        (atts '(:inherit query-builder-field-name-face :weight bold)))
    `((,display . ,atts)))
  "Face for the name of a marked field in a query.")

(defface query-builder-field-type-face
  (let ((display t)
        (atts '(:inherit shadow)))
    `((,display . ,atts)))
  "Face for the type shown after each field.")

(defvar query-builder-marker-marked "[*] "
  "A string used to mark a marked field.")

(defvar query-builder-marker-unmarked "[ ] "
  "A string used to mark an unmarded field.")

(defvar query-builder-query-and-mutation-query
  "
query Introspect {
    __schema {
        queryType {
            name
        }
        mutationType {
            name
        }
        types {
            name
            fields(includeDeprecated: true) {
                name
                isDeprecated
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
    overview {
        popId
        popName
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

In terms of JSON, (\"fieldB\" \"fieldA\") corresponds to

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
Return (alist-get KEY3 (alist-get KEY2 (alist-get KEY1 alist))). If
chain is nil, return ALIST."
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
;;   scalar, an enum, a union, or an interface.

(defvar-local query-builder--schema nil
  "The schema object.")

(defvar-local query-builder--endpoint nil
  "The endpoint URL.")

(defvar-local query-builder--initial-query nil
  "The initial strin GraphQL query we’re building upon.")

(defvar-local query-builder--restclient-state nil
  "The states of restclient request that initiated this query builder.
The value should be an alist with the following keys:

  - ‘body’: The request body.
  - ‘buffer’: The restclient buffer
  - ‘point’: The point.")

(defvar-local query-builder--orig-window-config nil
  "The window configuration before we popped the query builder buffer.")

(defun query-builder--decode-type (type)
  "Decode TYPE into our internal structure FIELD-TYPE.

See comments in the source file for the definition of FIELD-TYPE.
TYPE is a JSON object from the schema."
  (pcase (alist-get 'kind type)
    ("LIST" `(List ,(query-builder--decode-type
                     (alist-get 'ofType type))))
    ("NON_NULL" `(Non-null ,(query-builder--decode-type
                             (alist-get 'ofType type))))
    ((or "OBJECT" "SCALAR" "ENUM" "UNION" "INTERFACE") (alist-get 'name type))
    (kind (signal 'query-builder-schema-error
                  (list "Unexpected kind of a type" kind type)))))

(defun query-builder--get-schema (url &optional headers)
  "Reuturn the schema at URL as a JSON object.

If HEADERS is non-nil, add those headers. It should be an alist that
looks like ((\"Content-Type\" . \"application/json\"))."
  (dolist (header `(("Accept" . "*/*")
                    ("Content-Type" . "application/json")
                    ("Host" . ,(url-host (url-generic-parse-url url)))))
    (unless (alist-get (car header) headers nil nil #'equal)
      (push header headers)))

  (plz 'post url
    :headers headers
    :body (json-serialize
           `(:query ,query-builder-query-and-mutation-query))
    :as (lambda () (json-parse-buffer :object-type 'alist :null-object nil))))

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

;;;; Building query

(defun query-builder--get-all-marked-field-paths (ui-state)
  "Get all the field paths that are marked in UI-STATE.

;; Reverse every field path so the root field comes first, and sort field
;; paths so shorter field paths comes first, and field paths with the same
;; prefix stays together.
"
  (let (results)
    (pcase-dolist (`(,field-path . ,states) ui-state)
      (when (alist-get 'marked states)
        (push field-path results)))
    results
    ;; (seq-sort (lambda (path-a path-b)
    ;;             (if (< (length path-a) (length path-b))
    ;;                 t
    ;;               (catch 'less
    ;;                 (dolist (zip (cl-mapcar #'cons path-a path-b))
    ;;                   (when (not (equal (car zip) (cdr zip)))
    ;;                     (throw 'less (string< (car zip) (cdr zip))))))))
    ;;           (seq-map #'reverse results))
    ))

(defun query-builder--construct-query-object (field-paths suffix)
  "Build a JSON object out of FIELD-PATHS.
Return a JSON object made of all the field paths that ends with
SUFFIX.

FIELD-PATHS: (\"A\") (\"B\" \"A\") (\"C\" \"A\") (\"D\")
Return: JSON object that encodes { A: { B: nil, C: nil }, D: nil }"
  ;; TODO: Stratify FIELD-PATHS by length.
  (let* ((suffix-len (if suffix (length suffix) 0))
         (field (car suffix))
         (immediate-children
          (if (eq suffix-len 0)
              (seq-filter (lambda (field-path) (eq (length field-path) 1))
                          field-paths)
            (seq-filter (lambda (field-path)
                          (and (eq (length field-path) (1+ suffix-len))
                               (equal (cdr field-path) suffix)))
                        field-paths)))
         ;; If there’s no immediate children, SUBGRAPHS would be nil,
         ;; if there are, SUBGRAPHS will be an alist, where each
         ;; immediate children is the key, and their subgraph are the
         ;; values.
         (subgraphs (mapcar (lambda (child)
                              (query-builder--construct-query-object
                               field-paths child))
                            immediate-children)))
    (if field (cons field subgraphs) subgraphs)))

(defun query-builder--serialize-query-object (query-object &optional indent)
  "Serialize QUERY-OBJECT to a GraphQL query string.

If INDENT is non-nil, it should be the indent level, a number, and this
function will pretty print the query."
  (let ((indent-string (if indent (make-string (* 2 indent) ?\s) nil)))
    (string-join
     (mapcar (lambda (child)
               (let ((name (car child))
                     (sub-fields (cdr child)))
                 (if sub-fields
                     (if indent-string
                         (concat indent-string name " {\n"
                                 (query-builder--serialize-query-object
                                  sub-fields (and indent (1+ indent)))
                                 indent-string "}\n")
                       (format "%s { %s }" name
                               (query-builder--serialize-query-object
                                sub-fields)))
                   (if indent-string
                       (concat indent-string name "\n")
                     name))))
             query-object)
     (if indent nil " "))))

;;;; UI: drawing UI, toggling fields

(defvar query-builder-field-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'query-builder-toggle-expanded)
    (define-key map (kbd "m") #'query-builder-mark)
    (define-key map (kbd "u") #'query-builder-unmark)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
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

(defun query-builder--sort-by-marked (fields parent-field-path)
  "Sort FIELDS by putting marked ones in the front.
FIELDS and PARENT-FIELD-PATH are the same as in
‘query-builder--insert-fields’."
  (seq-sort (lambda (a b)
              (let ((a-marked (query-builder--get-state
                               (cons (nth 1 a) parent-field-path) 'marked))
                    (b-marked (query-builder--get-state
                               (cons (nth 1 b) parent-field-path) 'marked)))
                (cond
                 ((and a-marked (not b-marked)) t)
                 ((and b-marked (not a-marked)) nil)
                 (t (string< (nth 1 a) (nth 1 b))))))
            fields))

(defun query-builder--insert-fields (fields indent-level parent-field-path)
  "Insert FIELDS at point.
Each field in FIELDS should be for the form

    (Field FIELD-NAME FIELD-TYPE)

INDENT-LEVEL is the nesting level of the fields. PARENT-FIELD-PATH is the
field path to the parent of fields. It’s used for constructing the field
path of each field in FIELDS. Specifically, each fields field path
is (cons FIELD-NAME PARENT-FIELED-PATH)."
  (pcase-dolist (`(Field ,name ,type)
                 (query-builder--sort-by-marked fields parent-field-path))
    (let* ((field-path (cons name parent-field-path))
           (marked (query-builder--get-state field-path 'marked)))
      (insert (propertize
               (concat
                (make-string (* 2 indent-level) ?\s)
                (if marked
                    query-builder-marker-marked
                  query-builder-marker-unmarked)
                (propertize (or name "N/A")
                            'face (if marked
                                      'query-builder-marked-field-name-face
                                    'query-builder-field-name-face))
                " "
                (propertize (query-builder--render-type (or type "N/A"))
                            'face 'query-builder-field-type-face))
               'query-builder-field-path field-path
               'query-builder-field-name name
               'query-builder-field-type type
               'query-builder-indent-level indent-level
               'keymap query-builder-field-map)
              "\n")
      ;; Insert subfields if this field is expanded.
      (when (and query-builder--schema
                 (query-builder--get-state field-path 'expanded))
        (query-builder--insert-fields
         (query-builder--get-fields-for-type
          query-builder--schema
          (query-builder--render-type type t))
         (1+ indent-level)
         field-path)))))

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

(defun query-builder-toggle-expanded (&optional flag)
  "Toggle expanding the field at point.
If FLAG is 1 or -1, expand or collapse regardless of current expansion
state."
  (interactive)
  (let ((inhibit-read-only t)
        (orig-point (point))
        (flag (or flag 0)))
    (let ((expanded (query-builder--get-state-at-point 'expanded))
          (indent-level (get-text-property (point) 'query-builder-indent-level))
          (field-type (query-builder--render-type
                       (get-text-property (point) 'query-builder-field-type)
                       t))
          (field-path (get-text-property (point) 'query-builder-field-path)))
      (when (or (< flag 0)
                (and (eq flag 0) expanded))
        (query-builder--set-state-at-point 'expanded nil)
        (when indent-level
          (query-builder--remove-fields-after-point indent-level)))
      (when (or (> flag 0)
                (and (eq flag 0) (not expanded)))
        (query-builder--set-state-at-point 'expanded t)
        (when (and indent-level field-path field-type query-builder--schema)
          (let ((fields (query-builder--get-fields-for-type
                         query-builder--schema field-type)))
            ;; Only show message when this command is called
            ;; interactively.
            (when (and (eq flag 0) (null fields))
              (message "Can’t find any fields for %s" field-type))
            (forward-line 1)
            (query-builder--insert-fields
             fields (1+ indent-level) field-path)))))
    (goto-char orig-point)))

;; If a field is marked, it will be included in the final query that
;; we build. We store the marked/unmkared state in the UI state, like
;; we do for expanded state.
(defun query-builder-mark ()
  "Mark the field at point."
  (interactive)
  (save-excursion
    (let ((marked (query-builder--get-state-at-point 'marked))
          (inhibit-read-only t)
          (props nil))
      (when (not marked)
        (query-builder--set-state-at-point 'marked t)
        (forward-line 0)
        (setq props (text-properties-at (point)))
        (when (search-forward query-builder-marker-unmarked nil t)
          (replace-match (apply #'propertize query-builder-marker-marked
                                props)))
        (when-let ((match (text-property-search-forward
                           'face 'query-builder-field-name-face #'eq)))
          (put-text-property (prop-match-beginning match)
                             (prop-match-end match)
                             'face 'query-builder-marked-field-name-face))))
    (query-builder-toggle-expanded 1))
  (forward-line 1))

(defun query-builder-unmark ()
  "Unmark the field at point."
  (interactive)
  (save-excursion
    (let ((marked (query-builder--get-state-at-point 'marked))
          (inhibit-read-only t)
          (props nil))
      (when marked
        (query-builder--set-state-at-point 'marked nil)
        (forward-line 0)
        (setq props (text-properties-at (point)))
        (when (search-forward query-builder-marker-marked nil t)
          (replace-match (apply #'propertize query-builder-marker-unmarked
                                props)))
        (when-let ((match (text-property-search-forward
                           'face 'query-builder-marked-field-name-face #'eq)))
          (put-text-property (prop-match-beginning match)
                             (prop-match-end match)
                             'face 'query-builder-field-name-face)))))
  (forward-line 1))

(defun query-builder-toggle-marked-all ()
  "Mark/unmark all the fields under this field."
  (interactive)
  (ignore 'todo))

;;;; Major mode

(defvar query-builder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'query-builder-refresh)
    (define-key map (kbd "r") #'query-builder-reorder)
    map)
  "Mode map for ‘query-builder-mode’.")

(define-derived-mode query-builder-mode special-mode "QueryBuilder"
  "Major mode for building GraphQL queries."
  :keymap query-builder-mode-map
  (font-lock-mode -1))

(defun query-builder (url &optional headers)
  "Start or resume a query builder session with GraphQL endpoint at URL.
If HEADERS is non-nil, add those headers. It should be an alist that
looks like ((\"Content-Type\" . \"application/json\"))."
  (interactive "sEndpoint: ")
  (pop-to-buffer
   (get-buffer-create (format "<query builder for %s>" url)))
  (query-builder-mode)
  (let ((inhibit-read-only t))
    (condition-case err
        (progn
          (unless query-builder--schema
            (setq query-builder--schema (query-builder--get-schema url headers)))
          (setq query-builder--endpoint url)
          (erase-buffer)
          (save-excursion
            (query-builder--insert-fields
             (query-builder--get-all-queries query-builder--schema) 0 nil)))
      (plz-http-error
       (erase-buffer)
       (insert "Can’t retrieve schema from " url ":\n")
       (print err (current-buffer))))))

(defun query-builder-refresh ()
  "Refresh schema and buffer."
  (interactive)
  (setq query-builder--schema
        (query-builder--get-schema query-builder--endpoint))
  (query-builder-reorder))

(defun query-builder-reorder ()
  "Reorder fields so marked ones come first."
  (interactive)
  (let ((inhibit-read-only t)
        (orig-point (point)))
    (erase-buffer)
    (if (null query-builder--schema)
        (insert "Can’t retrieve schema from " url "\n")
      (query-builder--insert-fields
       (query-builder--get-all-queries query-builder--schema) 0 nil))
    (goto-char (min orig-point (point-max)))))

;;; Restclient integration

(defvar query-builder--data-store-location
  (if (file-exists-p (expand-file-name "var" user-emacs-directory))
      (expand-file-name "var/query-builder/query-builder-data.el"
                        user-emacs-directory)
    (expand-file-name "query-builder-data.el" user-emacs-directory))
  "Location of the data store.")

(defvar query-builder--data-store nil
  "Maps GraphQL query string to the UI-state for the query builder.
The value is an alist mapping (ENDPOINT-URL QUERY-STRING) to UI
states. QUERY-STRING doesn’t have trailing whitespace/newline.")

(defun query-builder--load-data-store ()
  "Load cached queries form data store file."
  (when (file-exists-p query-builder--data-store-location)
    (with-temp-buffer
      (insert-file-contents query-builder--data-store-location)
      (goto-char (point-min))
      (setq query-builder--data-store (read (current-buffer))))))

(defun query-builder--save-ui-state ()
  "Save the current UI state to query cache."
  (unless query-builder--endpoint
    (signal 'query-builder-error '("Current buffer doesn’t have a saved GraphQL endpoint (‘query-builder--endpoint’)")))
  (unless query-builder--initial-query
    (signal 'query-builder-error '("Current buffer doesn’t have a saved initial query (‘query-builder--initial-query’)")))

  (setf (alist-get (list query-builder--endpoint query-builder--initial-query)
                   query-builder--data-store nil t #'equal)
        query-builder--ui-state))

(defun query-builder--save-data-store ()
  "Save cached queries into data store file."
  (when query-builder--data-store-location
    (let ((dir (file-name-directory query-builder--data-store-location)))
      (unless (file-exists-p dir)
        (mkdir dir t)))
    (with-temp-buffer
      (print query-builder--data-store (current-buffer))
      (write-file query-builder--data-store-location))))

(defun query-builder--restclient-show-query-builder
    (method url headers body pos)
  "Show query builder for this request with METHOD, URL, HEADERS, BODY.

POS is the position of point when user invoked ‘restclient-query-builder’.

This function is supposed to be called by
‘restclient-http-parse-current-and-do’."
  (unless (equal method "POST")
    (signal 'query-builder-error '("GraphQL request should be POST request")))

  (unless query-builder--data-store
    (query-builder--load-data-store))

  (let ((buf (current-buffer))
        (window-config (current-window-configuration)))

    (query-builder url headers)
    (setq query-builder--orig-window-config window-config)
    (setq query-builder--restclient-state (list (cons 'body body)
                                                (cons 'buffer buf)
                                                (cons 'point pos)))
    (when (and body (not (equal body "")))
      (let* ((json-object (json-parse-string body :object-type 'alist))
             (query-string (string-trim (alist-get 'query json-object)))
             (ui-state (and query-string
                            (alist-get (list url query-string)
                                       query-builder--data-store
                                       nil t #'equal))))
        (unless query-string
          (signal 'query-builder-parse-error
                  '("Can’t find “query” field in the body")))
        (setq query-builder--initial-query query-string)
        (if (null ui-state)
            (message "Can’t resume the query, query builder can only resume query built by itself, it can’t parse an existing query")
          (setq query-builder--ui-state ui-state)
          (query-builder-reorder))))))

(defun restclient-query-builder ()
  "Popup a query builder buffer to edit the query in the current request."
  (interactive)
  (restclient-http-parse-current-and-do
   #'query-builder--restclient-show-query-builder
   (point)))

(defun query-builder-save-and-quit ()
  "Save the query in the query builder to the original restclient buffer.
And quit the query builder."
  (interactive)
  (let* ((body (alist-get 'body query-builder--restclient-state))
         (buffer (alist-get 'buffer query-builder--restclient-state))
         (pos (alist-get 'point query-builder--restclient-state))
         (query (concat "{ "
                        (query-builder--serialize-query-object
                         (query-builder--construct-query-object
                          (query-builder--get-all-marked-field-paths
                           query-builder--ui-state)
                          nil))
                        " }"))
         (new-body (json-serialize `((query . ,query)) )))
    (setf (alist-get (list query-builder--endpoint query-builder--initial-query)
                     query-builder--data-store nil t #'equal)
          query-builder--ui-state)
    (when buffer
      (with-current-buffer buffer
        (goto-char pos)
        (goto-char (restclient-current-min))
        (if (or (null body) (equal body ""))
            (if (re-search-forward restclient-method-url-regexp (point-max) t)
                (insert "\n\n" new-body)
              (signal 'query-builder-error '("Can’t find the original request’s header")))

          (unless (search-forward body nil t)
            (signal 'query-builder-error '("Can’t find the original request body")))
          (replace-match new-body)
          (insert "\n")))))

  (when query-builder--orig-window-config
    (set-window-configuration query-builder--orig-window-config)))

(provide 'query-builder)

;;; query-builder.el ends here

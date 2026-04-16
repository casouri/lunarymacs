;;; tsx-imenu.el --- Imenu helpers for tsx-ts-mode  -*- lexical-binding: t; -*-

;; LLM-generated.

(require 'subr-x)
(require 'treesit)

(defconst tsx-imenu-non-top-level-regexp
  (rx bos
      (or "abstract_class_declaration"
          "arrow_function"
          "class_declaration"
          "function_declaration"
          "function_expression"
          "generator_function"
          "generator_function_declaration"
          "internal_module"
          "method_definition")
      eos)
  "Ancestor node types that make a declaration non-top-level for Imenu.")

(defun tsx-imenu-name (node)
  "Return an Imenu name for TSX NODE."
  (when-let* ((name-node
               (pcase (treesit-node-type node)
                 ((or "lexical_declaration" "variable_declaration")
                  (when-let* ((declarator
                               (treesit-search-subtree
                                node "variable_declarator" nil nil 1)))
                    (treesit-node-child-by-field-name declarator "name")))
                 ("internal_module"
                  (or (treesit-node-child-by-field-name node "name")
                      (treesit-node-child node 1)))
                 (_
                  (treesit-node-child-by-field-name node "name")))))
    (treesit-node-text name-node t)))

(defun tsx-imenu-declaration-value (node)
  "Return the first initializer value found in declaration NODE."
  (when-let* ((declarator
               (treesit-search-subtree node "variable_declarator" nil nil 1)))
    (treesit-node-child-by-field-name declarator "value")))

(defun tsx-imenu-function-value-p (node)
  "Return non-nil if NODE denotes a function value for Imenu."
  ;; Tree-sitter wraps function-valued declarations in a few ways, so
  ;; this predicate peels wrappers until it reaches the real value.
  ;; That lets all of these land under Function:
  ;;
  ;;   const foo = () => {}
  ;;   const foo = function () {}
  ;;   const foo = (() => {})
  ;;   const foo = bar as () => void
  ;;
  ;; while keeping plain declarations like `const foo = 1' out.
  (pcase (treesit-node-type node)
    ((or "arrow_function" "function_expression" "generator_function") t)
    ((or "as_expression"
         "non_null_expression"
         "parenthesized_expression"
         "satisfies_expression"
         "type_assertion")
     (when-let* ((child (or (treesit-node-child-by-field-name node "expression")
                            (treesit-node-child node 0 t))))
       (tsx-imenu-function-value-p child)))
    (_ nil)))

(defun tsx-imenu-function-p (node)
  "Return non-nil if NODE should appear under Function in Imenu."
  ;; `treesit-simple-imenu' can filter entries, but it doesn't let one
  ;; node type contribute to two categories with different rules.  So
  ;; lexical/variable declarations are matched in both Function and
  ;; Variable, and the -p predicates decide where they finally go:
  ;;
  ;;   const foo = () => {}   => Function
  ;;   const foo = 1          => not Function
  ;;   function foo() {}      => Function
  (pcase (treesit-node-type node)
    ((or "function_declaration" "generator_function_declaration") t)
    ((or "lexical_declaration" "variable_declaration")
     (when-let* ((value (tsx-imenu-declaration-value node)))
       (tsx-imenu-function-value-p value)))
    (_ nil)))

(defun tsx-imenu-top-level-variable-p (node)
  "Return non-nil if NODE is a non-function top-level variable declaration."
  ;; This is the complement of `tsx-imenu-function-p' for declarations.
  ;; It keeps:
  ;;
  ;;   const foo = 1
  ;;
  ;; at file scope, but filters out both:
  ;;
  ;;   function outer() { const foo = 1 }
  ;;   const foo = () => {}
  ;;
  ;; The explicit ancestor check is here because we still want named
  ;; functions nested inside other functions/methods to show up, while
  ;; plain variables should stay top-level only.
  (and (not (tsx-imenu-function-p node))
       (not (treesit-node-top-level
             node tsx-imenu-non-top-level-regexp))))

(defconst tsx-imenu-settings
  `(("Namespace" "\\`internal_module\\'" nil
     ,#'tsx-imenu-name)
    ("Interface" "\\`interface_declaration\\'" nil
     ,#'tsx-imenu-name)
    ("Type" "\\`type_alias_declaration\\'" nil
     ,#'tsx-imenu-name)
    ("Enum" "\\`enum_declaration\\'" nil
     ,#'tsx-imenu-name)
    ("Class" ,(rx bos (or "abstract_class_declaration"
                          "class_declaration")
                  eos)
     nil
     ,#'tsx-imenu-name)
    ("Method" ,(rx bos (or "abstract_method_signature"
                           "method_definition"
                           "method_signature")
                   eos)
     nil
     ,#'tsx-imenu-name)
    ("Function" ,(rx bos (or "function_declaration"
                             "generator_function_declaration"
                             "lexical_declaration"
                             "variable_declaration")
                     eos)
     ,#'tsx-imenu-function-p
     ,#'tsx-imenu-name)
    ("Variable" ,(rx bos (or "lexical_declaration"
                             "variable_declaration")
                     eos)
     ,#'tsx-imenu-top-level-variable-p
     ,#'tsx-imenu-name))
  "Imenu settings for `tsx-ts-mode'.")

(provide 'tsx-imenu)

;;; tsx-imenu.el ends here

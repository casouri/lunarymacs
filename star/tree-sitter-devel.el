;; -*- lexical-binding: t -*-

(defun ts-test ()
  (with-temp-buffer
    (progn
      (insert "[1,2,{\"name\": \"Bob\"},3]")
      (setq parser (tree-sitter-parser-create
                    (current-buffer) 'tree-sitter-json))
      (setq root-node (tree-sitter-parser-root-node
                       parser))
      (setq doc-node (tree-sitter-node-child root-node 0))
      ;; (tree-sitter-node-child doc-node 8)
      (tree-sitter-node-start (tree-sitter-node-child doc-node 8))
      ;; (tree-sitter-node-start doc-node)
      ;; (tree-sitter-node-end doc-node)
      )))

(when (boundp 'tree-sitter-parser-list)
  (require 'tree-sitter)
  (load "~/emacs/test/src/tree-sitter-tests.el")

  (defun tree-sitter-show-buffer-tree ()
    (interactive)
    (let ((root-node (tree-sitter-parser-root-node
                      (or (car tree-sitter-parser-list)
                          (tree-sitter-create-parser (current-buffer) (tree-sitter-c))))))
      (pop-to-buffer (get-buffer-create "*tree-sitter-show-tree*"))
      (erase-buffer)
      (insert (pp-to-string (read (tree-sitter-node-string root-node)))))))

;;; Python

(defvar ts-python-builtins
  '("abs" "all" "any" "ascii" "bin" "bool" "breakpoint" "bytearray"
    "bytes" "callable" "chr" "classmethod" "compile" "complex"
    "delattr" "dict" "dir" "divmod" "enumerate" "eval" "exec"
    "filter" "float" "format" "frozenset" "getattr" "globals"
    "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
    "issubclass" "iter" "len" "list" "locals" "map" "max"
    "memoryview" "min" "next" "object" "oct" "open" "ord" "pow"
    "print" "property" "range" "repr" "reversed" "round" "set"
    "setattr" "slice" "sorted" "staticmethod" "str" "sum" "super"
    "tuple" "type" "vars" "zip" "__import__"))

(defvar ts-python-operators
  '("-" "-=" "!=" "*" "**" "**=" "*=" "/" "//" "//=" "/=" "&" "%" "%="
    "^" "+" "+=" "<" "<<" "<=" "<>" "=" "==" ">" ">=" ">>" "|" "~"
    "and" "in" "is" "not" "or"))

(defvar ts-python-keywords
  '("as" "assert" "async" "await" "break" "class" "continue" "def"
    "del" "elif" "else" "except" "exec" "finally" "for" "from"
    "global" "if" "import" "lambda" "nonlocal" "pass" "print"
    "raise" "return" "try" "while" "with" "yield"))

(defvar ts-python-special-attributes
  '("__annotations__" "__closure__" "__code__"
    "__defaults__" "__dict__" "__doc__" "__globals__"
    "__kwdefaults__" "__name__" "__module__" "__package__"
    "__qualname__" "__all__"))

(defvar ts-python-exceptions
  '(;; Python 2 and 3:
    "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
    "BufferError" "BytesWarning" "DeprecationWarning" "EOFError"
    "EnvironmentError" "Exception" "FloatingPointError" "FutureWarning"
    "GeneratorExit" "IOError" "ImportError" "ImportWarning"
    "IndentationError" "IndexError" "KeyError" "KeyboardInterrupt"
    "LookupError" "MemoryError" "NameError" "NotImplementedError"
    "OSError" "OverflowError" "PendingDeprecationWarning"
    "ReferenceError" "RuntimeError" "RuntimeWarning" "StopIteration"
    "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit" "TabError"
    "TypeError" "UnboundLocalError" "UnicodeDecodeError"
    "UnicodeEncodeError" "UnicodeError" "UnicodeTranslateError"
    "UnicodeWarning" "UserWarning" "ValueError" "Warning"
    "ZeroDivisionError"
    ;; Python 2:
    "StandardError"
    ;; Python 3:
    "BlockingIOError" "BrokenPipeError" "ChildProcessError"
    "ConnectionAbortedError" "ConnectionError" "ConnectionRefusedError"
    "ConnectionResetError" "FileExistsError" "FileNotFoundError"
    "InterruptedError" "IsADirectoryError" "NotADirectoryError"
    "PermissionError" "ProcessLookupError" "RecursionError"
    "ResourceWarning" "StopAsyncIteration" "TimeoutError"
    ;; OS specific
    "VMSError" "WindowsError"
    ))

(defun ts-python-string-fontify (beg end _)
  "Do not fontify the initial f with string-face."
  (let ((beg (if (eq (char-after beg) ?f)
                 (1+ beg) beg)))
    (put-text-property beg end 'face 'font-lock-string-face)))

(defvar ts-python-tree-sitter-settings-1
  `((tree-sitter-python
     ,(tree-sitter-expand-query
       `((function_definition
          name: (identifier) @font-lock-function-name-face)

         (class_definition
          name: (identifier) @font-lock-type-face)

         (string) @ts-python-string-fontify
         (comment) @font-lock-comment-face

         ((string) @font-lock-doc-face
          (:match ,(rx (seq bol "\"\"\"")) @font-lock-doc-face))
         (interpolation) @default)))))

(defvar ts-python-tree-sitter-settings-2
  `((tree-sitter-python
     ,(concat (cadar ts-python-tree-sitter-settings-1)
              (tree-sitter-expand-query
               `([,@ts-python-keywords] @font-lock-keyword-face

                 ((identifier) @font-lock-keyword-face
                  (:match ,(rx (seq bol "self" eol))
                          @font-lock-keyword-face))

                 ((identifier) @font-lock-builtin-face
                  (:match ,(rx-to-string
                            `(seq bol
                                  (or ,@ts-python-builtins
                                      ,@ts-python-special-attributes)
                                  eol))
                          @font-lock-builtin-face))))))))

(defvar ts-python-tree-sitter-settings-3
  `((tree-sitter-python
     ,(concat (cadar ts-python-tree-sitter-settings-2)
              (tree-sitter-expand-query
               `((assignment left: (identifier)
                             @font-lock-variable-name-face)
                 (pattern_list (identifier)
                               @font-lock-variable-name-face)
                 (tuple_pattern (identifier)
                                @font-lock-variable-name-face)
                 (list_pattern (identifier)
                               @font-lock-variable-name-face)
                 (list_splat_pattern (identifier)
                                     @font-lock-variable-name-face)
                 (decorator) @font-lock-type-face
                 ((identifier) @font-lock-type-face
                  (:match ,(rx-to-string
                            `(seq bol (or ,@ts-python-exceptions)
                                  eol))
                          @font-lock-type-face))))))))

(define-derived-mode ts-python-mode prog-mode "TS Python"
  "C mode with tree-sitter support."
  (if (tree-sitter-should-enable-p)
      (progn
        (setq-local tree-sitter-font-lock-defaults
                    '((ts-python-tree-sitter-settings-1
                       ts-python-tree-sitter-settings-1
                       ts-python-tree-sitter-settings-2
                       ts-python-tree-sitter-settings-3))

                    font-lock-defaults nil
                    ;; (ignore t nil nil nil)

                    ;; indent-line-function
                    ;; #'tree-sitter-indent

                    ;; tree-sitter-simple-indent-rules
                    ;; ts-python-tree-sitter-indent-rules
                    )
        (tree-sitter-font-lock-enable))
    ))

(add-to-list 'auto-mode-alist '("\\.tspy\\'" . ts-python-mode))


;;; Lab (remove before merge)

(define-derived-mode json-mode js-mode "JSON"
  "Major mode for JSON documents."
  (setq-local tree-sitter-font-lock-defaults
              '((json-tree-sitter-settings-1)))
  (tree-sitter-font-lock-enable))

(defvar json-tree-sitter-settings-1
  '(tree-sitter-json
    ((string) @font-lock-string-face
     (true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (null) @font-lock-constant-face)))

(defun ts-c-fontify-system-lib (beg end _)
  "Fortify a #include <lib>.
Fortify the angled brackets in preprocessor-face,
and the lib name in string-face."
  (put-text-property beg (1+ beg) 'face 'font-lock-preprocessor-face)
  (put-text-property (1- end) end 'face 'font-lock-preprocessor-face)
  (put-text-property (1+ beg) (1- end)
                     'face 'font-lock-string-face))

;; Please compiler.
(defvar ts-c-tree-sitter-indent-rules)
(define-derived-mode ts-c-mode prog-mode "TS C"
  "C mode with tree-sitter support."
  (if (tree-sitter-should-enable-p)
      (progn
        (setq-local tree-sitter-font-lock-defaults
                    '((ts-c-tree-sitter-settings-1))

                    font-lock-defaults
                    '(nil t)

                    indent-line-function
                    #'tree-sitter-indent

                    tree-sitter-simple-indent-rules
                    ts-c-tree-sitter-indent-rules)
        (tree-sitter-font-lock-enable))
    ;; Copied from cc-mode.
    (setq-local font-lock-defaults
                '((c-font-lock-keywords
                   c-font-lock-keywords-1
                   c-font-lock-keywords-2
                   c-font-lock-keywords-3)
                  nil nil
                  ((95 . "w")
                   (36 . "w"))
                  c-beginning-of-syntax
                  (font-lock-mark-block-function . c-mark-function)))))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.tsc\\'" . ts-c-mode))

(defvar ts-c-tree-sitter-indent-rules
  `((tree-sitter-c
     ;; Empty line.
     (no-node prev-line 0)

     ;; Function/struct definition body {}.
     ((match nil "function_definition" "body") parent 0)
     ((node-is "field_declaration_list") parent 0)

     ;; Call expression.
     ((parent-is "call_expression") parent 2)

     ;; If-else.
     ((match nil "if_statement" "condition") parent 2)
     ((match nil "if_statement" "consequence") parent 2)
     ((match nil "if_statement" "alternative") parent 2)
     ((match nil "switch_statement" "condition")  parent 2)
     ((node-is "else") parent 0)

     ;; Switch case.
     ((parent-is "case_statement") parent 2)
     ((node-is "case_statement") parent 0)

     ;; { and }.
     ((node-is "compound_statement") parent 2)
     ((node-is "}") parent 0)

     ;; Multi-line string.
     ((parent-is "string_literal") no-indent 0)

     ;; List.
     ,@(cl-loop for type in '("compound_statement" "initializer_list"
                              "argument_list" "parameter_list"
                              "field_declaration_list")
                collect `((match nil ,type nil 0 0) parent 2)
                collect `((match nil ,type nil 1) first-sibling 0)))))

(defvar ts-c-tree-sitter-settings-1
  '((tree-sitter-c
     ((null) @font-lock-constant-face
      (true) @font-lock-constant-face
      (false) @font-lock-constant-face

      (comment) @font-lock-comment-face

      (system_lib_string) @ts-c-fontify-system-lib

      (unary_expression
       operator: _ @font-lock-negation-char-face)

      (string_literal) @font-lock-string-face
      (char_literal) @font-lock-string-face



      (function_definition
       declarator: (identifier) @font-lock-function-name-face)

      (declaration
       declarator: (identifier) @font-lock-function-name-face)

      (function_declarator
       declarator: (identifier) @font-lock-function-name-face)



      (init_declarator
       declarator: (identifier) @font-lock-variable-name-face)

      (parameter_declaration
       declarator: (identifier) @font-lock-variable-name-face)

      (preproc_def
       name: (identifier) @font-lock-variable-name-face)

      (enumerator
       name: (identifier) @font-lock-variable-name-face)

      (field_identifier) @font-lock-variable-name-face

      (parameter_list
       (parameter_declaration
        (identifier) @font-lock-variable-name-face))

      (pointer_declarator
       declarator: (identifier) @font-lock-variable-name-face)

      (array_declarator
       declarator: (identifier) @font-lock-variable-name-face)

      (preproc_function_def
       name: (identifier) @font-lock-variable-name-face
       parameters: (preproc_params
                    (identifier) @font-lock-variable-name-face))



      (type_identifier) @font-lock-type-face
      (primitive_type) @font-lock-type-face

      "auto" @font-lock-keyword-face
      "break" @font-lock-keyword-face
      "case" @font-lock-keyword-face
      "const" @font-lock-keyword-face
      "continue" @font-lock-keyword-face
      "default" @font-lock-keyword-face
      "do" @font-lock-keyword-face
      "else" @font-lock-keyword-face
      "enum" @font-lock-keyword-face
      "extern" @font-lock-keyword-face
      "for" @font-lock-keyword-face
      "goto" @font-lock-keyword-face
      "if" @font-lock-keyword-face
      "register" @font-lock-keyword-face
      "return" @font-lock-keyword-face
      "sizeof" @font-lock-keyword-face
      "static" @font-lock-keyword-face
      "struct" @font-lock-keyword-face
      "switch" @font-lock-keyword-face
      "typedef" @font-lock-keyword-face
      "union" @font-lock-keyword-face
      "volatile" @font-lock-keyword-face
      "while" @font-lock-keyword-face

      "long" @font-lock-type-face
      "short" @font-lock-type-face
      "signed" @font-lock-type-face
      "unsigned" @font-lock-type-face

      "#include" @font-lock-preprocessor-face
      "#define" @font-lock-preprocessor-face
      "#ifdef" @font-lock-preprocessor-face
      "#ifndef" @font-lock-preprocessor-face
      "#endif" @font-lock-preprocessor-face
      "#else" @font-lock-preprocessor-face
      "#elif" @font-lock-preprocessor-face
      ))))

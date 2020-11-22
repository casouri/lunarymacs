;; -*- lexical-binding: t -*-
;;
;; from doom-emacs
;;

(use-package composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; with characters begin with key, send matched to harfbuzz
  (when (version<= "27.0" emacs-version)
    (let ((alist
           '((?!  . "\\(?:!\\(?:===?\\|[!=]\\)\\)")
             (?*  . "\\(?:\\*\\(?:\\*[*/]\\|[)*/>]\\)?\\)")                            ; (regexp-opt '("*" "**" "***" "**/" "*/" "*>" "*)"))
             (?+  . "\\(?:\\++\\)\\([>:]\\)?")
             (?-  . "-+\\(?:>>?-*\\||\\|:\\)")
             (?.  . "\\(?:\\.\\(?:\\.[.<]\\|[.=>-]\\)\\)")                             ; (regexp-opt '(".-" ".." "..." "..<" ".=" ".>"))
             (?/  . "\\(?:/\\(?:\\*\\*\\|//\\|==\\|[*/=>]\\)\\)")                      ; (regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>"))
             (?:  . "\\(?::\\(?:::\\|[+:<=>]\\)?\\)")                                  ; (regexp-opt '(":" "::" ":::" ":=" ":<" ":=" ":>" ":+"))
             (?\; . ";;")                                                              ; (regexp-opt '(";;"))
             (?0  . "0\\(?:\\(x[a-fA-F0-9]\\).?\\)") ; Tries to match the x in 0xDEADBEEF
             ;; (?x . "x") ; Also tries to match the x in 0xDEADBEEF
             ;; (regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<**>" "<+" "<+>" "<-" "<--" "<---" "<->" "<-->" "<--->" "</" "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=<" "<==" "<=>" "<===>" "<>" "<|" "<|>" "<~" "<~~" "<." "<.>" "<..>"))
             (?<  . "\\(?:<\\(?:!--\\|\\$>\\|\\*\\(?:\\*?>\\)\\|\\+>\\|-\\(?:-\\(?:->\\|[>-]\\)\\|[>-]\\)\\|\\.\\(?:\\.?>\\)\\|/>\\|<[<=-]\\|=\\(?:==>\\|[<=>]\\)\\||>\\|~~\\|[$*+./<=>|~-]\\)\\)")
             (?=  . "=+>?")
             (?>  . "\\(?:>\\(?:->\\|=>\\|>[=>-]\\|[:=>-]\\)\\)")                      ; (regexp-opt '(">-" ">->" ">:" ">=" ">=>" ">>" ">>-" ">>=" ">>>"))
             (??  . "\\(?:\\?[.:=?]\\)")                                               ; (regexp-opt '("??" "?." "?:" "?="))
             (?\[ . "\\(?:\\[\\(?:|]\\|[]|]\\)\\)")                                    ; (regexp-opt '("[]" "[|]" "[|"))
             (?\\ . "\\(?:\\\\\\\\[\\n]?\\)")                                          ; (regexp-opt '("\\\\" "\\\\\\" "\\\\n"))
             (?^  . "\\(?:\\^==?\\)")                                                  ; (regexp-opt '("^=" "^=="))
             (?{  . "\\(?:{\\(?:|\\(?:|}\\|[|}]\\)\\|[|-]\\)\\)")                      ; (regexp-opt '("{-" "{|" "{||" "{|}" "{||}"))
             (?|  . "\\(?:|\\(?:->\\|=>\\||=\\|[]=>|}-]\\)\\)")                        ; (regexp-opt '("|=" "|>" "||" "||=" "|->" "|=>" "|]" "|}" "|-"))
             (?_  . "\\(?:_\\(?:|?_\\)\\)")                                            ; (regexp-opt '("_|_" "__"))
             (?\( . "\\(?:(\\*\\)")                                                    ; (regexp-opt '("(*"))
             (?~  . "\\(?:~\\(?:~>\\|[>~]\\)\\)")) ; (regexp-opt '("~>" "~~>" "~~"))
           ))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

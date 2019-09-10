;;-*-coding: utf-8;-*-

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(("down" "↓"    nil 0)
    ("up" "↑"      nil 0)
    ("left" "←"    nil 0)
    ("right" "→"   nil 0)

    ("ctrl" "⌃"    nil 0)
    ("cmd" "⌘"     nil 0)
    ("shift" "⇧"   nil 0)
    ("option" "⌥"  nil 0)

    ;; Captials are automatic
    ("alpha" "α"   nil 0)
    ("beta" "β"    nil 0)
    ("gamma" "γ"   nil 0)
    ("delta" "δ"   nil 3)
    ("epsilon" "ε" nil 0)
    ("zeta" "ζ"     nil 0)
    ("eta" "η"     nil 0)
    ("theta" "θ"   nil 0)
    ("iota" "ι"    nil 0)
    ("kappa" "κ"   nil 0)
    ("lambda" "λ"  nil 0)
    ("mu" "μ"      nil 0)
    ("nu" "ν"      nil 0)
    ("xi" "ξ"      nil 0)
    ("omicron" "ο" nil 0)
    ("pi" "π"      nil 0)
    ("rho" "ρ"     nil 0)
    ("sigma" "σ"   nil 0)
    ("tau" "τ"     nil 0)
    ("upsilon" "υ"  nil 0)
    ("phi" "φ"     nil 0)
    ("chi" "χ"      nil 0)
    ("psi" "ψ"     nil 0)
    ("omega" "ω"   nil 0))
  "Global abbrev table.")

(define-abbrev-table 'org-mode-abbrev-table '())

#lang racket/base

(require redex/reduction-semantics
         racket/match
         redex/pict
         unstable/gui/redex
         pict
         (for-syntax racket/base)
         "lang.rkt"
         "type-rep.rkt"
         "inhabitation.rkt"
         "metafunctions.rkt"
         "testing.rkt")

(provide (all-defined-out))


(define mf-font "Latin Modern Mono")
(define math-font "Latin Modern Math")
(define lit-font "Latin Modern Mono Caps")
(non-terminal-style "Latin Modern Math")
(non-terminal-subscript-style (cons 'subscript "Latin Modern Math"))
(non-terminal-superscript-style (cons 'superscript "Latin Modern Math"))
(default-style "Latin Modern Math")
(literal-style "Latin Modern Math")
(paren-style "Latin Modern Math")
(grammar-style "Latin Modern Math")


(add-atomic-rewriters!
 'idx    (λ () (text "i" math-font (default-font-size)))
 'Bot    (λ () (text "𝟘" math-font (default-font-size)))
 'Top    (λ () (text "𝟙" math-font (default-font-size)))
 'Int    (λ () (text "Int" lit-font (default-font-size)))
 'True   (λ () (text "True" lit-font (default-font-size)))
 'False  (λ () (text "False" lit-font (default-font-size)))
 'Str    (λ () (text "Str" lit-font (default-font-size)))
 'Any    (λ () (text "Any" lit-font (default-font-size)))
 'Empty    (λ () (text "Empty" lit-font (default-font-size)))
 'Any-t   (λ () (text "⊤" lit-font (default-font-size)))
 'Empty-t (λ () (text "⊥" lit-font (default-font-size)))
 'Any-Base-t   (λ () (hbl-append (text "⊤" lit-font (default-font-size))
                                 (text "ι" '(superscript . symbol) (default-font-size))))
 'Any-Prod-t   (λ () (hbl-append (text "⊤" lit-font (default-font-size))
                                 (text "×" '(superscript . symbol) (default-font-size))))
 'Any-Fun-t   (λ () (hbl-append (text "⊤" lit-font (default-font-size))
                                (text "→" '(superscript . symbol) (default-font-size))))
 '+      (λ () (text "+" mf-font (default-font-size)))
 '-      (λ () (text "-" mf-font (default-font-size)))
 'Prodb (λ () (hbl-append -3 (text "b" math-font (default-font-size))
                          (text "×" '(superscript . symbol) (default-font-size))))
 'Arrowb (λ () (hbl-append -3 (text "b" math-font (default-font-size))
                           (text "→" '(superscript . symbol) (default-font-size))))
 'Base-or   (λ () (text "∪" math-font (default-font-size)))
 'Base-and  (λ () (text "∩" math-font (default-font-size)))
 'Base-diff (λ () (text "\\" math-font (default-font-size)))
 'b-or   (λ () (text "∪" math-font (default-font-size)))
 'b-and  (λ () (text "∩" math-font (default-font-size)))
 'b-diff (λ () (text "\\" math-font (default-font-size)))
 'b-not (λ () (text "¬" math-font (default-font-size)))
 't-or   (λ () (text "∪" math-font (default-font-size)))
 't-and  (λ () (text "∩" math-font (default-font-size)))
 't-diff (λ () (text "\\" math-font (default-font-size)))
 't-not   (λ () (text "¬" math-font (default-font-size)))
 'emptyP (λ () (hbl-append (text "empty" mf-font (default-font-size))
                           (text "×" (cons 'superscript mf-font) (default-font-size))))
 'emptyA (λ () (hbl-append (text "empty" mf-font (default-font-size))
                           (text "→" (cons 'superscript mf-font) (default-font-size))))
 'empty (λ () (text "empty" mf-font (default-font-size)))
 'and (λ () (text "and" math-font (default-font-size)))
 'or (λ () (text "or" math-font (default-font-size)))
 'true (λ () (text "true" mf-font (default-font-size)))
 'false (λ () (text "false" mf-font (default-font-size)))
 'emptyP-aux (λ () (hbl-append (text "θ" math-font (default-font-size))
                               (text "×" (cons 'superscript mf-font) (default-font-size))))
 'emptyA-aux (λ () (hbl-append (text "θ" math-font (default-font-size))
                               (text "→" (cons 'superscript mf-font) (default-font-size))))
 'empty (λ () (text "empty" mf-font (default-font-size)))
 'subtype (λ () (text "<:" math-font (default-font-size)))
 '<: (λ () (text "<:" math-font (default-font-size)))
 'node (λ () (text "⟨_,_,_,_⟩" math-font (default-font-size)))
 'atom (λ () (text "a" mf-font (default-font-size)))
 'left (λ () (text "l" mf-font (default-font-size)))
 'mid (λ () (text "m" mf-font (default-font-size)))
 'right (λ () (text "r" mf-font (default-font-size)))
 'parse (λ () (text "parse" mf-font (default-font-size)))
 'Any-Pair (λ () (hbl-append (text "Any" lit-font (default-font-size))
                             (text "×" (cons 'superscript math-font) (default-font-size))))
 'Any-Fun (λ () (hbl-append (text "Any" lit-font (default-font-size))
                            (text "→" (cons 'superscript math-font) (default-font-size))))
 'Any-Base (λ () (hbl-append (text "Any" lit-font (default-font-size))
                             (text "ι" (cons 'superscript math-font) (default-font-size))))
 'select (λ () (text "select" mf-font (default-font-size)))
 'maybe-project (λ () (hbl-append (text "proj" mf-font (default-font-size))
                                  (text "?" (cons 'superscript mf-font) (default-font-size))))
 'project (λ () (text "proj" mf-font (default-font-size)))
 'project-aux (λ () (hbl-append (text "ϕ" math-font (default-font-size))
                                (text "×" (cons 'superscript math-font) (default-font-size))))
 'maybe-domain (λ () (hbl-append (text "dom" mf-font (default-font-size))
                                 (text "?" (cons 'superscript mf-font) (default-font-size))))
 'domain (λ () (text "dom" mf-font (default-font-size)))
 'maybe-funapp (λ () (hbl-append (text "apply" mf-font (default-font-size))
                                 (text "?" (cons 'superscript mf-font) (default-font-size))))
 'funapp (λ () (text "apply" mf-font (default-font-size)))

 ;; peano arith example lang
 'peano-plus (λ () (text "plus" mf-font (default-font-size)))
 'peano-lt (λ () (text "_ < _" math-font (default-font-size)))
 'peano-gt (λ () (text "_ > _" math-font (default-font-size)))
 'O (λ () (text "O" math-font (default-font-size)))
 'S (λ () (text "S" math-font (default-font-size)))
 )
(add-compound-rewriters!
 'less-than (binary-rw (text " < " math-font (default-font-size)))
 'greater-than (binary-rw (text " > " math-font (default-font-size)))
 'equal-to (binary-rw (text " = " math-font (default-font-size)))
 'Set-union (binary-rw (text " ∪ " math-font (default-font-size)))
 'Set-intersect (binary-rw (text " ∩ " math-font (default-font-size)))
 'Set-diff (binary-rw (text " \\ " math-font (default-font-size)))
 'Base-or (binary-rw (text " ∪ " math-font (default-font-size)))
 'Base-and (binary-rw (text " ∩ " math-font (default-font-size)))
 'Base-diff (binary-rw (text " \\ " math-font (default-font-size)))
 
 'b-diff (binary-rw (text " \\ " math-font (default-font-size)) #:parenthesize-arg '(b-and b-or))
 'b-and (binary-rw (text " ∩ " math-font (default-font-size)) #:parenthesize-arg '(b-or b-diff))
 'b-or (binary-rw (text " ∪ " math-font (default-font-size)) #:parenthesize-arg '(b-and b-diff))
 'b-not (prefix-rw (text "¬" math-font (default-font-size)) #:parenthesize-arg '(b-and b-or b-diff))
 '× (binary-rw (text " × " math-font (default-font-size)))
 '→ (binary-rw (text " → " math-font (default-font-size)))
 
 
 'Node (bracket-rw 'angle)
 'node (bracket-rw 'angle)
 'Type (bracket-rw 'angle)
 'Base (bracket-rw 'angle)
 'Set (bracket-rw 'curly)
 'Fun (binary-rw " → " #:parenthesize-arg '(Fun Pair And Or))
 'Pair (binary-rw " × " #:parenthesize-arg '(Fun Pair And Or))
 'Not (prefix-rw "¬ " #:parenthesize-arg '(Fun Pair And Or))
 'Or (binary-rw (text " ∨ " math-font (default-font-size))
                #:parenthesize-arg '(Fun Pair And))
 'And (binary-rw (text " ∧ " math-font (default-font-size))
                 #:parenthesize-arg '(Fun Pair Or))
 '¬ (prefix-rw (text "¬" 'symbol (default-font-size))
               #:parenthesize-arg '(× →))
 'atom (function-rw (text "a" mf-font (default-font-size)))
 'left (function-rw (text "l" mf-font (default-font-size)))
 'mid (function-rw (text "m" mf-font (default-font-size)))
 'right (function-rw (text "r" mf-font (default-font-size)))
 
 't-and (binary-rw (text " ∩ " math-font (default-font-size))
                   #:parenthesize-arg '(T-or T-diff))
 't-or (binary-rw (text " ∪ " math-font (default-font-size))
                  #:parenthesize-arg '(T-and T-diff))
 't-diff (binary-rw (text " \\ " math-font (default-font-size))
                    #:parenthesize-arg '(T-and T-or))
 't-not (prefix-rw (text "¬" math-font (default-font-size))
                   #:parenthesize-arg '(T-and T-or T-diff))
   
 'AND (splice-rw)
 'OR (splice-rw)
 'PAND (bracket-rw 'round #:comma? #f)
 'POR (bracket-rw 'round #:comma? #f)
 'empty (function-rw (text "empty" mf-font (default-font-size)))
 'emptyP (function-rw (hbl-append (text "empty" mf-font (default-font-size))
                                  (text "×" (cons 'superscript mf-font) (default-font-size))))
 'emptyA (function-rw (hbl-append (text "empty" mf-font (default-font-size))
                                  (text "→" (cons 'superscript mf-font) (default-font-size))))
 'emptyP-aux (function-rw (hbl-append (text "θ" math-font (default-font-size))
                                      (text "×" (cons 'superscript mf-font) (default-font-size))))
 'emptyA-aux (function-rw (hbl-append (text "θ" math-font (default-font-size))
                                      (text "→" (cons 'superscript mf-font) (default-font-size))))
 'subtype (binary-rw (text " <: " math-font (default-font-size)))
 '<: (binary-rw (text " <: " math-font (default-font-size)))
   
 'set-cons (set-cons-rw)
 'parse (function-rw (text "parse" mf-font (default-font-size)))
 'select (function-rw (text "select" mf-font (default-font-size)))
 'maybe-project (function-rw (hbl-append (text "proj" mf-font (default-font-size))
                                         (text "?" (cons 'superscript mf-font) (default-font-size))))
 'project (function-rw (text "proj" mf-font (default-font-size)))
 'project-aux (function-rw (hbl-append (text "ϕ" math-font (default-font-size))
                                       (text "×" (cons 'superscript math-font) (default-font-size))))
 'maybe-domain (function-rw (hbl-append (text "dom" mf-font (default-font-size))
                                        (text "?" (cons 'superscript mf-font) (default-font-size))))
 'domain (function-rw (text "dom" mf-font (default-font-size)))
 'maybe-funapp (function-rw (hbl-append (text "apply" mf-font (default-font-size))
                                             (text "?" (cons 'superscript mf-font) (default-font-size))))
 'funapp (function-rw (text "apply" mf-font (default-font-size)))
 ;; peano arith example lang
 'peano-plus (function-rw (text "plus" mf-font (default-font-size)))
 'peano-lt (binary-rw (text " < " math-font (default-font-size)))
 'peano-gt (binary-rw (text " > " math-font (default-font-size)))
 )

;; we only use unquote for binary ops -- remove pink from those
(define (remove-some-pink x)
  (match x
    [(lw (list (app remove-some-pink xs) ...) l l-span col col-span unq? mf?)
     (lw xs l l-span col col-span unq? mf?)]
    [(lw (and literal (or 'or 'and "or" "and" "(" ")" ""))
         l l-span col col-span #t mf?)
     (lw literal l l-span col col-span #f mf?)]
    [_ x]))

(add-unquote-rewriters!
 (match-lambda
  [(lw (list (lw "(" _ _ _ _ #t _)
             (lw (or 'AND 'OR 'PAND 'POR "OR" "AND" "PAND" "POR") _ _ _ _ #t _)
             _ ...)
       _ _ _ _ #t _)
   #t]
  [_ #f])
 remove-some-pink)

(metafunction-pict-style  'left-right/vertical-side-conditions)

(define default-scale 1.5)

(define-syntax-rule (define-metafunction-renderers [fun mf-name] ...)
  (begin
    (provide fun ...)
    (define (fun [scalar default-scale])
      (scale (with-rewriters
                 (λ () (render-metafunction mf-name #:contract? #t)))
             scalar))
    ...))



(define (render-nts nts [scalar default-scale])
  (apply
   vl-append
   10
   (for*/list ([entry (in-list nts)]
               [nt-name (in-value (car entry))]
               [nt-syms (in-value (cdr entry))])
     (vl-append
      (scale (text nt-name math-font) scalar)
      (scale (with-rewriters
                 (λ () (render-language sst #:nts nt-syms)))
             scalar)))))
  

(define-syntax-rule (render-semantic-types-term t)
  (scale (with-rewriters (λ () (render-term sst t)))
         default-scale))

(define (mf name [size (default-font-size)])
  (scale (text name mf-font size) default-scale))


(define-language peano
  [p q ::= O (S p)]
  [bool ::= #true #false])


(define-metafunction peano
  peano-plus : p p -> p
  [(peano-plus O p) p]
  [(peano-plus (S p_1) p_2)
   (S q)
   (where q (peano-plus p_1 p_2))])

(define-metafunction peano
  peano-lt : p p -> bool
  [(peano-lt (S p) (S q)) (peano-lt p q)]
  [(peano-lt _ O) #false]
  [(peano-lt _ _) #true])

(define-metafunction peano
  peano-gt : p p -> bool
  [(peano-gt p q) #true
                  (side-condition (term (peano-lt q p)))]
  [(peano-gt _ _) #false])

(define-syntax (render= stx)
  (syntax-case stx ()
    [(_ lang term1 term2s)
     (syntax/loc stx (render= lang term1 term2s #:no-newline))]
    [(_ lang term1 term2 kw)
     (unless (or (eq? '#:no-newline (syntax-e #'kw))
                 (eq? '#:newline (syntax-e #'kw)))
       (raise-syntax-error
        'render=
        "expected 4th argument to be #:no-newline or #:newline"
        #'kw
        stx))
     (let ([newline? (eq? '#:newline (syntax-e #'kw))])
       (with-syntax ([err (syntax/loc stx (error 'render= "~a and ~a are not equal!"
                                                 '(term term1)
                                                 '(term term2)))])
         (quasisyntax/loc stx
           (begin
             (unless (equal? (term term1 #:lang lang)
                             (term term2 #:lang lang))
               err)
             (scale (with-rewriters
                        (λ () (#,(if newline? #'vl-append #'hbl-append)
                               5
                               (hbl-append 5
                                           (render-term lang term1)
                                           (text "≡" "Latin Modern Math"))
                               (hbl-append
                                #,(if newline? #'(blank 15 5) #'(blank 0 0))
                               (render-term lang term2)))))
                    default-scale)))))]
    ))

(define-syntax (render≈ stx)
  (syntax-case stx ()
    [(_ type1 type2)
     (with-syntax ([err (syntax/loc stx (error 'render≈ "~a and ~a are not equivalent!"
                                               '(term type1)
                                               '(term type2)))])
       (syntax/loc stx
         (begin
           (unless (judgment-holds (≈ type1 type2))
             err)
           (scale (with-rewriters
                      (λ () (hbl-append 5
                                        (render-term sst type1)
                                        (text "≡" "Latin Modern Math")
                                        (render-term sst type2))))
                  default-scale))))]))

(define-syntax (render-valid-judgment stx)
  (syntax-case stx ()
    [(_ judgment-statement)
     (with-syntax ([err (syntax/loc stx (error 'render-valid-judgment
                                               "~a does not hold!"
                                               'term1))])
       (syntax/loc stx
         (begin
           (unless (judgment-holds judgment-statement)
             err)
           (scale (with-rewriters
                      (λ () (render-term sst judgment-statement)))
                  default-scale))))]))


(define-metafunction-renderers
  ;; Peano arith example functions
  [render-mf-peano-plus peano-plus]
  [render-mf-peano-lt peano-lt]
  [render-mf-peano-gt peano-gt]
  ;; sst functions
  [render-mf-Base-or Base-or]
  [render-mf-Base-and Base-and]
  [render-mf-Base-diff Base-diff]
  [render-mf-b-or b-or]
  [render-mf-b-and b-and]
  [render-mf-b-diff b-diff]
  [render-mf-b-not b-not]
  [render-mf-t-or t-or]
  [render-mf-t-and t-and]
  [render-mf-t-diff t-diff]
  [render-mf-t-not t-not]
  [render-mf-empty-prod emptyP]
  [render-mf-empty-prod-aux emptyP-aux]
  [render-mf-empty-arrow emptyA]
  [render-mf-empty-arrow-aux emptyA-aux]
  [render-mf-empty empty]
  [render-mf-subtype subtype]
  [render-mf-node node]
  [render-mf-atom atom]
  [render-mf-left left]
  [render-mf-mid mid]
  [render-mf-right right]
  [render-mf-parse parse]
  [render-mf-select select]
  [render-mf-maybe-project maybe-project]
  [render-mf-project project]
  [render-mf-project-aux project-aux]
  [render-mf-maybe-domain maybe-domain]
  [render-mf-domain domain]
  [render-mf-maybe-funapp maybe-funapp]
  [render-mf-funapp funapp]
  )


(define (render-peano-grammar [scalar default-scale])
  (scale (with-rewriters
             (λ ()
               (vl-append
                10
                (vl-append
                 (text "Peano Natural Numbers" math-font)
                 (render-language peano #:nts '(p q)))
                (vl-append
                 (text "Booleans" math-font)
                 (render-language peano #:nts '(bool))))))
         scalar))


(define-syntax-rule (render-sst-term t)
  (scale (with-rewriters (λ () (render-term sst t))) default-scale))
(define-syntax-rule (render-peano-term t)
  (scale (with-rewriters (λ () (render-term peano t))) default-scale))
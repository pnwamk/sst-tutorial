#lang racket/base

(require redex/reduction-semantics
         racket/match)

(provide (all-defined-out))



(define-language sst
  ;; base types
  [ι ::= Int Str True False]
  ;; user-level type syntax
  [τ σ ::= ι (Pair τ τ) (Fun τ τ) (Or τ τ) (And τ τ) (Not τ) Any Empty]
  ;; types (efficient representation)
  [t s ::= (Type β Prodb Arrowb)]
  [β ::= (Base ± B)]
  [B ::= (Set ι ...)]
  [± ::= + -]
  ;; Lazy Binary Decision Diagrams (BDD)
  [b ::= Top Bot n]
  [n ::= (Node a b b b)]
  [a ::= (× t t) (→ t t)]
  [Prodb  ::= Top Bot (Node (× t t) Prodb Prodb Prodb)]
  [Arrowb ::= Top Bot (Node (→ t t) Arrowb Arrowb Arrowb)]
  ;; Base type union/negation
  [P ::= ∅ (set-cons a P)]
  [N ::= ∅ (set-cons a N)]
  [bool ::= #true #false]
  [idx ::= 1 2])

(define-term Any-Pair (Pair Any Any))
(define-term Any-Fun  (Fun Empty Any))
(define-term Any-Base (Not (Or Any-Pair Any-Fun)))

(define-judgment-form sst
  #:mode (equal-to I I)
  #:contract (equal-to any any)
  [------------------
   (equal-to any_1 any_1)])


;; an ordering on symbol/null/cons trees
;; sym < nil < cons
(define (raw-term<? t1 t2)
  (match* (t1 t2)
    [(_ _)
     #:when (eq? t1 t2)
     #f]
    [((? symbol? x) (? symbol? y))
     (symbol<? x y)]
    [((? symbol? x) _) #t]
    [((list) _) #t]
    [((cons x xs) (cons y ys))
     (cond
       [(raw-term<? x y) #t]
       [(raw-term<? y x) #f]
       [else (raw-term<? xs ys)])]
    [((cons _ _) _) #f]))


(define-judgment-form sst
  #:mode (less-than I I)
  #:contract (less-than any any)
  [(where #t ,(raw-term<? (term any_1) (term any_2)))
   -----------------------
   (less-than any_1 any_2) ])

(define-judgment-form sst
  #:mode (greater-than I I)
  #:contract (greater-than any any)
  [(less-than any_2 any_1)
   -----------------------
   (greater-than any_1 any_2)])
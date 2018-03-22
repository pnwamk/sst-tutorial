#lang racket/base

(require "lang.rkt"
         redex/reduction-semantics
         (only-in racket/set set-union set-intersect set-subtract))


(provide (all-defined-out))


(define-metafunction sst
  Base-t : ι -> t
  [(Base-t ι) (Type (Base + (Set ι)) Bot Bot)])

(define-metafunction sst
  Prod-t : t t -> t
  [(Prod-t t_1 t_2) (Type (Base + (Set)) (Node (× t_1 t_2) Top Bot Bot) Bot)])

(define-metafunction sst
  Arrow-t : t t -> t
  [(Arrow-t t_1 t_2) (Type (Base + (Set)) Bot (Node (→ t_1 t_2) Top Bot Bot))])


(define-metafunction sst
  parse : τ -> t
  [(parse ι) (Type (Base + (Set ι)) Bot Bot)]
  [(parse (Pair τ σ)) (Type (Base + (Set)) (Node (× t_1 t_2) Top Bot Bot) Bot)
                      (where t_1 (parse τ))
                      (where t_2 (parse σ))]
  [(parse (Fun τ σ)) (Type (Base + (Set)) Bot (Node (→ t_1 t_2) Top Bot Bot))
                     (where t_1 (parse τ))
                     (where t_2 (parse σ))]
  [(parse (Or τ σ)) (t-or (parse τ) (parse σ))]
  [(parse (And τ σ)) (t-and (parse τ) (parse σ))]
  [(parse (Not τ)) (t-not (parse τ))]
  [(parse Any) (Type (Base - (Set)) Top Top)]
  [(parse Empty) (Type (Base + (Set)) Bot Bot)])


(define-metafunction sst
  Set-union : (Set any ...) (Set any ...) -> (Set any ...)
  [(Set-union (Set any_1 ...) (Set any_2 ...))
   (Set ,@(set-union (term (any_1 ...)) (term (any_2 ...))))])

(define-metafunction sst
  Set-intersect : (Set any ...) (Set any ...) -> (Set any ...)
  [(Set-intersect (Set any_1 ...) (Set any_2 ...))
   (Set ,@(set-intersect (term (any_1 ...)) (term (any_2 ...))))])

(define-metafunction sst
  Set-diff : (Set any ...) (Set any ...) -> (Set any ...)
  [(Set-diff (Set any_1 ...) (Set any_2 ...))
   (Set ,@(set-subtract (term (any_1 ...)) (term (any_2 ...))))])

(define-metafunction sst
  Base-or : β β -> β
  [(Base-or (Base + B_1) (Base + B_2))
   (Base + (Set-union B_1 B_2))]
  [(Base-or (Base - B_1) (Base - B_2))
   (Base - (Set-intersect B_1 B_2))]
  [(Base-or (Base + B_1) (Base - B_2))
   (Base - (Set-diff B_2 B_1))]
  [(Base-or (Base - B_1) (Base + B_2))
   (Base - (Set-diff B_1 B_2))])

(define-metafunction sst
  Base-and : β β -> β
  [(Base-and (Base + B_1) (Base + B_2))
   (Base + (Set-intersect B_1 B_2))]
  [(Base-and (Base - B_1) (Base - B_2))
   (Base - (Set-union B_1 B_2))]
  [(Base-and (Base + B_1) (Base - B_2))
   (Base + (Set-diff B_1 B_2))]
  [(Base-and (Base - B_1) (Base + B_2))
   (Base + (Set-diff B_2 B_1))])

(define-metafunction sst
  Base-diff : β β -> β
  [(Base-diff (Base + B_1) (Base + B_2))
   (Base + (Set-diff B_1 B_2))]
  [(Base-diff (Base - B_1) (Base - B_2))
   (Base + (Set-diff B_2 B_1))]
  [(Base-diff (Base + B_1) (Base - B_2))
   (Base + (Set-intersect B_1 B_2))]
  [(Base-diff (Base - B_1) (Base + B_2))
   (Base - (Set-union B_1 B_2))])

(define-metafunction sst
  node : a b b b -> b
  [(node a b_l Top b_r) Top]
  [(node a b b_m b) (b-or b b_m)]
  [(node a b_l b_m b_r) (Node a b_l b_m b_r)])

(define-metafunction sst
  atom : n -> a
  [(atom (Node a b_l b_m b_r)) a])

(define-metafunction sst
  left : n -> b
  [(left (Node a b_l b_m b_r)) b_l])

(define-metafunction sst
  mid : n -> b
  [(mid (Node a b_l b_m b_r)) b_m])

(define-metafunction sst
  right : n -> b
  [(right (Node a b_l b_m b_r)) b_r])

(define-metafunction sst
  b-or : b b -> b
  [(b-or b b) b]
  [(b-or b Top) Top]
  [(b-or Top b) Top]
  [(b-or b Bot) b]
  [(b-or Bot b) b]
  [(b-or n_1 n_2)
   (node (atom n_1) (left n_1) (b-or (mid n_1) n_2) (right n_1))
   (judgment-holds (less-than (atom n_1) (atom n_2)))]
  [(b-or n_1 n_2)
   (node (atom n_2) (left n_2) (b-or (mid n_2) n_1) (right n_2))
   (judgment-holds (greater-than (atom n_1) (atom n_2)))]
  [(b-or n_1 n_2)
   (node (atom n_1) (b-or (left n_1) (left n_2)) (b-or (mid n_1) (mid n_2)) (b-or (right n_1) (right n_2)))
   (judgment-holds (equal-to (atom n_1) (atom n_2)))])


(define-metafunction sst
  b-and : b b -> b
  [(b-and b b) b]
  [(b-and b Top) b]
  [(b-and Top b) b]
  [(b-and b Bot) Bot]
  [(b-and Bot b) Bot]
  [(b-and n_1 n_2)
   (node (atom n_1) (b-and (left n_1) n_2) (b-and (mid n_1) n_2) (b-and (right n_1) n_2))
   (judgment-holds (less-than (atom n_1) (atom n_2)))]
  [(b-and n_1 n_2)
   (node (atom n_2) (b-and n_1 (left n_2)) (b-and n_1 (mid n_2)) (b-and n_1 (right n_2)))
   (judgment-holds (greater-than (atom n_1) (atom n_2)))]
  [(b-and n_1 n_2)
   (node (atom n_1)
         (b-and (b-or (left n_1) (mid n_1)) (b-or (left n_2) (mid n_2)))
         Bot
         (b-and (b-or (right n_1) (mid n_1)) (b-or (right n_2) (mid n_2))))
   (judgment-holds (equal-to (atom n_1) (atom n_2)))])


(define-metafunction sst
  b-not : b -> b
  [(b-not Top) Bot]
  [(b-not Bot) Top]
  [(b-not (Node a b_1 b_2 Bot))
   (node a Bot (b-not (b-or b_2 b_1)) (b-not b_2))]
  [(b-not (Node a Bot b_2 b_3))
   (node a (b-not b_2) (b-not (b-or b_2 b_3)) Bot)]
  [(b-not (Node a b_1 Bot b_3))
   (node a (b-not b_1) (b-not (b-or b_1 b_3)) (b-not b_3))]
  [(b-not (Node a_1 b_1 b_2 b_3))
   (node a_1 (b-not (b-or b_1 b_2)) Bot (b-not (b-or b_3 b_2)))])


(define-metafunction sst
  b-diff : b b -> b
  [(b-diff b b) Bot]
  [(b-diff b Top) Bot]
  [(b-diff Top b) (b-not b)]
  [(b-diff b Bot) b]
  [(b-diff Bot b) Bot]
  [(b-diff n_1 n_2)
   (node (atom n_1) (b-diff (b-or (left n_1) (mid n_1)) n_2) Bot (b-diff (b-or (right n_1) (mid n_1)) n_2))
   (judgment-holds (less-than (atom n_1) (atom n_2)))]
  [(b-diff n_1 n_2)
   (node (atom n_2) (b-diff n_1 (b-or (left n_2) (mid n_2))) Bot (b-diff n_1 (b-or (right n_2) (mid n_2))))
   (judgment-holds (greater-than (atom n_1) (atom n_2)))]
  [(b-diff n_1 n_2)
   (node (atom n_1) (b-diff (left n_1) n_2) (b-diff (mid n_1) n_2) (b-diff (right n_1) n_2))])

(define-metafunction sst
  t-and : t t -> t
  [(t-and (Type β_1 Prodb_1 Arrowb_1) (Type β_2 Prodb_2 Arrowb_2))
   (Type (Base-and β_1 β_2) (b-and Prodb_1 Prodb_2) (b-and Arrowb_1 Arrowb_2))])

(define-metafunction sst
  t-or : t t -> t
  [(t-or (Type β_1 Prodb_1 Arrowb_1) (Type β_2 Prodb_2 Arrowb_2))
   (Type (Base-or β_1 β_2) (b-or Prodb_1 Prodb_2) (b-or Arrowb_1 Arrowb_2))])

(define-metafunction sst
  t-diff : t t -> t
  [(t-diff (Type β_1 Prodb_1 Arrowb_1) (Type β_2 Prodb_2 Arrowb_2))
   (Type (Base-diff β_1 β_2) (b-diff Prodb_1 Prodb_2) (b-diff Arrowb_1 Arrowb_2))])

(define-metafunction sst
  t-not : t -> t
  [(t-not t) (t-diff Any-t t)])


(define-metafunction sst
  t-or* : t ... -> t
  [(t-or*) Empty-t]
  [(t-or* t) t]
  [(t-or* t s ...) (t-or t (t-or* s ...))])

(define-metafunction sst
  t-and* : t ... -> t
  [(t-and*) Any-t]
  [(t-and* t) t]
  [(t-and* t s ...) (t-and t (t-and* s ...))])



(define-term Any-t (Type (Base - (Set)) Top Top))
(define-term Empty-t (Type (Base + (Set)) Bot Bot))
(define-term Any-Prod-t (Type (Base + (Set)) Top Bot))
(define-term Any-Fun-t (Type (Base + (Set)) Bot Top))
(define-term Any-Base-t (Type (Base - (Set)) Bot Bot))
(define-term Int-t (Base-t Int))
(define-term Str-t (Base-t Str))
(define-term True-t (Base-t True))
(define-term False-t (Base-t False))
(define-term Bool-t (Or True-t False-t))
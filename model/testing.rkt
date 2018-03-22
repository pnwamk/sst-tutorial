#lang racket/base

(require "lang.rkt"
         "type-rep.rkt"
         "inhabitation.rkt"
         "metafunctions.rkt"
         redex/reduction-semantics
         (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax (redex-check-true stx)
  (syntax-case stx ()
    [(_ name body ...)
     (begin
       (unless (string? (syntax-e #'name))
         (raise-syntax-error
          'redex-check-true
          "first argument must be a string description"
          #'name))
       (quasisyntax/loc stx
         (begin
           (printf "redex-check ~a... " name)
           (flush-output)
           #,(syntax/loc stx
               (test-equal (redex-check body ... #:print? #f) #true))
           (printf "complete.\n"))))]))



;                                                                 
;                 ;                                               
;                 ;        ;                     ;                
;                 ;        ;                                      
;    ;;;   ;   ;  ;;;;   ;;;;;  ;   ;  ;;;;    ;;;   ; ;;    ;;;; 
;   ;   ;  ;   ;  ;; ;;    ;    ;   ;  ;; ;;     ;   ;;  ;  ;;  ; 
;   ;      ;   ;  ;   ;    ;     ; ;   ;   ;     ;   ;   ;  ;   ; 
;    ;;;   ;   ;  ;   ;    ;     ; ;   ;   ;     ;   ;   ;  ;   ; 
;       ;  ;   ;  ;   ;    ;     ; ;   ;   ;     ;   ;   ;  ;   ; 
;   ;   ;  ;   ;  ;; ;;    ;     ;;    ;; ;;     ;   ;   ;  ;; ;; 
;    ;;;    ;;;;  ;;;;     ;;;    ;    ;;;;    ;;;;; ;   ;   ;;;; 
;                                 ;    ;                        ; 
;                                ;     ;                     ;  ; 
;                               ;;     ;                      ;;  
;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;         


(module+ test

  ;; the following judgments/metafunctions
  ;; are used for testing purposes:
  (define-metafunction sst
  NAND : τ τ -> τ
  [(NAND τ σ) (Not (And τ σ))])

(define-judgment-form sst
  #:mode (comp<: I I)
  #:contract (comp<: τ τ)
  [(where #true (subtype (parse σ) (parse τ)))
   (where #true (nsubtype (parse σ) (parse τ)))
   ----------------
   (comp<: σ τ)])

(define-judgment-form sst
  #:mode (comp</: I I)
  #:contract (comp</: τ τ)
  [(where #false (subtype (parse σ) (parse τ)))
   (where #false (nsubtype (parse σ) (parse τ)))
   ----------------
   (comp</: σ τ)])

(define-judgment-form sst
  #:mode (mt? I)
  #:contract (mt? τ)
  [(where #true (empty (parse τ)))
   ----------------
   (mt? τ)])

(define-judgment-form sst
  #:mode (nmt? I)
  #:contract (nmt? τ)
  [(where #true (nempty (parse τ)))
   ----------------
   (nmt? τ)])
  
  (define-judgment-form sst
  #:mode (</: I I)
  #:contract (</: τ τ)
  [(where #false (subtype (parse σ) (parse τ)))
   ----------------
   (</: σ τ)])

(define-judgment-form sst
  #:mode (overlap? I I)
  #:contract (overlap? τ τ)
  [(where #false (empty (t-and (parse σ) (parse τ))))
   ----------------
   (overlap? σ τ)])

(define-judgment-form sst
  #:mode (not-overlap? I I)
  #:contract (not-overlap? τ τ)
  [(where #true (empty (t-and (parse σ) (parse τ))))
   ----------------
   (not-overlap? σ τ)])

(define-judgment-form sst
  #:mode (n<: I I)
  #:contract (n<: τ τ)
  [(where #true (nsubtype (parse σ) (parse τ)))
   ----------------
   (n<: σ τ)])

(define-judgment-form sst
  #:mode (n</: I I)
  #:contract (n</: τ τ)
  [(where #false (nsubtype (parse σ) (parse τ)))
   ----------------
   (n</: σ τ)])

                            


  ;; Any/Empty tests
  (test-judgment-holds (comp<: Empty Any))
  (test-judgment-holds (comp<: Any (Not Empty)))
  (test-judgment-holds (comp<: (Not Empty) Any))
  (test-judgment-holds (comp</: Any Empty))
  (test-judgment-holds (comp</: (Not Empty) Empty))

  ;; simple base type tests
  (test-judgment-holds (comp<: Int Int))
  (test-judgment-holds (comp<: True True))
  (test-judgment-holds (comp<: Int Any))
  (test-judgment-holds (comp<: Str Any))
  (test-judgment-holds (comp</: Any Int))
  (test-judgment-holds (comp</: Any Str))
  (test-judgment-holds (comp</: Int Str))
  ;; more base tests
  (test-judgment-holds (comp<: Int (Or Int Str)))
  (test-judgment-holds (comp<: (Or Int Str) (Or Int (Or Str True))))
  (test-judgment-holds (comp</: (Or Int Str) Int))
  (test-judgment-holds (comp</: (Or Int (Or Str True)) (Or Int Str)))
  (test-judgment-holds (comp<: (Not (Or Int Str)) (Not Int)))
  (test-judgment-holds (comp</: (Not Int) (Not (Or Int Str))))
  (test-judgment-holds (comp<: (Not (Or Int (Or Str True))) (Not (Or Int Str))))
  (test-judgment-holds (comp</: (Not (Or Int Str)) (Not (Or Int (Or Str True)))))
  (test-judgment-holds (comp</: Int (And Int Str)))
  (test-judgment-holds (comp<: (And Int Str) Empty))
  (test-judgment-holds (comp<: (And Int Str) Int))
  (test-judgment-holds (comp<: (And (Or Int Str) (Or True Str)) Str))
  (test-judgment-holds (comp<: Str (And (Or Int Str) (Or True Str))))
  
  ;; simple product tests
  (test-judgment-holds (comp<: (Pair Int Int) (Pair Int Int)))
  (test-judgment-holds (comp</: (Pair Int Int) (Pair Int Str)))
  (test-judgment-holds (comp</: (Pair Int Any) (Pair Int Int)))
  (test-judgment-holds (comp</: (Pair Int Int) (Pair Empty Int)))
  (test-judgment-holds (comp<: (Pair Int Int) (Pair Int Any)))
  (test-judgment-holds (comp<: (Pair Int Int) (Pair Any Int)))
  (test-judgment-holds (comp<: (Pair Empty Str) (Pair Int Int)))
  (test-judgment-holds (comp<: (Pair Str Empty) (Pair Int Int)))
  (test-judgment-holds (comp<: (Pair Str Empty) (Pair Int Empty)))
  (test-judgment-holds (comp<: (Pair Str Empty) (Pair Empty Int)))
  (test-judgment-holds (comp<: (Pair Int Int) (Pair Int Int)))
  (test-judgment-holds (comp</: (Or (Pair Int Any)
                                    (Pair Any Int))
                                (Pair Int Int)))
  (test-judgment-holds (comp<: (Pair Int Int)
                               (Or (Pair Int Any)
                                   (Pair Any Int))))
  (test-judgment-holds (comp<: (And (Pair Int Any)
                                    (Pair Any Int))
                               (Pair Int Int)))
  (test-judgment-holds (comp<: (Pair Int Int)
                               (And (Pair Int Any)
                                    (Pair Any Int))))
  (test-judgment-holds (comp<: (And (Pair (Or True Int)   (Or False Int))
                                    (Pair (Or True False) (Or False True)))
                               (Pair True False)))
  (test-judgment-holds (comp<: (Pair True False)
                               (And (Pair (Or True Int)   (Or False Int))
                                    (Pair (Or True False) (Or False True)))))
  (test-judgment-holds (comp</: (And (Pair (Or True Int)   (Or True False))
                                     (Pair (Or True False) (Or False True)))
                                (Pair True False)))
  (test-judgment-holds (comp</: (And (Pair (Or True Int)   (Or True False))
                                     (Pair (Or True False) (Or False True)))
                                (Pair True False)))
  (test-judgment-holds (comp</: (And (Pair (Or True False) (Or False Int))
                                     (Pair (Or True False) (Or False True)))
                                (Pair True False)))

  (test-judgment-holds (comp<: (Pair True False)
                               (And (Pair (Or True Int)   (Or True False))
                                    (Pair (Or True False) (Or False True)))))
  (test-judgment-holds (comp<: (Pair True False)
                               (And (Pair (Or True False) (Or False Int))
                                    (Pair (Or True False) (Or False True)))))
  (test-judgment-holds (comp<: (And (Pair (Or True False) (Or True False))
                                    (And (Not (Pair True True))
                                         (Not (Pair False False))))
                               (Or (Pair True False)
                                   (Pair False True))))
  (test-judgment-holds (comp<: (Or (Pair True False)
                                   (Pair False True))
                               (And (Pair (Or True False) (Or True False))
                                    (And (Not (Pair True True))
                                         (Not (Pair False False))))))
  
  ;; simple function tests
  (test-judgment-holds (comp<: (Fun Int Int) (Fun Int Int)))
  (test-judgment-holds (comp<: (Fun Any Any) (Fun Empty Any)))
  (test-judgment-holds (comp<: (Fun Any Any) (Fun Empty Empty)))
  (test-judgment-holds (comp</: (Fun Empty Any) (Fun Any Any)))
  (test-judgment-holds (comp<: (Fun Int Int) (Fun Int Any)))
  (test-judgment-holds (comp<: (Fun Any Int) (Fun Int Int)))
  (test-judgment-holds (comp</: (Fun Int Int) (Fun Any Int)))
  (test-judgment-holds (comp</: (Fun Int Any) (Fun Int Int)))
  (test-judgment-holds (comp<: (And (Fun Int Int)
                                    (Fun True False))
                               (Fun Int Int)))
  (test-judgment-holds (comp<: (And (Fun Int Int)
                                    (Fun True False))
                               (Fun True False)))
  (test-judgment-holds (comp</: (Fun Int Int)
                                (And (Fun Int Int)
                                     (Fun True False))))
  (test-judgment-holds (comp</: (Fun True False)
                                (And (Fun Int Int)
                                     (Fun True False))))
  (test-judgment-holds (comp<: (And (Fun Int Int)
                                    (Fun True False))
                               (Fun (Or True Int) (Or False Int))))
  (test-judgment-holds (comp<: (And (Fun Int True)
                                    (Fun (Not Int) False))
                               (Fun Any (Or True False))))
  (test-judgment-holds (comp</: (Fun (Or True Int) (Or False Int))
                                (And (Fun Int Int)
                                     (Fun True False))))
  (test-judgment-holds (comp<: (Or (Fun (Or True Int) Int)
                                   (Fun (Or False Int) Int))
                               (Fun Int Int)))
  (test-judgment-holds (comp<: (Or (Fun (Or True Int) Int)
                                   (Fun (Or False Int) False))
                               (Fun Int (Or False Int))))

  ;; misc
  (test-judgment-holds (comp<: (And (Or True False) (Not True)) False))
  (test-judgment-holds (comp<: False (And (Not True) (Or True False))))
  (test-judgment-holds (comp<: False (And (Or True False) (Not True))))
  (test-judgment-holds (comp<: (And (Or (Pair True True)
                                        (Or (Pair True False)
                                            (Or (Pair False True)
                                                (Pair False False))))
                                    (Not (Pair True Any)))
                               (Or (Pair False True) (Pair False False))))
  (test-judgment-holds (comp<: (And (Or (Pair True True)
                                        (Or (Pair True False)
                                            (Or (Pair False True)
                                                (Pair False False))))
                                    (Pair (Not True) (Or Int (Or True False))))
                               (Or (Pair False True) (Pair False False))))
  (test-judgment-holds (comp<: (Or (Pair False True) (Pair False False))
                               (Or (Pair True True)
                                   (Or (Pair True False)
                                       (Or (Pair False True)
                                           (Pair False False))))))
  (test-judgment-holds (comp<: (Or (Pair False True) (Pair False False))
                               (And (Or (Pair True True)
                                        (Or (Pair True False)
                                            (Or (Pair False True)
                                                (Pair False False))))
                                    (Pair (Not True) (Or Int (Or True False))))))


  ;; simple top type checks
  (redex-check-true
   "top base type"
   sst
   ι
   (judgment-holds (comp<: ι Any-Base))
   #:attempts 5)
  (redex-check-true
   "top base or"
   sst
   (ι_1 ι_2)
   (judgment-holds (comp<: (Or ι_1 ι_2) Any-Base))
   #:attempts 5)
  (redex-check-true
   "top pair type"
   sst
   (τ_1 τ_2)
   (judgment-holds (comp<: (Pair τ_1 τ_2) Any-Pair))
   #:attempts 100)
  (redex-check-true
   "top fun type"
   sst
   (τ_1 τ_2)
   (judgment-holds (comp<: (Fun τ_1 τ_2) Any-Fun))
   #:attempts 100)
  ;; do efficient and naive emptiness agree?
  (redex-check-true
   "compare emptiness implementations"
   sst
   τ
   (equal? (term (mt? τ)) (term (nmt? τ)))
   #:attempts 10000)
  ;; does efficient and naive subtyping agree?
  (redex-check-true
   "compare subtyping implementations"
   sst
   (σ τ)
   (and (equal? (term (<: σ τ))  (term (n<: σ τ)))
        (equal? (term (<: τ σ))  (term (n<: τ σ))))
   #:attempts 10000)


  
  ;; subtyping/overlap/emptiness properties
  ;; - - - - - - - - - - - - - - - - - - - - 

  ;; reflexivity
  (redex-check-true
   "reflexivity"
   sst
   τ
   (judgment-holds (≈ τ τ))
   #:attempts 1000)
  ;; pair not in
  (redex-check-true
   "pair/int no overlap"
   sst
   (τ_1 τ_2)
   (judgment-holds (not-overlap? (Pair τ_1 τ_2) Int))
   #:attempts 1000)
  ;; fun not int
  (redex-check-true
   "fun/int no overlap"
   sst
   (τ_1 τ_2)
   (judgment-holds (not-overlap? (Fun τ_1 τ_2) Int))
   #:attempts 1000)
  ;; pair not fun
  (redex-check-true
   "pair/fun no overlap"
   sst
   (τ_1 τ_2 τ_3 τ_4)
   (judgment-holds (not-overlap? (Pair τ_1 τ_2) (Fun τ_3 τ_4)))
   #:attempts 1000)
  ;; orR1
  (redex-check-true
   "orR1"
   sst
   (τ_1 τ_2)
   (judgment-holds (<: τ_1 (Or τ_1 τ_2)))
   #:attempts 1000)
  ;; orR2
  (redex-check-true
   "orR2"
   sst
   (τ_1 τ_2)
   (judgment-holds (<: τ_2 (Or τ_1 τ_2)))
   #:attempts 1000)
  ;; andL1
  (redex-check-true
   "andL1"
   sst
   (τ_1 τ_2)
   (judgment-holds (<: (And τ_1 τ_2) τ_1))
   #:attempts 1000)
  ;; andL2
  (redex-check-true
   "andL2"
   sst
   (τ_1 τ_2)
   (judgment-holds (<: (And τ_1 τ_2) τ_2))
   #:attempts 1000)
  ;; notOrL1
  (redex-check-true
   "not OrL1"
   sst
   (τ_1 τ_2)
   (judgment-holds (<: (Not (Or τ_1 τ_2)) (Not τ_1)))
   #:attempts 1000)
  ;; notOrL2
  (redex-check-true
   "not OrL2"
   sst
   (τ_1 τ_2)
   (judgment-holds (<: (Not (Or τ_1 τ_2)) (Not τ_2)))
   #:attempts 1000)
  ;; pairLHS
  (redex-check-true
   "pair LHS"
   sst
   (τ_1 τ_2)
   (if (judgment-holds (<: τ_1 τ_2))
       (judgment-holds (<: (Pair τ_1 Any) (Pair τ_2 Any)))
       (judgment-holds (</: (Pair τ_1 Any) (Pair τ_2 Any))))
   #:attempts 1000)
  ;; pairRHS
  (redex-check-true
   "pair RHS"
   sst
   (τ_1 τ_2)
   (if (judgment-holds (<: τ_1 τ_2))
       (judgment-holds (<:  (Pair Any τ_1) (Pair Any τ_2)))
       (judgment-holds (</: (Pair Any τ_1) (Pair Any τ_2))))
   #:attempts 1000)
  ;; And Pair w/ Or/Not
  (redex-check-true
   "And Pair w/ Or/Not"
   sst
   (τ_1 τ_2 τ_3)
   (judgment-holds (<: (And (Pair (Or τ_1 τ_2) τ_3)
                            (Pair (Not τ_1) τ_3))
                       (Pair τ_2 τ_3)))
  ;; And Pair w/ Or/Not
   #:attempts 1000)
  (redex-check-true
   "And Pair w/ Or/Not"
   sst
   (τ_1 τ_2 τ_3)
   (judgment-holds (<: (And (Pair τ_1 (Or τ_2 τ_3))
                            (Pair τ_1 (Not τ_2)))
                       (Pair τ_1 τ_3)))
   #:attempts 1000)
  ;; or pairs
  (redex-check-true
   "or pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6)
   (judgment-holds (<: (Or (Pair τ_1 τ_2)
                           (Or (Pair τ_3 τ_4)
                               (Pair τ_5 τ_6)))
                       (Pair (Or τ_1 (Or τ_3 τ_5))
                             (Or τ_2 (Or τ_4 τ_6)))))
   #:attempts 1000)
  ;; And Pairs
  (redex-check-true
   "And Pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6)
   (judgment-holds (≈ (And (Pair τ_1 τ_2)
                           (And (Pair τ_3 τ_4)
                                (Pair τ_5 τ_6)))
                      (Pair (And τ_1 (And τ_3 τ_5))
                            (And τ_2 (And τ_4 τ_6)))))
   #:attempts 1000)
  ;; Fun Contra/Co
  (redex-check-true
   "Fun Contra/Co"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6)
   (judgment-holds (<: (Fun (Or τ_1 (Or τ_3 τ_5)) τ_2)
                       (Fun τ_1 (Or τ_2 (Or τ_4 τ_6)))))
   #:attempts 1000)
  ;; Fun And
  (redex-check-true
   "Fun And"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6)
   (judgment-holds (<: (And (Fun τ_1 τ_2)
                            (And (Fun τ_3 τ_4)
                                 (Fun τ_5 τ_6)))
                       (Fun τ_1 τ_2)))
   #:attempts 1000)
  ;; Fun And Or
  (redex-check-true
   "Fun And Or"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6)
   (judgment-holds (<: (And (Fun τ_1 τ_2)
                            (And (Fun τ_3 τ_4)
                                 (Fun τ_5 τ_6)))
                       (Fun (Or τ_1 (Or τ_3 τ_5))
                            (Or τ_2 (Or τ_4 τ_6)))))
   #:attempts 1000)
  ;; Fun And And
  (redex-check-true
   "Fun And And"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6)
   (judgment-holds (<: (And (Fun τ_1 τ_2)
                            (And (Fun (Or τ_1 τ_3) τ_4)
                                 (Fun τ_5 τ_6)))
                       (Fun τ_1 (Or τ_2 τ_4))))
   #:attempts 1000)

  
  ;; logical equivalences
  ;; - - - - - - - - - - - -
  ;; and comm
  (redex-check-true
   "and comm"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (And τ_1 τ_2) (And τ_2 τ_1)))
   #:attempts 1000)
  ;; and assoc
  (redex-check-true
   "and assoc"
   sst
   (τ_1 τ_2 τ_3)
   (judgment-holds (≈ (And τ_1 (And τ_2 τ_3)) (And (And τ_1 τ_2) τ_3)))
   #:attempts 1000)
  ;; or comm
  (redex-check-true
   "or comm"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (Or τ_1 τ_2) (Or τ_2 τ_1)))
   #:attempts 1000)
  ;; or assoc
  (redex-check-true
   "or assoc"
   sst
   (τ_1 τ_2 τ_3)
   (judgment-holds (≈ (Or τ_1 (Or τ_2 τ_3)) (Or (Or τ_1 τ_2) τ_3)))
   #:attempts 1000)
  ;; and distr
  (redex-check-true
   "and distr"
   sst
   (τ_1 τ_2 τ_3)
   (judgment-holds (≈ (And τ_1 (Or τ_2 τ_3)) (Or (And τ_1 τ_2) (And τ_1 τ_3))))
   #:attempts 1000)
  ;; or idempotency
  (redex-check-true
   "or idempotency"
   sst
   (τ)
   (judgment-holds (≈ (Or τ τ) τ))
   #:attempts 1000)
  ;; and idempotency
  (redex-check-true
   "and idempotency"
   sst
   (τ)
   (judgment-holds (≈ (And τ τ) τ))
   #:attempts 1000)
  ;; tautology
  (redex-check-true
   "tautology"
   sst
   (τ)
   (judgment-holds (≈ (Or τ (Not τ)) Any))
   #:attempts 1000)
  ;; contradiction
  (redex-check-true
   "contradiction"
   sst
   (τ)
   (judgment-holds (≈ (And τ (Not τ)) Empty))
   #:attempts 1000)
  ;; double negation
  (redex-check-true
   "double negation"
   sst
   (τ)
   (judgment-holds (≈ (Not (Not τ)) τ))
   #:attempts 1000)
  ;; DeMorgan's Law
  (redex-check-true
   "DeMorgan's Law 1"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (Not (Or τ_1 τ_2)) (And (Not τ_1) (Not τ_2))))
   #:attempts 1000)
  (redex-check-true
   "DeMorgan's Law 2"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (Not (And τ_1 τ_2)) (Or (Not τ_1) (Not τ_2))))
   #:attempts 1000)
  ;; disj syllogism
  (redex-check-true
   "disj syllogism 1"
   sst
   (τ_1 τ_2)
   (judgment-holds (<: (And (Or τ_1 τ_2) (Not τ_1)) τ_2))
   #:attempts 1000)
  (redex-check-true
   "disj syllogism 2"
   sst
   (τ_1 τ_2)
   (judgment-holds (<: (And (Or τ_1 τ_2) (Not τ_2)) τ_1))
   #:attempts 1000)


  ;; tests build on NAND equivs
  ;; --------------------------
  ;; not w/ NAND
  (redex-check-true
   "not w/ NAND"
   sst
   (τ)
   (judgment-holds (≈ (Not τ) (NAND τ τ)))
   #:attempts 1000)
  ;; and w/ NAND
  (redex-check-true
   "and w/ NAND"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (And τ_1 τ_2) (NAND (NAND τ_1 τ_2) (NAND τ_1 τ_2))))
   #:attempts 1000)
  ;; OR w/ NAND
  (redex-check-true
   "OR w/ NAND"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (Or τ_1 τ_2) (NAND (NAND τ_1 τ_1) (NAND τ_2 τ_2))))
   #:attempts 1000)
  ;; NOR w/ NAND
  (redex-check-true
   "NOR w/ NAND"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (Not (Or τ_1 τ_2)) (NAND (NAND (NAND τ_1 τ_1) (NAND τ_2 τ_2))
                                               (NAND (NAND τ_1 τ_1) (NAND τ_2 τ_2)))))
   #:attempts 1000)
  ;; XOR w/ NAND
  (redex-check-true
   "XOR w/ NAND"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (Or (And τ_1 (Not τ_2)) (And (Not τ_1) τ_2))
                      (NAND (NAND τ_1 (NAND τ_1 τ_2))
                            (NAND τ_2 (NAND τ_1 τ_2)))))
   #:attempts 1000)
  ;; XNOR w/ NAND
  (redex-check-true
   "XNOR w/ NAND"
   sst
   (τ_1 τ_2)
   (judgment-holds (≈ (Or (And τ_1 τ_2) (And (Not τ_1) (Not τ_2)))
                      (NAND (NAND (NAND τ_1 τ_1) (NAND τ_2 τ_2))
                            (NAND τ_1 τ_2))))
   #:attempts 1000)

) ;; end of subtyping tests




  ;                                                                        
  ;                           ;                                            
  ;                                               ;       ;                
  ;                                               ;                        
  ;   ;;;;    ;;;;   ;;;    ;;;    ;;;    ;;;   ;;;;;   ;;;    ;;;   ; ;;  
  ;   ;; ;;   ;;  ; ;; ;;     ;   ;;  ;  ;;  ;    ;       ;   ;; ;;  ;;  ; 
  ;   ;   ;   ;     ;   ;     ;   ;   ;; ;        ;       ;   ;   ;  ;   ; 
  ;   ;   ;   ;     ;   ;     ;   ;;;;;; ;        ;       ;   ;   ;  ;   ; 
  ;   ;   ;   ;     ;   ;     ;   ;      ;        ;       ;   ;   ;  ;   ; 
  ;   ;; ;;   ;     ;; ;;     ;   ;      ;;       ;       ;   ;; ;;  ;   ; 
  ;   ;;;;    ;      ;;;      ;    ;;;;   ;;;;    ;;;   ;;;;;  ;;;   ;   ; 
  ;   ;                       ;                                            
  ;   ;                       ;                                            
  ;   ;                     ;;                                             
                                    
  ;     ;                    ;          
  ;     ;                    ;          
  ;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
  ;     ;    ;;  ;  ;   ;    ;    ;   ; 
  ;     ;    ;   ;; ;        ;    ;     
  ;     ;    ;;;;;;  ;;;     ;     ;;;  
  ;     ;    ;          ;    ;        ; 
  ;     ;    ;      ;   ;    ;    ;   ; 
  ;     ;;;   ;;;;   ;;;     ;;;   ;;;  
  ;                                     
  ;                                     
  ;

(module+ test
    
  (define-judgment-form sst
    #:mode (proj-test I I)
    #:contract (proj-test (τ idx) any)
    [(where t_1 (parse τ))
     (where t_2 (maybe-project idx t_1))
     (where t_3 (nmaybe-project idx t_1))
     (where s (parse σ))
     (side-condition (equiv t_2 s))
     (side-condition (equiv t_3 s))
     --------------------
     (proj-test (τ idx) σ)]
    [(where #false (maybe-project idx (parse τ)))
     --------------------
     (proj-test (τ idx) #false)])
  
  (test-judgment-holds
   (proj-test [(Pair Int Str) 1]
              Int))
  (test-judgment-holds
   (proj-test [Any 1]
              #false))
  (test-judgment-holds
   (proj-test [(Or (Pair Int Int) (Fun Int Int)) 1]
              #false))
  (test-judgment-holds
   (proj-test [(Pair Int Str) 2]
              Str))
  (test-judgment-holds
   (proj-test [(Or (Pair Int Str) (Pair True False)) 1]
              (Or True Int)))
  (test-judgment-holds
   (proj-test [(Or (Pair Int Str) (Pair True False)) 2]
              (Or False Str)))
  (test-judgment-holds
   (proj-test [(And (Pair (Or Int  Str) (Or True False))
                    (Pair (Or True Str) (Or True Int)))
               1]
              Str))
  (test-judgment-holds
   (proj-test [(And (Pair (Or Int  Str) (Or True False))
                    (Pair (Or True Str) (Or True Int)))
               2]
              True))
  (test-judgment-holds
   (proj-test [(Or (And (Pair (Or Int  Str) (Or True False))
                        (Pair (Or True Str) (Or True Int)))
                   (Pair True False))
               1]
              (Or Str True)))
  (test-judgment-holds
   (proj-test [(Or (And (Pair (Or Int  Str) (Or True False))
                        (Pair (Or True Str) (Or True Int)))
                   (Pair True False))
               2]
              (Or True False)))
  (test-judgment-holds
   (proj-test [(And (Pair (Or True False) (Or True False))
                    (Not (Pair True False)))
               1]
              (Or True False)))
  (test-judgment-holds
   (proj-test [(And (Pair (Or True False) (Or True False))
                    (Not (Pair True (Or True False))))
               1]
              False))
  (test-judgment-holds
   (proj-test [(And (Pair (Or True False) (Or True False))
                    (Not (Pair True False)))
               2]
              (Or True False)))
  (test-judgment-holds
   (proj-test [(And (Pair (Or True False) (Or True False))
                    (Not (Pair (Or True False) True)))
               2]
              False))

  (define-judgment-form sst
    #:mode (proj-comp I I)
    #:contract (proj-comp τ idx)
    [(where t_1 (parse τ))
     (where t_2a (maybe-project idx t_1))
     (where t_2b (nmaybe-project idx t_1))
     (side-condition (equiv t_2a t_2b))
     --------------------
     (proj-comp τ idx)]
    [(where t_1 (parse τ))
     (where #false (maybe-project idx t_1))
     (where #false (nmaybe-project idx t_1))
     --------------------
     (proj-comp τ idx)])
  
  (redex-check-true
   "proj union comp"
   sst
   (τ idx)
   (judgment-holds (proj-comp τ idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp Pair"
   sst
   (τ_1 τ_2 idx)
   (judgment-holds (proj-comp (Pair τ_1 τ_2) idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp Or"
   sst
   (τ_1 τ_2 idx)
   (judgment-holds (proj-comp (Or τ_1 τ_2) idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp Or of Pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 idx)
   (judgment-holds (proj-comp (Or (Pair τ_1 τ_2)
                                  (Pair τ_3 τ_4))
                              idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp And of Pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 idx)
   (judgment-holds (proj-comp (And (Pair τ_1 τ_2)
                                   (Pair τ_3 τ_4))
                              idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp And of Pairs w/ Neg"
   sst
   (τ_1 τ_2 τ_3 τ_4 idx)
   (judgment-holds (proj-comp (And (Pair τ_1 τ_2)
                                   (Not (Pair τ_3 τ_4)))
                              idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp Or/And of Pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 idx)
   (judgment-holds (proj-comp (Or (And (Pair τ_1 τ_2)
                                       (Pair τ_3 τ_4))
                                  (And (Pair τ_5 τ_6)
                                       (Pair τ_7 τ_8)))
                              idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp And/Or of Pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 idx)
   (judgment-holds (proj-comp (And (Or (Pair τ_1 τ_2)
                                       (Pair τ_3 τ_4))
                                   (Or (Pair τ_5 τ_6)
                                       (Pair τ_7 τ_8)))
                              idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp And/Or+Neg of Pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9 τ_10 idx)
   (judgment-holds (proj-comp (And (Or (Pair τ_1 τ_2)
                                       (Pair τ_3 τ_4))
                                   (And (Or (Pair τ_5 τ_6)
                                            (Pair τ_7 τ_8))
                                        (Or (Pair τ_1 τ_2)
                                            (Not (Pair τ_9 τ_10)))))
                              idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp Or/And+Neg of Pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9 τ_10 idx)
   (judgment-holds (proj-comp (And (Or (And (Pair τ_1 τ_2)
                                            (Pair τ_3 τ_4))
                                       (And (Pair τ_5 τ_6)
                                            (Pair τ_7 τ_8)))
                                   (Not (Pair τ_9 τ_10)))
                              idx))
   #:attempts 1000)
  (redex-check-true
   "proj union comp And/Or+Neg of Pairs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9 τ_10 idx)
   (judgment-holds (proj-comp (And (And (Or (Pair τ_1 τ_2)
                                            (Pair τ_3 τ_4))
                                        (Or (Pair τ_5 τ_6)
                                            (Pair τ_7 τ_8)))
                                   (Not (Pair τ_9 τ_10)))
                              idx))
   #:attempts 1000)

  )



;                                            
;       ;                                    
;       ;                          ;         
;       ;                                    
;    ;;;;   ;;;  ;;;;;;  ;;;;    ;;;   ; ;;  
;   ;; ;;  ;; ;; ;  ;  ;     ;     ;   ;;  ; 
;   ;   ;  ;   ; ;  ;  ;     ;     ;   ;   ; 
;   ;   ;  ;   ; ;  ;  ;  ;;;;     ;   ;   ; 
;   ;   ;  ;   ; ;  ;  ; ;   ;     ;   ;   ; 
;   ;; ;;  ;; ;; ;  ;  ; ;   ;     ;   ;   ; 
;    ;;;;   ;;;  ;  ;  ;  ;;;;   ;;;;; ;   ; 
;                                            
;                                            
;                                            
;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test

  (define-judgment-form sst
    #:mode (domain-test I I)
    #:contract (domain-test τ any)
    [(where t_1 (parse τ))
     (where t_2 (maybe-domain t_1))
     (where t_3 (nmaybe-domain t_1))
     (where s (parse σ))
     (side-condition (equiv t_2 s))
     (side-condition (equiv t_3 s))
     --------------------
     (domain-test τ σ)]
    [(where t_1 (parse τ))
     (where #false (maybe-domain t_1))
     (where #false (nmaybe-domain t_1))
     --------------------
     (domain-test τ #false)])

  (test-judgment-holds
   (domain-test (Fun Int Str) Int))
  (test-judgment-holds
   (domain-test (And (Fun Int Str)
                     (Fun True False))
                (Or Int True)))
  (test-judgment-holds
   (domain-test (Or (And (Fun Int Str)
                         (Fun True False))
                    (And (Fun Int Str)
                         (Fun False True)))
                Int))
  
  (define-judgment-form sst
    #:mode (domain-comp I)
    #:contract (domain-comp τ)
    [(where t_1 (parse τ))
     (where t_2a (maybe-domain t_1))
     (where t_2b (nmaybe-domain t_1))
     (side-condition (equiv t_2a t_2b))
     --------------------
     (domain-comp τ)]
    [(where t_1 (parse τ))
     (where #false (maybe-domain t_1))
     (where #false (nmaybe-domain t_1))
     --------------------
     (domain-comp τ)])

  (redex-check-true
   "maybe-domain comp"
   sst
   (τ)
   (judgment-holds (domain-comp τ))
   #:attempts 10000)
  (redex-check-true
   "maybe-domain comp Or"
   sst
   (τ_1 τ_2)
   (judgment-holds (domain-comp (Or τ_1 τ_2)))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp And"
   sst
   (τ_1 τ_2)
   (judgment-holds (domain-comp (Or τ_1 τ_2)))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp Or Funs"
   sst
   (τ_1 τ_2 τ_3 τ_4)
   (judgment-holds (domain-comp (Or (Fun τ_1 τ_2) (Fun τ_3 τ_4))))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp Or Funs 2"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6)
   (judgment-holds (domain-comp (Or (Fun τ_1 τ_2)
                                    (Or (Fun τ_3 τ_4)
                                        (Fun τ_5 τ_6)))))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp And Funs"
   sst
   (τ_1 τ_2 τ_3 τ_4)
   (judgment-holds (domain-comp (And (Fun τ_1 τ_2) (Fun τ_3 τ_4))))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp And Funs 2"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6)
   (judgment-holds (domain-comp (And (Fun τ_1 τ_2)
                                     (And (Fun τ_3 τ_4)
                                          (Fun τ_5 τ_6)))))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp Or/And Funs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8)
   (judgment-holds (domain-comp (Or (And (Fun τ_1 τ_2) (Fun τ_3 τ_4))
                                    (And (Fun τ_5 τ_6) (Fun τ_7 τ_8)))))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp And/Or Funs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8)
   (judgment-holds (domain-comp (And (Or (Fun τ_1 τ_2) (Fun τ_3 τ_4))
                                     (Or (Fun τ_5 τ_6) (Fun τ_7 τ_8)))))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp Or/And+Neg Funs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9 τ_10)
   (judgment-holds (domain-comp (And (Or (And (Fun τ_1 τ_2) (Fun τ_3 τ_4))
                                         (And (Fun τ_5 τ_6) (Fun τ_7 τ_8)))
                                     (Not (Fun τ_9 τ_10)))))
   #:attempts 1000)
  (redex-check-true
   "maybe-domain comp And/Or+Neg Funs"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9 τ_10)
   (judgment-holds (domain-comp (And (And (Or (Fun τ_1 τ_2) (Fun τ_3 τ_4))
                                          (Or (Fun τ_5 τ_6) (Fun τ_7 τ_8)))
                                     (Not (Fun τ_9 τ_10)))))
   #:attempts 1000)

  )



;                                            
;      ;;                                    
;     ;                                      
;     ;                                      
;   ;;;;;  ;   ;  ; ;;   ;;;;   ;;;;   ;;;;  
;     ;    ;   ;  ;;  ;      ;  ;; ;;  ;; ;; 
;     ;    ;   ;  ;   ;      ;  ;   ;  ;   ; 
;     ;    ;   ;  ;   ;   ;;;;  ;   ;  ;   ; 
;     ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
;     ;    ;   ;  ;   ;  ;   ;  ;; ;;  ;; ;; 
;     ;     ;;;;  ;   ;   ;;;;  ;;;;   ;;;;  
;                               ;      ;     
;                               ;      ;     
;                               ;      ;     

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (define-judgment-form sst
    #:mode (funapp-test I I)
    #:contract (funapp-test (τ τ) any)
    [(where t_f (parse τ_f))
     (where t_a (parse τ_a))
     (where t_1 (maybe-funapp t_f t_a))
     (where t_2 (nmaybe-funapp t_f t_a))
     (where s (parse σ))
     (side-condition (equiv t_1 s))
     (side-condition (equiv t_2 s))
     --------------------
     (funapp-test (τ_f τ_a) σ)]
    [(where t_f (parse τ_f))
     (where t_a (parse τ_a))
     (where #false (maybe-funapp t_f t_a))
     (where #false (nmaybe-funapp t_f t_a))
     --------------------
     (funapp-test (τ_f τ_a) #false)])

  (test-judgment-holds
   (funapp-test ((Fun Int Str) Int) Str))
  (test-judgment-holds
   (funapp-test ((Fun Int Str) Str) #false))
  (test-judgment-holds
   (funapp-test ((And (Fun Int Str)
                           (Fun True False))
                      Int)
                     Str))
  (test-judgment-holds
   (funapp-test ((And (Fun Int Str)
                           (Fun True False))
                      True)
                     False))
  (test-judgment-holds
   (funapp-test ((And (Fun Int Str)
                           (Fun True False))
                      Str)
                     #false))
  (test-judgment-holds
   (funapp-test ((Or (And (Fun Int Str)
                               (Fun True False))
                          (And (Fun Int True)
                               (Fun False True)))
                      Int)
                     (Or Str True)))
  (test-judgment-holds
   (funapp-test ((Or (And (Fun Int Str)
                               (Fun True False))
                          (And (Fun Int True)
                               (Fun False True)))
                      False)
                     #false))
  (test-judgment-holds
   (funapp-test ((And (Or (Fun Int Str)
                               (Fun Int False))
                           (Or (Fun Str True)
                               (Fun Str False)))
                      Int)
                     (Or Str False)))
  (test-judgment-holds
   (funapp-test ((And (Or (Fun Int Str)
                               (Fun Int False))
                           (Or (Fun Str True)
                               (Fun Str False)))
                      Str)
                     (Or True False)))
  (test-judgment-holds
   (funapp-test ((And (Or (Fun Int Str)
                               (Fun Int False))
                           (Or (Fun Str True)
                               (Fun Str False)))
                      True)
                     #false))

  (define-judgment-form sst
    #:mode (funapp-comp I I)
    #:contract (funapp-comp τ τ)
    [(where t_f (parse τ_f))
     (where t_a (parse τ_a))
     (where t_1 (maybe-funapp t_f t_a))
     (where t_2 (nmaybe-funapp t_f t_a))
     (side-condition (equiv t_1 t_2))
     --------------------
     (funapp-comp τ_f τ_a)]
    [(where t_f (parse τ_f))
     (where t_a (parse τ_a))
     (where #false (maybe-funapp t_f t_a))
     (where #false (nmaybe-funapp t_f t_a))
     --------------------
     (funapp-comp τ_f τ_a)])

  (redex-check-true
   "maybe-result-type comp"
   sst
   (τ_1 τ_2)
   (judgment-holds (funapp-comp τ_1 τ_1))
   #:attempts 1000)
  (redex-check-true
   "maybe-result-type comp Or"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5)
   (judgment-holds (funapp-comp (Or (Fun τ_1 τ_2) (Fun τ_3 τ_4)) τ_5))
   #:attempts 1000)
  (redex-check-true
   "maybe-result-type comp Or 2"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7)
   (judgment-holds (funapp-comp (Or (Fun τ_1 τ_2) (Or (Fun τ_3 τ_4) (Fun τ_5 τ_6))) τ_7))
   #:attempts 1000)
  (redex-check-true
   "maybe-result-type comp And"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5)
   (judgment-holds (funapp-comp (And (Fun τ_1 τ_2) (Fun τ_3 τ_4)) τ_5))
   #:attempts 1000)
  (redex-check-true
   "maybe-result-type comp And 2"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7)
   (judgment-holds (funapp-comp (And (Fun τ_1 τ_2) (And (Fun τ_3 τ_4) (Fun τ_5 τ_6))) τ_7))
   #:attempts 1000)
  (redex-check-true
   "maybe-result-type comp And/Or"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9)
   (judgment-holds (funapp-comp (And (Or (Fun τ_1 τ_2) (Fun τ_3 τ_4))
                                          (Or (Fun τ_5 τ_6) (Fun τ_7 τ_8)))
                                     τ_9))
   #:attempts 1000)
  (redex-check-true
   "maybe-result-type comp Or/And"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9)
   (judgment-holds (funapp-comp (Or (And (Fun τ_1 τ_2) (Fun τ_3 τ_4))
                                         (And (Fun τ_5 τ_6) (Fun τ_7 τ_8)))
                                     τ_9))
   #:attempts 1000)
  (redex-check-true
   "maybe-result-type comp And/Or+Neg"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9 τ_10 τ_11)
   (judgment-holds (funapp-comp (And (And (Or (Fun τ_1 τ_2) (Fun τ_3 τ_4))
                                               (Or (Fun τ_5 τ_6) (Fun τ_7 τ_8)))
                                          (Not (Fun τ_9 τ_10)))
                                     τ_11))
   #:attempts 1000)
  (redex-check-true
   "maybe-result-type comp Or/And+Neg"
   sst
   (τ_1 τ_2 τ_3 τ_4 τ_5 τ_6 τ_7 τ_8 τ_9 τ_10 τ_11)
   (judgment-holds (funapp-comp (And (Or (And (Fun τ_1 τ_2) (Fun τ_3 τ_4))
                                              (And (Fun τ_5 τ_6) (Fun τ_7 τ_8)))
                                          (Not (Fun τ_9 τ_10)))
                                     τ_11))
   #:attempts 1000)

  )


(module+ test
  (test-results))



#lang racket/base

(require redex/reduction-semantics
         racket/match
         (only-in racket/list in-combinations)
         (only-in racket/set set-subtract)
         "lang.rkt"
         "type-rep.rkt"
         "inhabitation.rkt")

(provide (all-defined-out))

(define-metafunction sst
  select : idx t t -> t
  [(select 1 t_1 t_2) t_1]
  [(select 2 t_1 t_2) t_2])

;; Given a type, calulcate its first
;; projection (or return false if
;; it is not a pair)
(define-metafunction sst
  maybe-project : idx t -> t or #false
  [(maybe-project idx t)
   (project idx Prodb Any-t Any-t ∅)
   (side-condition (term (subtype t Any-Prod-t)))
   (where (Type _ Prodb _) t)]
  [(maybe-project idx t) #false])

(define-metafunction sst
  project : idx Prodb s s N -> t
  [(project idx Bot s_1 s_2 N) Empty-t]
  [(project idx Prodb s_1 s_2 N)
   Empty-t
   (side-condition (OR (term (empty s_1)) or (term (empty s_2))))]
  [(project idx Top s_1 s_2 N)
   (project-aux idx s_1 s_2 N)]
  [(project idx (Node (× t_1 t_2) Prodb_l Prodb_m Prodb_r) s_1 s_2 N)
   (t-or t_l (t-or t_m t_r))
   (where t_l (project idx Prodb_l (t-and s_1 t_1) (t-and s_2 t_2) N))
   (where t_m (project idx Prodb_m s_1 s_2 N))
   (where t_r (project idx Prodb_r s_1 s_2 (set-cons (× t_1 t_2) N)))])

(define-metafunction sst
  project-aux : idx s s N -> t
  [(project-aux idx s_1 s_2 N)
   Empty-t
   (side-condition (OR (term (empty s_1)) or (term (empty s_2))))]
  [(project-aux idx s_1 s_2 ∅) (select idx s_1 s_2)]
  [(project-aux idx s_1 s_2 (set-cons (× t_1 t_2) N))
   (t-or (project-aux idx (t-diff s_1 t_1) s_2 N) (project-aux idx s_1 (t-diff s_2 t_2) N))])


;; Given a type, calculate its
;; domain (or return #false if it
;; is not a function).
(define-metafunction sst
  maybe-domain : t -> t or #false
  [(maybe-domain t)
   (domain Empty-t Arrowb)
   (side-condition (term (subtype t Any-Fun-t)))
   (where (Type _ _ Arrowb) t)]
  [(maybe-domain t) #false])

;; NOTE slightly diff from haskell
;; version, double check w/ testing of course
(define-metafunction sst
  domain : t Arrowb -> t
  [(domain t Top) t]
  [(domain t Bot) Any-t]
  [(domain t (Node (→ s_1 s_2) Arrowb_l Arrowb_m Arrowb_r))
   (t-and t_l (t-and t_m t_r))
   (where t_l (domain (t-or t s_1) Arrowb_l))
   (where t_m (domain t Arrowb_m))
   (where t_r (domain t Arrowb_r))])

;; Given a (function) type and
;; a (argument) type, return the
;; type of the result of applying
;; the first to the second, or #false
;; if the first is not a function or
;; if the second is not in the domain
;; of the first.
(define-metafunction sst
  maybe-funapp : t t -> t or #false
  [(maybe-funapp t_f t_a)
   (funapp t_a Any-t Arrowb)
   (where t_d (maybe-domain t_f))
   (side-condition (term (subtype t_a t_d)))
   (where (Type _ _ Arrowb) t_f)]
  [(maybe-funapp t_f t_a) #false])

(define-metafunction sst
  funapp : t t Arrowb -> t
  [(funapp t_a t Bot) Empty-t]
  [(funapp t_a t Arrowb)
   Empty-t
   (side-condition (OR (term (empty t_a)) or (term (empty t))))]
  [(funapp t_a t Top) t]
  [(funapp t_a t (Node (→ s_1 s_2) Arrowb_l Arrowb_m Arrowb_r))
   (t-or t_l1 (t-or t_l2 (t-or t_m t_r)))
   (where t_l1 (funapp t_a (t-and t s_2) Arrowb_l))
   (where t_l2 (funapp (t-diff t_a s_1) t Arrowb_l))
   (where t_m (funapp t_a t Arrowb_m))
   (where t_r (funapp t_a t Arrowb_r))])



;                                     
;                                     
;                    ;                
;                                     
;   ; ;;   ;;;;    ;;;   ;   ;   ;;;  
;   ;;  ;      ;     ;   ;   ;  ;;  ; 
;   ;   ;      ;     ;    ; ;   ;   ;;
;   ;   ;   ;;;;     ;    ; ;   ;;;;;;
;   ;   ;  ;   ;     ;    ; ;   ;     
;   ;   ;  ;   ;     ;     ;    ;     
;   ;   ;   ;;;;   ;;;;;   ;     ;;;; 
;                                     
;                                     
;                                     
;                                                          
;                                  ;;                      
;                   ;             ;                        
;                   ;             ;                        
;  ;;;;;;   ;;;   ;;;;;  ;;;;   ;;;;;  ;   ;  ; ;;    ;;;  
;  ;  ;  ; ;;  ;    ;        ;    ;    ;   ;  ;;  ;  ;   ; 
;  ;  ;  ; ;   ;;   ;        ;    ;    ;   ;  ;   ;  ;     
;  ;  ;  ; ;;;;;;   ;     ;;;;    ;    ;   ;  ;   ;   ;;;  
;  ;  ;  ; ;        ;    ;   ;    ;    ;   ;  ;   ;      ; 
;  ;  ;  ; ;        ;    ;   ;    ;    ;   ;  ;   ;  ;   ; 
;  ;  ;  ;  ;;;;    ;;;   ;;;;    ;     ;;;;  ;   ;   ;;;  
;                                                          
;                                                          
;                                                          


(define (union-domains arrows)
  (for/fold ([dom (term Empty-t)])
            ([a (in-list arrows)])
    (match a
      [`(→ ,t1 ,_) (term (t-or ,dom ,t1))])))

(define (intersect-codomains arrows)
  (for/fold ([cdom (term Any-t)])
            ([a (in-list arrows)])
    (match a
      [`(→ ,_ ,t2) (term (t-and ,cdom ,t2))])))


(define-metafunction sst
  nmaybe-project : idx t -> t or #false
  [(nmaybe-project idx t)
   (nproject idx Prodb)
   (side-condition (term (subtype t Any-Prod-t)))
   (where (Type _ Prodb _) t)]
  [(nmaybe-project idx t) #false])

(define-metafunction sst
  nproject : idx Prodb -> t
  [(nproject idx Prodb) ,(naive-project (term idx) (term Prodb))])

(define (naive-project idx bdd)
  (let loop ([bdd bdd]
             [s1 (term Any-t)]
             [s2 (term Any-t)])
    (match bdd
      ['Bot (term Empty-t)]
      ['Top (match* ((term (empty ,s1)) (term (empty ,s2)))
              [(#true _) (term Empty-t)]
              [(_ #true) (term Empty-t)]
              [(#false #false)
               (match idx [1 s1] [2 s2])])]
      [`(Node (× ,t1 ,t2) ,l ,m ,r)
       (define res1 (loop l
                          (term (t-and ,s1 ,t1))
                          (term (t-and ,s2 ,t2))))
       (define res2 (loop m s1 s2))
       (define res3 (loop r (term (t-diff ,s1 ,t1)) s2))
       (define res4 (loop r s1 (term (t-diff ,s2 ,t2))))
       (term (t-or ,res1 (t-or ,res2 (t-or ,res3 ,res4))))])))


(define-metafunction sst
  nmaybe-domain : t -> t or #false
  [(nmaybe-domain t)
   (ndomain Arrowb)
   (side-condition (term (subtype t Any-Fun-t)))
   (where (Type _ _ Arrowb) t)]
  [(nmaybe-domain t) #false])


(define-metafunction sst
  ndomain : Arrowb -> t
  [(ndomain Arrowb) ,(naive-domain (term Arrowb))])

(define (naive-domain bdd)
  (let loop ([bdd bdd]
             [P '()])
    (match bdd
      ['Bot (term Any-t)]
      ['Top (union-domains P)]
      [`(Node ,a ,l ,m ,r)
       (define res1 (loop l (cons a P)))
       (define res2 (loop m P))
       (define res3 (loop r P))
       (term (t-and ,res1 (t-and ,res2 ,res3)))])))


(define-metafunction sst
  nmaybe-funapp : t t -> t or #false
  [(nmaybe-funapp t_f t_a)
   (nfunapp t_a Arrowb)
   (where t_d (nmaybe-domain t_f))
   (side-condition (term (subtype t_a t_d)))
   (where (Type _ _ Arrowb) t_f)]
  [(nmaybe-funapp t_f t_a) #false])

(define-metafunction sst
  nfunapp : t Arrowb -> t
  [(nfunapp t_a Arrowb) ,(naive-funapp (term t_a) (term Arrowb))])


(define (naive-funapp arg bdd)
  (let loop ([bdd bdd]
             [P '()])
    (match bdd
      ['Bot (term Empty-t)]
      ['Top (for/fold ([res (term Empty-t)])
                      ([P* (in-combinations P)]
                       #:when (not (term (subtype ,arg ,(union-domains
                                                         (set-subtract P P*))))))
              (term (t-or ,res ,(intersect-codomains P*))))]
      [`(Node ,a ,l ,m ,r)
       (define res1 (loop l  (cons a P)))
       (define res2 (loop m P))
       (define res3 (loop r P))
       (term (t-or ,res1 (t-or ,res2 ,res3)))])))


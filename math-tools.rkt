#lang racket/base

(require racket/contract
         racket/match
         pict
         racket/class
         pict/tree-layout
         racket/draw)

(provide
 (contract-out
  [$ (->* () (#:size exact-nonnegative-integer?)
          #:rest (listof (or/c pict? string?))
          pict?)]
  [msub (->* (string?) (exact-nonnegative-integer? real?) pict?)]
  [msup (->* (string?) (exact-nonnegative-integer? real?) pict?)]
  [roman (->* (string?) (exact-nonnegative-integer? real?) pict?)]
  [sans (->* (string?) (exact-nonnegative-integer? real?) pict?)]
  [mono (->* (string?) (exact-nonnegative-integer? real?) pict?)]
  [mcaps (->* (string?) (exact-nonnegative-integer? real?) pict?)]
  [rcaps (->* (string?) (exact-nonnegative-integer? real?) pict?)]
  [big-wedge (->* () (#:size exact-nonnegative-integer? #:below pict?)
               #:rest (listof pict?)
               pict?)]
  [big-vee (->* () (#:size exact-nonnegative-integer? #:below pict?)
             #:rest (listof pict?)
             pict?)]
  [big-cap (->* () (#:size exact-nonnegative-integer? #:below pict?)
             #:rest (listof pict?)
             pict?)]
  [big-cup (->* () (#:size exact-nonnegative-integer? #:below pict?)
             #:rest (listof pict?)
             pict?)]
  [parens (->* () (#:offset exact-integer?
                   #:style (or/c 'round 'square 'curly 'angle))
               #:rest (listof pict?)
               pict?)]
  [inference-rule (->* (pict? pict?)
                       (#:thickness exact-nonnegative-integer?
                        #:scale (and/c real? positive?))
                       pict?)]
  [draw-bdd (-> bdd/c pict?)]
  [draw-lbdd (-> lbdd/c pict?)]))

(define ($ #:size [size 20]
           . bodies)
  (define (->pict x)
    (match x
      [(? string?) (math x)]
      [(? pict?) x]))
  (apply hbl-append (map ->pict bodies)))

(define (msub str [size 20] [angle 0])
  (text str (cons 'subscript "Latin Modern Math") size angle))

(define (msup str [size 20] [angle 0])
  (text str (cons 'superscript "Latin Modern Math") size angle))

(define (math str [size 20] [angle 0])
  (text str "Latin Modern Math" size angle))

(define (roman str [size 20] [angle 0])
  (text str "Latin Modern Roman" size angle))

(define (sans str [size 20] [angle 0])
  (text str "Latin Modern Sans" size angle))

(define (mono str [size 20] [angle 0])
  (text str "Latin Modern Mono" size angle))

(define (mcaps str [size 20] [angle 0])
  (text str "Latin Modern Mono Caps" size angle))

(define (rcaps str [size 20] [angle 0])
  (text str "Latin Modern Roman Caps" size angle))


(define ((big-op op) #:size [size 20] #:below [below (blank 0 0)]
                     . rhss)
  (define the-op (math op (* 3 size)))
  (define rhs (apply hbl-append rhss))
  (define big-op-app (if (< (pict-height rhs) (pict-height the-op))
                         hc-append
                         hbl-append))
  (vl-append
   (big-op-app the-op rhs)
   (scale below 3/4)))

(define big-wedge (big-op "∧"))
(define big-vee (big-op "∨"))
(define big-cap (big-op "∩"))
(define big-cup (big-op "∪"))


(define (parens #:style [style 'round]
                #:offset [offset 0] . bodies)
  (define body (apply hbl-append bodies))
  (define body-height (pict-height body))
  (define-values (l-str r-str)
    (match style
      ['round (values "(" ")")]
      ['square (values "[" "]")]
      ['curly (values "{" "}")]
      ['angle (values "⟨" "⟩")]))
  (let loop ([size 2]
             [scalar 1])
    (define l (scale (math l-str size) 1 scalar))
    (cond
      [(>= (pict-height l) body-height)
       (hc-append offset l body (scale (math r-str size) 1 scalar))]
      [(< scalar 6/4) (loop size (+ scalar 1/4))]
      [else
       (loop (+ 2 size) 1)])))

(define (inference-rule above below
                        #:thickness [thickness 1]
                        #:scale [scalar 1])
  (define w (* scalar (max (pict-width above) (pict-width below))))
  (vc-append (scale above scalar)
             (hline w thickness)
             (scale below scalar)))

(define blue (make-object color% 51 153 255))
(define red (make-object color% 255 80 80))
(define grey (make-object color% 200 200 200))


(define bdd/c
  (or/c pict?
        (list/c pict?
                (recursive-contract bdd/c #:flat)
                (recursive-contract bdd/c #:flat))))

(define (node p)
  (cc-superimpose
   (filled-rounded-rectangle
    (* 1.5 (pict-width p))
    (* 1.5 (pict-height p))
    #:color "white")
   p))

(define edge-width 6)

(define (draw-bdd b)
  (inset
   (naive-layered	
    (let loop ([b b])
      (match b
        [(? pict?) (tree-layout #:pict (node b) #f #f)]
        [(list x l r)
         (tree-layout
          #:pict (node x)
          (tree-edge #:edge-color blue
                     #:edge-width edge-width
                     (loop l))
          (tree-edge #:edge-color red
                     #:edge-width edge-width
                     (loop r)))])))
   5))

(define lbdd/c
  (or/c pict?
        (list/c pict?
                (recursive-contract lbdd/c #:flat)
                (recursive-contract lbdd/c #:flat)
                (recursive-contract lbdd/c #:flat))))

(define (draw-lbdd b)
  (inset
   (naive-layered
          (let loop ([b b])
            (match b
              [(? pict?) (tree-layout #:pict (node b))]
              [(list x l m r)
               (tree-layout
                #:pict (node x)
                (tree-edge #:edge-color blue
                           #:edge-width edge-width
                           (loop l))
                (tree-edge #:edge-color grey
                           #:edge-width edge-width
                           (loop m))
                (tree-edge #:edge-color red
                           #:edge-width edge-width
                           (loop r)))])))
   5))

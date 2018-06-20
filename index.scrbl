#lang scribble/manual
@(require "math-tools.rkt"
          "bib.rkt"
          (except-in scriblib/figure left)
          (rename-in "model.rkt"
                     [render-sst-term rt]
                     [render-peano-term rpt])
          scribble/core
          (except-in pict table)
          racket/date
          scribble/html-properties
          redex/pict)

@elem[#:style 
      (style #f (list (alt-tag "a") 
		      (attributes 
		       '((href . "https://github.com/pnwamk/sst-tutorial/")))))
      @elem[#:style
	    (style #f
	      (list (alt-tag "img")
		    (attributes 
		     '((style . "position: absolute; top: 0; right: 0; border: 0;")
		       (src . "https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png")
		       (alt . "Fork me on GitHub")))))]]



@title{Down and Dirty with Semantic Set-theoretic
 Types (a tutorial) v0.2}

@author+email["Andrew M. Kent" "pnwamk@gmail.com"]

Last updated: @date->string[(current-date)]

@(table-of-contents)

@section{Introduction}

@margin-note{This is a ``living'' document: please submit bug reports
and pull requests if you spot a problem!
@url{https://github.com/pnwamk/sst-tutorial/}}

This is an informal tutorial designed to:

@itemlist[

 @item{(1) briefly introduce semantic set-theoretic types, and}

 @item{(2) describe in detail (i.e. with pseudo code) their
  implementation details.}

]

Most of the "pseudo code" in this tutorial was generated
directly from a @emph{functioning redex model}
@~cite[bib:klein-et-al-2012] to greatly reduce the chance
for typos or other subtle bugs in their presentation.

We would like to thank Giuseppe Castagna for the time he
spent writing the wonderful, detailed manuscript @emph{
 Covariance and Contravariance: a fresh look at an old issue
 (a primer in advanced type systems for learning functional
 programmers)} @~cite[bib:castagna-2013] which we used to
initially understand the implementation details we will
discuss, as well as Alain Frisch, Giuseppe Castagna, and
Véronique Benzaken for their detailed journal article @emph{
 Semantic subtyping: Dealing set-theoretically with function,
 union, intersection, and negation types}
@~cite[bib:frisch-et-al-2008] which contains thorough
mathematical and technical descriptions of semantic
subtyping. Without those works this tutorial would simply
not be possible! We @emph{highly} recommend perusing those
works as well for readers who are interested in this material.

@subsection{Prerequisites}

We assume the reader has some mathematical maturity and is
familiar with terminology commonly used by computer
scientists for describing programming languages and types.
In particular, we assume basic familiarity with:

@itemlist[
 @item{how types are generally used to describe programs,}

 @item{basic set-theoretic concepts,}

 @item{context-free grammars (CFGs), and}
 
 @item{defining functions via pattern matching.}

]

@subsection{Example Grammar and Function Definitions}

To be clear about how our term and function definitions
should be read we start by briefly examining a simple
well-understood domain: Peano natural numbers.

Here is a grammar for Peano naturals and booleans:

@(figure "fig:peano-grammar"
         "a context-free grammar for natural numbers and booleans"
         (render-peano-grammar))

This grammar states that a natural number is either @rpt[O]
(zero) or @rpt[(S p)] (i.e. the successor of some natural
number p), and that a boolean must be either @rpt[#true] or
@rpt[#false].


We can then define the function @rpt[plus] to be addition by
structural recursion on the first argument:

@(figure "fig:peano-plus"
         "addition for natural numbers"
         (render-mf-peano-plus))

Example usages:

@(render= peano (peano-plus (S O) O) (S O))

@(render= peano (peano-plus O (S O)) (S O))

@(render= peano (peano-plus (S (S O)) (S O)) (S (S (S O))))

@rpt[<] will be defined to be a function that returns a
@rpt[bool] indicating whether the first argument is strictly
less than the second:

@(figure "fig:peano-lt"
         "less than for natural numbers"
         (render-mf-peano-lt))

Example usages:

@(render= peano (peano-lt (S O) O) #false)

@(render= peano (peano-lt O (S O)) #true)

@(render= peano (peano-lt (S O) (S (S O))) #true)

@rpt[>] will be defined to be a function that returns a
@rpt[bool] indicating whether the first argument is strictly
greater-than the second:


@(figure "fig:peano-gt"
         "greater than for natural numbers"
         (render-mf-peano-gt))

Example usages:

@(render= peano (peano-gt (S O) O) #true)

@(render= peano (peano-gt O (S O)) #false)

@(render= peano (peano-gt (S O) (S (S O))) #false)


Things to note about these definitions (and about
definitions in general):

@itemlist[
          
 @item{The first line is the @emph{signature} of the
  function and tells us how many arguments the function takes
  in addition to the types the inputs and outputs will have.
  e.g., @rpt[plus] takes two natural numbers and returns a
  natural number;}
  
 @item{Underscores that appear @emph{in the signature} of a
  function are simply indicators for where arguments will go
  for functions with non-standard usage syntax. e.g.,
  @rpt[plus] does not use underscores because we use standard
  function application syntax to talk about its application
  (e.g. @rpt[(plus O O)]), but @rpt[<] and @rpt[>] use
  underscores to indicate they will be used as @emph{infix}
  operators (i.e. we will write @rpt[(peano-lt p q)] inbetween the
  two arguments instead of at the front like @rpt[plus]);}

 @item{Underscores used @emph{within the definition} of the
  function (e.g. the second and third clauses for @rpt[<]) are
  "wild-card" patterns and will match any input;}
 
 @item{The order of function clauses matters! i.e. when
  determining which clause of a function applies to a
  particular input, the function tests each clause @emph{ in
   order} to see if the input matches the pattern(s);}

 @item{In addition to pattern matching against the arguments
  themselves, "where clauses" also act as "pattern matching
  constraints" when present, e.g. @rpt[>]'s first clause
  matches on any naturals @rpt[p] and @rpt[q] @emph{only
   where} @rpt[(peano-lt q p)] equals @rpt[#true].}

 @item{When the same non-terminal variable appears in two
  places in a pattern, that pattern matches only if the same
  term appears in both places. Note: this does not apply to
  function @emph{signatures}, where non-terminals only
  indicate what @emph{kind} of terms are accepted/returned.}
 ]



@bold{Important Note:} We will "overload" certain convenient
symbols during the course of our discussion since we can
always distinguish them based on the types of arguments they
are given. e.g. we will define several "union" operations,
all of which will use the standard set-theoretic symbol
@rt[t-or]. But when we see @rt[(t-or t_1 t_2)] and
@rt[(b-or b_1 b_2)], for example, we will know the former
usage of @rt[t-or] is the version explicitly defined to
operate on @rt[t]'s and the latter is the version explicitly
defined to operate on @rt[b]'s. Like the function
definitions above, @emph{all} function definitions will have
clear signatures describing the kinds of arguments they
operate on.


@section[#:tag "sec:overview"]{Set-theoretic Types: An Overview}

Set-theoretic types are a flexible and natural way for
describing sets of values, featuring intuitive "logical
combinators" in addition to traditional types for creating
detailed specifications.

@(figure "fig:types"
         "set-theoretic types"
         (render-nts '(("Base Types" ι) ("Types" τ σ))))

As @figure-ref["fig:types"] illustrates, languages with
set-theoretic types feature (at least some of) the following
logical type constructors:

@itemlist[
 @item{@rt[(Or τ σ)] is the union of types @rt[τ] and @rt[σ],
  describing values of type @rt[τ] @emph{or} of
  type @rt[σ];}
 @item{@rt[(And τ σ)] is the intersection of types @rt[τ] and
  @rt[σ], describing values of both type @rt[τ]
  @emph{and} @rt[σ];}
 @item{@rt[(Not τ)] is the complement (or negation) of
  type @rt[τ], describing values @emph{not} of type
  @rt[τ];}
 @item{@rt[Any] is the type describing all possible values; and}
 @item{@rt[Empty] is the type describing no values (i.e. @rt[(Not Any)]).}]

Additionally, we may specify "specific top types", which for
each kind of atomic type denotes all values of that
particular kind:

@itemlist[
 @item{@rt[Any-Pair] is the type that denotes all pairs,}
 @item{@rt[Any-Fun] is the type that denotes all function, and}
 @item{@rt[Any-Base] is the type that denotes all base
  values (i.e. integers, strings, and booleans).}]

@(figure
  "fig:specific-top-types"
  "specific top type definitions"
  (vc-append
   15
   (render≈ Any-Pair (Pair Any Any))
   (render≈ Any-Fun (Fun Empty Any))
   (render≈ Any-Base (Not (Or Any-Pair Any-Fun)))))

Set theoretic types frequently appear in type systems which
reason about dynamically typed languages (e.g. TypeScript,
Flow, Typed Racket, Typed Clojure, Julia), but some statically
typed languages use them as well (e.g. CDuce, Pony).

@subsection{Subtyping}

With set-theoretic types, the programmer (and the type
system) must be able to reason about how types that are not
equivalent relate. i.e., even though @rt[τ] is not the same
type as @rt[σ], is it the case that a value of type @rt[τ]
will necessarrily also be a value of type @rt[σ]? In other
words, does @rt[(<: τ σ)] hold (i.e. is τ a subtype of σ)?

For example, consider the following subtyping question:

@(centered
  (render-valid-judgment
   (<: (Pair (Or Int Str) Str) (Or (Pair Int Str) (Pair Str Str)))))

Clearly the two types are not equal... but we can also see
that any pair whose first element is either an integer or a
string and whose second element is a string (i.e. the type
on the left-hand side) is indeed either a pair with an
integer and a string or a pair with a string and a string
(i.e. the type on the right-hand side). As a programmer then
we might reasonably expect that anywhere a
@rt[(Or (Pair Int Str) (Pair Str Str))] is expected, we
could provide a value of type @rt[(Pair (Or Int Str) Str)]
and things should work just fine.

Unfortunately, many (most?) systems that feature
set-theoretic types use sound but incomplete reasoning to
determine subtyping. This is because most type
systems reason about subtyping via syntactic inference
rules:

@(centered
  (vc-append
   50
   (hbl-append
    30
    (inference-rule
     (blank 5 5)
     (rt (<: Empty τ)))
    (inference-rule
     (blank 5 5)
     (rt (<: τ Any)))
    (inference-rule
     (blank 5 5)
     (rt (<: τ τ)))
    (inference-rule
     (hbl-append 20 (rt (<: τ_1 τ_2)) (rt (<: σ_1 σ_2)))
     (rt (<: (Pair τ_1 τ_2) (Pair σ_1 σ_2)))))
   (hbl-append
    30
    (inference-rule
     (rt (<: σ τ_1))
     (rt (<: σ (Or τ_1 τ_2))))
    (inference-rule
     (rt (<: σ τ_2))
     (rt (<: σ (Or τ_1 τ_2))))
    (inference-rule
     (hbl-append 20 (rt (<: τ_1 σ)) (rt (<: τ_2 σ)))
     (rt (<: (Or τ_1 τ_2) σ))))))

These rules allow us to conclude the statement below the
line if we can show that the statement(s) above the line
hold. Upsides to using a system built from rules like this
include (1) the rules can often directly be translated into
efficient code and (2) we can generally examine each rule
individually and decide if the antecedants necessarily imply
the consequent (i.e. determine if the rule valid). The
downside is that such rules are incomplete for set-theoretic
types: it is impossible to derive all valid subtyping
judgments, e.g. we cannot conclude
@rt[(Pair (Or Int Str) Str)] is a subtype of
@rt[(Or (Pair Int Str) (Pair Str Str))] even though it is
true.

For a complete treatment of subtyping for set-theoretic
types, a @emph{semantic} (instead of a syntactic) notion of
subtyping is required.

@margin-note{At the time of writing this tutorial, CDuce may
 be the only example of an in-use language with a type system
 which features set-theoretic types @emph{and} complete
 subtyping. This is not surprising since its developers are
 also the researchers that have pioneered the approaches we
 will discuss.}


@subsection[#:tag "sec:semantic-subtyping"]{Semantic Subtyping}

Instead of using a syntactic approach to reason about
subtyping, we will instead us a semantic approach: types
will simply denote sets values in the language in the
expected way.

@itemlist[
 @item{@rt[True] denotes singleton set @rt[(Set #true)];}
 @item{@rt[False] denotes singleton set @rt[(Set #false)];}
 @item{@rt[Int] denotes the set of integers;}
 @item{@rt[Str] denotes the set of strings;}
 @item{@rt[(Pair τ σ)] denotes the set of pairs whose first
  element is a value in @rt[τ] and whose second element is a
  value in @rt[σ] (i.e. the cartesian product of @rt[τ] and @rt[σ]);}
 @item{@rt[(Fun τ σ)] denotes the set of functions which can
  be applied to a value in @rt[τ] and will return a value from
  @rt[σ] (if they return);}
 @item{@rt[(Or τ σ)] denotes the union of the sets denoted
  by @rt[τ] and @rt[σ];}
 @item{@rt[(And τ σ)] denotes the intersection of the sets denoted
  by @rt[τ] and @rt[σ];}
 @item{@rt[(Not τ)] denotes the complement of the set denoted
  by @rt[τ];}
 @item{@rt[Any] denotes the set of all values; and}
 @item{@rt[Empty] denotes the empty set.}]

@margin-note{Our description here omits many interesting
 subtleties and details about why this approach more or less
 "just works"; @citet[bib:frisch-et-al-2008] @emph{
  thoroughly} discuss this topic and much more and should be
 consulted if the reader is so inclined.}

With our types merely denoting sets of values, subtyping can
be determined by deciding type inhabitation
(@figure-ref["fig:subtype-is-emptiness"]).

@(figure "fig:subtype-is-emptiness"
         "subtyping/inhabitation equivalence"
  (hbl-append
   @rt[(<: τ σ)]
   ($ " iff ")
   @rt[τ] ($ " ⊆ ") @rt[σ]
   ($ " iff ")
   @rt[τ] ($ " \\ ") @rt[σ] ($ " = ∅")
   ($ " iff ")
   @rt[τ] ($ " ∩ ") (inference-rule (blank 0 0) @rt[σ]) ($ " = ∅")
   ($ " iff ")
   @rt[(And τ (Not σ))]
   ($ " = ∅")))

In other words, @bold{"is a particular type inhabited" is
 really the only question we have to be able to answer} since
asking @rt[(<: τ σ)] is the same as asking if
@rt[(And τ (Not σ))] is uninhabited (i.e. does it denote the
empty set?).
 

@subsection[#:tag "sec:norm-forms"]{Deciding Inhabitation, Normal Forms}

To efficiently decide type inhabitation for set-theoretic
types we leverage some of the same strategies used to decide
boolean satisfiability:

@itemlist[
 @item{types are kept in disjunctive normal form (DNF), and}
 @item{special data structures are used to efficiently
  represent DNF types.}]

@subsubsection{Types in Disjunctive Normal Form}

In addition to using DNF, it will be helpful to impose some
additional structure to our normal form for types. To
illustrate, first let us note that a DNF boolean formula
@($ "D"):

@(centered
  (hc-append
   10
   ($ "D = ")
   (vl-append
    5
    (parens
     ($ "x" (msub "3") " ∧ ¬x" (msub "7") " ∧ x" (msub "13") " ∧ ..."))
    ($ "∨ "
       (parens
        ($ "x" (msub "11") " ∧ x" (msub "4") " ∧ ¬x" (msub "1") " ∧ ¬x" (msub "2") " ∧ ...")))
    ($ "∨ "
       (parens
        ($ "¬x" (msub "3") " ∧ ¬x" (msub "14") " ∧ x" (msub "1") " ∧ ...")))
    ($ "∨ ..."))))

can be reorganized slightly to "group" the positive and
negative atoms in each conjunction:

@(centered
  (hc-append
   10
   ($ "D = ")
   (vl-append
    5
    (parens
     (parens ($ "x" (msub "3") " ∧ x" (msub "13") " ∧ ..."))
     ($ " ∧ ")
     (parens ($ "¬x" (msub "7") " ∧ ...")))
    ($ "∨ "
       (parens
        (parens ($ "x" (msub "11") " ∧ x" (msub "4") " ∧ ..."))
        ($ " ∧ ")
        (parens ($ "¬x" (msub "1") " ∧ ¬x" (msub "2") " ∧ ..."))))
    ($ "∨ "
       (parens
        (parens
         ($ "x" (msub "1") " ∧ ..."))
        ($ " ∧ ")
        (parens
         ($ "¬x" (msub "3") " ∧ ¬x" (msub "14") " ∧ ..."))))
    ($ "∨ ..."))))

We then observe that @${D} can be described as a set of
pairs, with one pair @${(P,N)} for each clause in the original
disjunction, where @${P} is the set of positive atoms in the
clause and @${N} is the set of negated atoms in the clause:


@(centered
  (hc-append
   10
   ($ "D = ")
   (big-vee (parens
             (hc-append
              (parens (big-wedge ($ "x") #:below ($ "x ∈ P"))))
             ($ " ∧ ")
             (parens (big-wedge ($ "¬x") #:below ($ "x ∈ N"))))
            #:below (hbl-append ($ "(P,N) ∈ D")))))


Similarly, any type @rt[τ] can be converted into a DNF, i.e.
a set of pairs @${(P,N)}, where for each clause @${(P,N)},
@${P} contains the positive atoms (written @math{a}) which
are either a base type (@rt[ι]), a product type
(@rt[(Pair τ_1 τ_2)]), or function type
(@rt[(Fun τ_1 τ_2)]), and @${N} contains the negated atoms:

@(centered
  (hc-append
   10
   ($ "τ = ")
   (big-vee
    (parens
     (hc-append
      (parens (big-wedge ($ "a") #:below ($ "a ∈ P" )))
      ($ " ∧ ")
      (parens (big-wedge ($ "¬a") #:below ($ "a ∈ N")))))
    #:below (hbl-append ($ "(P,N) ∈ τ")))))


@subsubsection[#:tag "sec:partitioning-types"]{Partitioning Types}

In addition to being able to convert any type into DNF, for
any type @rt[τ] there exists three specialized types
@$[@rt[τ] (msup "ι")], @$[@rt[τ] (msup "×")], and
@$[@rt[τ] (msup "→")] which contain @emph{only atoms of the
 same kind} such that:

@(define base-part @$[@rt[τ] (msup "ι")])

@(define pict:type-equiv-union-3-types
   @$[@rt[τ] " = " "(" @rt[Any-Base] " ∧ " base-part ") ∨ ("
      @rt[Any-Pair] " ∧ " @rt[τ] (msup "×") ") ∨ ("
      @rt[Any-Fun] " ∧ " @rt[τ] (msup "→") ")"])

@centered[pict:type-equiv-union-3-types]

By representing a type in this way, we can efficiently
divide types into non-overlapping segments which can each
have their own DNF representation.

i.e., @base-part is a type whose atoms are all base types:

@(define pict:base-portion-DNF
   (hc-append
    ($ base-part " = ")
    (big-vee
     (parens
      (hc-append
       (parens (big-wedge ($ "ι") #:below ($ "ι ∈ P")))
       ($ " ∧ ")
       (parens (big-wedge ($ "¬ι") #:below ($ "ι ∈ N")))))
     #:below (hbl-append ($ "(P,N) ∈ τ" (msup "ι"))))))

@centered[pict:base-portion-DNF]

@(define prod-part @$[@rt[τ] (msup "×")])

@prod-part is a DNF type whose atoms are all product types:

@(define pict:product-portion-DNF
   (hc-append
    ($ prod-part " = ")
    (big-vee
     (parens
      (hc-append
       (parens (big-wedge @rt[(Pair τ_1 τ_2)]
                          #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ P")))
       ($ " ∧ ")
       (parens (big-wedge @rt[(Not (Pair τ_1 τ_2))]
                          #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ N")))))
     #:below (hbl-append ($ "(P,N) ∈ τ" (msup "×"))))))

@centered[pict:product-portion-DNF]

@(define arrow-part @$[@rt[τ] (msup "→")])

and @arrow-part is a DNF type whose atoms are all function
types:

@(define pict:function-portion-DNF
   (hc-append
    ($ arrow-part " = ")
    (big-vee
     (parens
      (hc-append
       (parens (big-wedge @rt[(Fun τ_1 τ_2)]
                          #:below ($ "(" @rt[(Fun τ_1 τ_2)] ") ∈ P")))
       ($ " ∧ ")
       (parens (big-wedge @rt[(Not (Fun τ_1 τ_2))]
                       #:below ($ "(" @rt[(Fun τ_1 τ_2)] ") ∈ N")))))
     #:below (hbl-append ($ "(P,N) ∈ τ" (msup "→"))))))

@centered[pict:function-portion-DNF]

To illustrate what this partitioning looks like in practice,
here are a few very simple types and their equivalent
"partitioned" representation:

@(render≈
  Empty
  (Or (And Any-Base Empty) (Or (And Any-Pair Empty) (And Any-Fun Empty))))
@(render≈
  Any
  (Or (And Any-Base Any) (Or (And Any-Pair Any) (And Any-Fun Any))))
@(render≈
  Int
  (Or (And Any-Base Int) (Or (And Any-Pair Empty) (And Any-Fun Empty))))
@(render≈
  (Pair Int Str)
  (Or (And Any-Base Empty) (Or (And Any-Pair (Pair Int Str)) (And Any-Fun Empty))))
@(render≈
  (Fun Int Str)
  (Or (And Any-Base Empty) (Or (And Any-Pair Empty) (And Any-Fun (Fun Int Str)))))
@(render≈
  (Or Int (Pair Int Str))
  (Or (And Any-Base Int) (Or (And Any-Pair (Pair Int Str)) (And Any-Fun Empty))))

This technique for partitioning types into separate
non-overlapping DNFs---which will inform our strategy for
actually representing types as data structures---will make
type inhabitation inquiries easier to implement since we're
specializing our representation to describe only the
interesting, non-trivial clauses in a type. We summarize
this discussion's key takeaway in
@figure-ref["fig:type-canonical-form"] for reference.

@(figure "fig:type-canonical-form"
         "canonical form for representing types"
         (vl-append
          20
          (vl-append
           5
           ($ "For any type " @rt[τ] " there exists specialized DNF types "
              @base-part
              ", "
              @prod-part
              ", and "
              @arrow-part
              " such that")
           ($ "all of the following hold:"))
          pict:type-equiv-union-3-types
          pict:base-portion-DNF
          pict:product-portion-DNF
          pict:function-portion-DNF))


@section[#:tag "sec:rep"]{Set-theoretic Type Representation}

In @secref["sec:overview"] we determined that

@itemlist[
 @item{many type-related inquiries for set-theoretic types
  can be reduced to deciding type inhabitation (see
  @secref["sec:semantic-subtyping"]), and that because of
  this}
 @item{a partitioned DNF representation (summarized in
  @figure-ref["fig:type-canonical-form"]) may be useful.}]

In this section we focus on the latter issue: type
representation (since how we represent types impacts how our
algorithms decide type inhabitation). We will introduce
several data structures, defining for each the binary
operators union ("∪"), intersection ("∩"), and difference
("\") and the unary operator complement ("¬").

@subsection{Types as Data Structures}

In @figure-ref["fig:type-canonical-form"] we noted a type
can be conveniently deconstructed into three partitions,
allowing us to reason separately about the base type
(@base-part), product type
(@prod-part), and function type
(@arrow-part) portion of a type:

@(centered pict:type-equiv-union-3-types)

Our representation of types will exactly mirror this structure.

@(figure "fig:type-rep"
         "internal type representation"
         (render-nts '(("Types (internal representation)" t s))))

As @figure-ref["fig:type-rep"] indicates, our internal
representation of types is a 3-tuple:

@itemlist[
 @item{@rt[β] (the first field) is the portion which
  contains base type information, corresponding to
  @base-part;}
 @item{@rt[Prodb] (the second field) is the portion
  corresponding to product type information, corresponding to
  @prod-part; and}
 @item{@rt[Arrowb] (the third field) is the portion
  corresponding to function type information, corresponding to
  @arrow-part;}]

The specific top types used in
@figure-ref["fig:type-canonical-form"] are implicit in our
representation, i.e. we know what kind of type-information
each field is responsible for so we need not explicitly keep
around @rt[Any-Base], @rt[Any-Pair], and @rt[Any-Fun] in our
partitioned representation.

The grammar and meaning for @rt[β] will be given in
@secref["sec:base-type-rep"] and for @rt[Prodb] and
@rt[Arrowb] will be given in @secref["sec:bdd-rep"].

@subsubsection{Top and Bottom Type Representation}

The representation of the "top type" @rt[Any]---the type
that denotes all values---is written @rt[Any-t] and is
defined by placing the top @rt[β], @rt[Prodb], and
@rt[Arrowb] in each respective field
(@figure-ref["fig:top-type-rep"]).

@(figure "fig:top-type-rep"
         "top type representation"
         (render= sst Any-t (Type (Base - (Set)) Top Top)))

This mirrors the previous "partitioned" version of @rt[Any]
we showed earlier:

@(render≈
  Any
  (Or (And Any-Base Any) (Or (And Any-Pair Any) (And Any-Fun Any))))

The representation of the "bottom type" @rt[Empty]---the
type that denotes no values---is written @rt[Empty-t] and is
defined by placing the bottom @rt[β], @rt[Prodb], and
@rt[Arrowb] in each respective field
(@figure-ref["fig:bot-type-rep"]).

@(figure "fig:bot-type-rep"
         "bottom type representation"
         (render= sst Empty-t (Type (Base + (Set)) Bot Bot)))

Again, this mirrors the previous "partitioned" version of
@rt[Empty] we showed earlier:

@(render≈
  Empty
  (Or (And Any-Base Empty) (Or (And Any-Pair Empty) (And Any-Fun Empty))))

@Figure-ref["fig:specific-top-type-rep"] describes how we
represent the specific top types @rt[Any-Base],
@rt[Any-Pair], and @rt[Any-Fun] as data structures.

@figure["fig:specific-top-type-rep"
        "specific top type representations"
        (vc-append
         15
         (render= sst Any-Base-t (Type (Base - (Set)) Bot Bot))
         (render= sst Any-Prod-t (Type (Base + (Set)) Top Bot))
         (render= sst Any-Fun-t  (Type (Base + (Set)) Bot Top)))]

@subsubsection{Type Operations}

Binary operations on types benefit from our partitioned
design: each is defined pointwise in the natural way on
each partition; type complement is defined in terms of type
difference, subtracting the negated type from the top type
(@figure-ref["fig:type-ops"]).

@(figure "fig:type-ops"
         "internal type operations"
  (vl-append
   30
   (render-mf-t-or)
   (render-mf-t-and)
   (render-mf-t-diff)
   (render-mf-t-not)))

@subsection[#:tag "sec:base-type-rep"]{Base DNF Representation}

We now examine how a DNF type with only base type atoms can
be efficiently represented (i.e. the base portion @base-part
of a type described in
@figure-ref["fig:type-canonical-form"] and the @rt[β] field
in our representation of types described in
@figure-ref["fig:type-rep"]).

Although any type can be represented by some DNF type, in
the case of base types things can be simplified even
further! Any DNF type @base-part whose atoms are all base
types is equivalent to either

@itemlist[      
 @item{a union of base types
  @($ "(" @rt[ι_0] " ∨ " @rt[ι_1] " ∨ ...)"), or}

 @item{a negated union of base types
  @($ "¬("@rt[ι_0] " ∨ " @rt[ι_1] " ∨ ...)").}]

To see why this is the case, it may be helpful to recall
that each base type is disjoint (i.e. no values inhabit more
than one base type), note that this is obviously true for
@rt[Any], @rt[Empty], and any a single base type @rt[ι] or
negated base type @rt[(Not ι)], and then examine the
definitions of base type operations presented in
@figure-ref["fig:base-type-ops"] and note how the
representation is naturally maintained.

Because any DNF of base types can be represented by a set of
base types (i.e. the elements in the union) and a polarity
(i.e. is the union negated or not), we represent the base
portion of a type @rt[β] using a tuple with these two pieces
of information (@figure-ref["fig:base-type-rep"]).

@(figure
  "fig:base-type-rep"
  "internal base type representation"
  (render-nts '(("Base type representation" β)
                ("Base set polarity" ±)
                ("Base set" B))))

The first field is the polarity flag (either @rt[+] for a
union or @rt[-] for a negated union) and the second field
is the set of base types @rt[B] in the union.

The top base type (i.e. the type which denotes all base type
values) is a negated empty set @rt[(Base - (Set))] (i.e. it
is not the case that this type contains no base values) and
the bottom base type (the type which denotes no base type
values) is a positive empty set @rt[(Base + (Set))] (i.e. it
is the case that this type contains no base values).

@subsubsection{Base DNF Operations}

Operations on these base type representations boil down to
selecting the appropriate set-theoretic operation to combine
the sets based on the polarities
(@figure-ref["fig:base-type-ops"]).

@(figure "fig:base-type-ops"
         "internal base DNF operations"
         (vc-append
          10
          (render-mf-Base-or)
          (render-mf-Base-and)
          (render-mf-Base-diff)))

Base type negation is not shown (because it is not used
anywhere in this model), but would simply require "flipping"
the polarity flag (i.e. the first field in the tuple).

@subsection[#:tag "sec:bdd-rep"]{Product and Function DNFs}

In order to efficiently represent a DNF type with only
product or function type atoms (i.e. the @prod-part and
@arrow-part portions of a type described in
@figure-ref["fig:type-canonical-form"] and the @rt[Prodb]
and @rt[Arrowb] fields in our type representation described
in @figure-ref["fig:type-rep"]) we will use a binary decision
diagram (BDD).

First we include a brief review of how BDDs work, then we
discuss how they can be used effectively to represent our
product/function DNF types.


@subsubsection{Binary Decision Diagrams}

A binary decision diagram (BDD) is a tree-like data structure
which provides a convenient way to represent sets or
relations.

For example, the boolean formula
@$["(¬x∧¬y∧¬z)∨(x∧y)∨(y∧z)"] has truth table:

@(centered
  (tabular
   #:style 'boxed
   #:sep @hspace[5]
   #:column-properties '(left left left center)
   #:row-properties '(bottom-border ())
   (list (list @${x} @${y} @${z} @${(¬x∧¬y∧¬z)∨(x∧y)∨(y∧z)})
         (list "0" "0" "0" "1")
         (list "0" "0" "1" "0")
         (list "0" "1" "0" "0")
         (list "0" "1" "1" "1")
         (list "1" "0" "0" "0")
         (list "1" "0" "1" "0")
         (list "1" "1" "0" "1")
         (list "1" "1" "1" "1"))))

and can be represented with the following BDD:

@(centered
  (draw-bdd
   (list ($ "x")
         (list ($ "y")
               (list ($ "z")
                     (rt Top)
                     (rt Top))
               (list ($ "z")
                     (rt Bot)
                     (rt Bot)))
         (list ($ "y")
               (list ($ "z")
                     (rt Top)
                     (rt Bot))
               (list ($ "z")
                     (rt Bot)
                     (rt Top))))))

Each node in the tree contains a boolean variable. A node's
left subtree (the blue edge) describes the "residual
formula" for when that variable is true. The node's right
subtree (the red edge) describes the "residual formula" when
that variable is false. We invite the reader to compare the
truth table and corresponding BDD until they are convinced
they indeed represent the same boolean formula. It may be
useful to observe that the leaves in the BDD correspond to
the right-most column in the truth table.

@subsubsection{Types as BDDs?}

BDDs can also naturally encode set-theoretic types (in our
case, DNF product or function types). Each node has a
function/product type associated with it; henceforth we will
call this associated type the @emph{atom} of the node. A
node's left sub-tree (the blue edge) describes the "residual
type" for when the atom is included in the overall type. A
node's right sub-tree (the red edge) describes the "residual
type" for when the atom's @emph{negation} is included in
the overall type.

For example, here we have encoded the type
@rt[(Pair Int Int)]:

@(centered
  (draw-bdd
   (list (rt (Pair Int Int))
         (rt Top)
         (rt Bot))))

and here is
@rt[(Or (Pair Int Int) (Pair Str Str))]:

@(centered
  (draw-bdd
   (list (rt (Pair Int Int))
         (rt Top)
         (list (rt (Pair Str Str))
               (rt Top)
               (rt Bot)))))

Basically, each path in the tree represents a clause in the
overall DNF, so the overall type is the union of all the
possibly inhabited paths (i.e. paths that end in @(rt Top)).

In other words, for an arbitrary (type) BDD

@(centered
  (hc-append
   @rt[b] ($ " = ")
   (draw-bdd
    (list (rt τ)
          (rt b_l)
          (rt b_r)))))

we would interpret the meaning of @rt[b] (written〚@rt[b]〛)
as follows:

@(centered
  (hc-append
   ($ "〚" @rt[b] "〛 = ")
   ($ "(" @rt[τ] " ∧ 〚" @rt[b_l] "〛) ∨ ("
      @rt[(Not τ)] " ∧ 〚" @rt[b_r]  "〛)")))

where @rt[Top] is interpreted as @rt[Any] and @rt[Bot] as
@rt[Empty].

There is, however, a well-known problem with BDDs we will
want to avoid: repeatedly unioning trees can lead to
significant increases in size. This is particularly
frustrating because---as we have previously noted---our
primary concern algorithmically is deciding type
inhabitation and taking the union of two types will have no
@emph{interesting} impact with respect to inhabitation. This
is because the union of two types is uninhabited only when
both the individual types themselves are already
uninhabited.

@subsubsection[#:tag "sec:types-as-lazy-bdds"]{Types as Lazy BDDs!}

Because there is no interesting impact on inhabitation when
computing unions, we can use "lazy" BDDs---whose unions are
only fully expand when computing type intersection or
difference (i.e. operations that @emph{can} have an
interesting impact on inhabitation)---to represent our
function/product DNF types.

Nodes in lazy BDDs have---in addition to the left and right
subtrees described before---a "middle" subtree (the grey
edge) which assumes nothing about its node's atom.

In other words, for an arbitrary lazy (type) BDD

@(centered
  (hc-append
   @rt[b] ($ " = ")
   (draw-lbdd
    (list (rt τ)
          (rt b_l)
          (rt b_m)
          (rt b_r)))))

we would interpret the meaning of @rt[b] (written〚@rt[b]〛)
as follows:

@(centered
  ($ "〚" @rt[b] "〛 = "
     "(" @rt[τ] " ∧ 〚" @rt[b_l] "〛) ∨ 〚"
     @rt[b_m] "〛 ∨ ("
     @rt[(Not τ)] " ∧ 〚" @rt[b_r]  "〛)"))

where @rt[Top] is interpreted as @rt[Any] and @rt[Bot] as
@rt[Empty].


@(figure "fig:bdd-rep-grammar"
         "lazy function/product type BDDs"
         (render-nts '(("(Lazy) BDD" b)
               ("BDD Node" n)
               ("BDD Atom" a)
               ("BDD of Product Types" Prodb)
               ("BDD of Function Types" Arrowb))))

@Figure-ref["fig:bdd-rep-grammar"] describes in detail our
representation for the DNF function/product portions of a type
as lazy BDDs. Note that

@itemlist[
 @item{@rt[b] describes a lazy BDD of either
  functions or products and is useful for describing functions
  that are parametric w.r.t. which kind of atom they
  contain;}

 @item{@rt[Top] and @rt[Bot] are the leaves in our BDDs,
  interpreted as @rt[Any] and @rt[Empty] respectively;}

 @item{@rt[n] describes a BDD node: a 4-tuple with an atom
  and three sub-trees (whose meaning are described earlier in
  this section);}

 @item{an atom (@rt[a]) is either a product or a function
  type---a given BDD will only contain atoms of one kind or
  the other; and}

 @item{@rt[Prodb] and @rt[Arrowb] simply allow us to be more
  specific and describe what kind of atoms a particular @rt[b]
  contains.}]


Although not explicit in the grammar, these trees are
constructed using an ordering on atoms that we leave up to
the reader to implement (note that this implies types, BDDs,
etc must all also have an ordering defined since these data
structures are mutually dependent). A simple lexicographic
ordering would work... a fast (possibly non-deterministic?)
hash code-based ordering should also work... the choice is
yours. The ordering---written @rt[(less-than a_1 a_2)] and
the like---will be called upon frequently in function
definitions for BDDs in the next section. The ordering
allows us to have consistent representations for BDDs with
similar elements.

To make some definitions more clear, "accessor functions"
which extract the various fields of a node are defined in
@figure-ref["fig:node-accessors"].

@(figure "fig:node-accessors"
         "accessors for node fields"
         (vl-append
          30
          (render-mf-atom)
          (render-mf-left)
          (render-mf-mid)
          (render-mf-right)))

Finally, we use a "smart constructor"---defined in
@figure-ref["fig:node-smart-constructor"]---to perform some
obvious simplifications when constructing a BDD node. We use
an implicit syntax for the smart constructor (i.e. it looks
identical to constructing a normal node), so whenever we
construct a node (except of course on the right-hand side of
the definition in @figure-ref["fig:node-smart-constructor"])
we are using the smart constructor.

@(figure "fig:node-smart-constructor"
         "simplifying constructor for BDD nodes"
         (render-mf-node))

@subsubsection{Lazy BDD Operations}

The operations on lazy BDDs can be understood by again
considering how we logically interpret a BDD:

@(centered
  (vl-append
   5
   ($ "〚" @rt[Top] "〛 = " @rt[Any])
   ($ "〚" @rt[Bot] "〛 = " @rt[Empty])
   ($ "〚" @rt[(Node a b_l b_m b_r)] "〛 = "
      "(" @rt[τ] " ∧ 〚" @rt[b_l] "〛) ∨ 〚"
      @rt[b_m] "〛 ∨ ("
      @rt[(Not τ)] " ∧ 〚" @rt[b_r]  "〛)")))

Also, recall that BDD binary operations will only ever be
used on two BDDs with atoms of the same kind.

@Figure-ref["fig:bdd-or"] describes BDD union, i.e. logical
"or".

@(figure
  "fig:bdd-or"
  "BDD union"
  (render-mf-b-or))

@Figure-ref["fig:bdd-and"] describes BDD intersection, i.e.
logical "and".

@(figure
  "fig:bdd-and"
  "BDD intersection"
  (render-mf-b-and))

@Figure-ref["fig:bdd-diff"] describes BDD difference.

@(figure
  "fig:bdd-diff"
  "BDD difference"
  (render-mf-b-diff))

@Figure-ref["fig:bdd-not"] describes BDD complement, i.e.
logical "not".

@(figure
  "fig:bdd-not"
  "BDD complement"
  (render-mf-b-not))

@subsection{Parsing and Example Types}

@Figure-ref["fig:parse-type"] defines a function that
converts the user-friendly types shown in
@figure-ref["fig:types"] into the internal representation we
have just finished describing:

@(figure "fig:parse-type"
         "type parsing function"
         (render-mf-parse))

Here are a few simple examples of what types look like once
parsed:

@(render= sst (parse Int)
          (Type (Base + (Set Int)) Bot Bot))

@(render= sst (parse (Or Int Str))
          (Type (Base + (Set Str Int)) Bot Bot))

@(render= sst (parse (Pair Int Int))
          (Type (Base + (Set)) (Node (× (Type (Base + (Set Int)) Bot Bot) (Type (Base + (Set Int)) Bot Bot)) Top Bot Bot) Bot)
          #:newline)

@(render= sst (parse (Fun Str Int))
          (Type (Base + (Set)) Bot (Node (→ (Type (Base + (Set Str)) Bot Bot) (Type (Base + (Set Int)) Bot Bot)) Top Bot Bot))
          #:newline)


@section{Implementing Semantic Subtyping}

Because we are working with set-theoretic types, we are free
to define subtyping purely in terms of type inhabitation
(see @figure-ref["fig:subtype-is-emptiness"]), which is
precisely what we do (@figure-ref["fig:subtyping"]).

@(figure
  "fig:subtyping"
  "(semantic) subtyping, defined in terms of type emptiness"
  (render-mf-subtype))


In the remainder of this section we examine how to decide
type inhabitation using the data structures introduced
in @secref["sec:rep"].

@subsection{Deciding Type Inhabitation}

A DNF type is uninhabited exactly when @emph{each clause in
 the overall disjunction is uninhabited}. With our DNF types
partitioned into base, product, and function segments (see
@figure-ref["fig:type-canonical-form"]):

@(centered pict:type-equiv-union-3-types)

we simply need ways to check if the base component
(@base-part), product component (@prod-part), and function
component (@arrow-part) each are empty.

@(figure "fig:empty-type"
         "type emptiness predicate"
         (render-mf-empty))

As @figure-ref["fig:empty-type"] suggests, the
representation of the base type portion is simple enough
that we can pattern match on it directly to check if it is
empty (recall that @rt[(Base + (Set))] is the bottom/empty
@rt[β]).

For deciding if the product and function components---which
are represented with lazy BDDs (see previous discussion in
@secref["sec:types-as-lazy-bdds"])---are empty, we rely on
helper functions @rt[emptyP] and @rt[emptyA] which are
defined in @secref["sec:prod-inhabitation"] and
@secref["sec:arrow-inhabitation"] respectively.

In these sections, we will use non-terminals @rt[P] and
@rt[N] to represent a collection of atoms
(see @figure-ref["fig:atom-sets"]).

@(figure "fig:atom-sets"
         "sets of atoms"
         (render-nts '(("Positive set of atoms" P)
                       ("Negative set of atoms" N))))

@subsubsection[#:tag "sec:prod-inhabitation"]{Product Type Inhabitation}

To decide if the product portion of a type is uninhabited,
we recall (from @secref["sec:partitioning-types"]) that it
is a union of conjunctive clauses, each of which can be
described with a pair of two sets (P,N), where P contains
the positive product types and N the negated product types:

@(centered pict:product-portion-DNF)

For @prod-part to be uninhabited, @emph{each} such clause
must be uninhabited. Checking that a given clause (P,N) is
uninhabited occurs in two steps:

@itemlist[
 #:style 'ordered
 @item{accumulate the positive type information in P into a
  single product @rt[(× s_1 s_2)] (i.e. fold over the products
  in P, accumulating their pairwise intersection in a single
  product),}
 @item{check that for each N′ ⊆ N the following holds:}]

@(centered
  (hc-append
   (parens
    (hc-append
     @rt[s_1]
     ($ " <: ")
     (big-vee @rt[t_1]
              #:below ($ "(" @rt[(× t_1 t_2)] ") ∈ N′"))))
   ($ " or ")
   (parens
    (hc-append
     @rt[s_2]
     ($ " <: ")
     (big-vee @rt[t_2]
              #:below ($ "(" @rt[(× t_1 t_2)] ") ∈ N \\ N′"))))))


The first step is justified because pairs are covariant in
their fields, i.e. if something is a @rt[(Pair τ_1 τ_2)]
@emph{and} a @rt[(Pair σ_1 σ_2)] then it is also a
@rt[(Pair (And τ_1 σ_1) (And τ_2 σ_2))].

The second step is more complicated. To understand, let us
first note that if we know something is a product of some
sort and also that it is of type @rt[(Not (Pair τ_1 τ_2))],
then either it is a product whose first field is
@rt[(Not τ_1)] @emph{or} it is a product whose second field
is @rt[(Not τ_2)] (i.e. logically we are applying DeMorgan's
law). And for the clause to be uninhabited, it must be
uninhabited for both possibilities (again, intuitively an
"or" is false only when all its elements are false). So by
exploring each subset @${N′ ⊆ N} and verifying that either the
left-hand side of the product is empty with the negated
left-hand sides in N @emph{or} the right-hand side is empty
for the negated right-hand sides in the complement (i.e. @${
 N \ N′}), we are exploring all possible combinations of negated
first and second fields from N and ensuring each possible
combination is indeed uninhabited.

We describe an algorithm to perform these computations in
@figure-ref["fig:empty-prod"]. The function @rt[emptyP]
walks over each path in the product BDD accumulating the
positive information in a product @rt[(× s_1 s_2)] and the
negative information in the set N. Then at each non-trivial
leaf in the BDD, we call the helper function @rt[emptyP-aux]
which searches the space of possible negation combinations
ensuring that for each possibility the pair ends up being
uninhabited.

@(figure
  "fig:empty-prod"
  "functions for checking if a product BDD is uninhabited"
  (vl-append
     30
     (parameterize ([linebreaks '(#f #f #t #t)])
       (render-mf-empty-prod))
     (parameterize ([linebreaks '(#f #f #t)])
       (render-mf-empty-prod-aux))))

Note that @rt[emptyP-aux] is designed with a "short-circuiting"
behavior, i.e. as we are exploring each possible combination
of negations, if a negated field we are considering would
negate the corresponding positive field (e.g.
@rt[(subtype s_1 t_1)]) then we can stop searching for
emptiness on that side, otherwise we subtract that negated
type from the corresponding field and we keep searching the
remaining possible negations checking for emptiness. If we
reach the base case when N is the empty set, then we have
failed to show the product is empty and we return false.
(Note that @rt[emptyP] checks for emptiness before calling
@rt[emptyP-aux], otherwise we would need to check @rt[s_1] and
@rt[s_2] for emptiness in the base case of @rt[emptyP-aux]).

@subsubsection[#:tag "sec:arrow-inhabitation"]{Function Type Inhabitation}

Just like with products, to show that the function portion
of a type is uninhabited we show that each clause in the
DNF---

@(centered pict:function-portion-DNF)

---i.e. each pair (P,N) where P contains the positive
function types and N the negated function types of the clause,
represents an uninhabited function type. To do this, we
show that for each clause (P,N) there exists a
(@rt[(→ t_1 t_2)]) @${∈ N} such that

@(centered
  (parens
   (hc-append
    @rt[t_1]
    ($ " <: ")
    (big-vee @rt[s_1]
             #:below ($ "(" @rt[(→ s_1 s_2)] ") ∈ P")))))

(i.e. @rt[t_1] is in the domain of the function) and that
for each @${P′ ⊆ P},

@(centered
  (hc-append
   (parens
    (hc-append
     @rt[t_1]
     ($ " <: ")
     (big-vee @rt[s_1]
              #:below ($ "("@rt[(→ s_1 s_2)] ") ∈ P \\ P′"))))
   ($ " or ")
   (parens
    (hc-append
     (big-wedge @rt[s_2]
                #:below ($ "(" @rt[(→ s_1 s_2)] ") ∈ P′"))
     ($ " <: ")
     @rt[t_2]))))

Basically we are verifying that for each possible set of
arrows P′ which @emph{must} handle a value of type @rt[t_1]
(i.e. the left-hand check fails), those arrows must map the
value to @rt[t_2] (the right-hand check), which is a
contradiction since we know this function is @emph{not} of
type @rt[(→ t_1 t_2)] and therefore this function type is
uninhabited.

@(figure
  "fig:empty-arrow"
  "functions for checking if a function BDD is uninhabited"
  (vl-append
   30
   (parameterize ([linebreaks '(#f #f #f #t #t)])
     (render-mf-empty-arrow))
   (parameterize ([linebreaks '(#f #f #t)])
     (render-mf-empty-arrow-aux))))

We implement this algorithm with the function @rt[emptyA]
defined in @figure-ref["fig:empty-arrow"]. It walks each
path in a function BDD accumulating the domain along the way
and collecting the negated function types in the variable
@rt[N]. At the non-trivial leaves of the BDD, it calls
@rt[emptyA-aux] with each function type @rt[(→ t_1 t_2)] @${∈ N}
until it finds a contradiction (i.e. an arrow that satisfies
the above described equation) or runs out of negated
function types.

@rt[emptyA-aux] is the function which explores each set of
arrows P′ ⊆ P checking that one of the two clauses in the
above noted disjunction is true. Note that in the initial
call from @rt[emptyA] we negate the original @rt[t_2]: this
is because although we are interested in @rt[(<: s_2 t_2)],
the equivalent "contrapositive" statement
@rt[(<: (t-not t_2) (t-not s_2))] is more convenient to
accumulatively check as we iterate through the function
types in @rt[P].

In the base case of @rt[emptyA-aux] when P has been exhausted,
the function checks that either the arrows not in P′ could
have handled the value of (the @emph{original}) type
@rt[t_1] (i.e. is @rt[t_1] now empty), otherwise it checks
if the value we mapped the input to must be a subtype of
(the @emph{original}) type @rt[t_2] (i.e. is @rt[t_2] now
empty).

In the case where @rt[P] has not been exhausted, we examine the
first arrow @rt[(→ s_1 s_2)] in @rt[P] and check two cases:
one for when that arrow is not in @rt[P′] (i.e. when it is in
@${P \ P′}) and one for when it is in @rt[P′].

The first clause in the conjunction of the non-empty P case
is for when @rt[(→ s_1 s_2)] is not in @rt[P′]. It first checks
if the set of arrows we're not considering (i.e. P \ P′)
would handle a value of type @rt[t_1] (i.e.
@rt[(subtype t_1 s_1)]), and if not it remembers that
@rt[(→ s_1 s_2)] is not in P′ by subtracting @rt[s_1] from
@rt[t_1] for the recursive call which keeps searching.

The second clause in the conjunction is for when
@rt[(→ s_1 s_2)] is in @rt[P′]. As we noted, instead of
checking @rt[(<: s_2 t_2)] (resembling the original
mathematical description above), it turns out to be more
convenient to check the contrapositive statement
@rt[(<: t_2 (t-not s_2))] (recall that @rt[t_2] was actually
negated originally when @rt[emptyA-aux] was called). First we
check if having @rt[(→ s_1 s_2)] in @rt[P′] means we would
indeed map a value of type @rt[t_1] to a value of type
@rt[t_2] (i.e. the @rt[(<: t_2 (t-not s_2))] check). If so
we are done, otherwise we recur while remembering that
@rt[(→ s_1 s_2)] is in P′ by adding @rt[s_2] to @rt[t_2]
(i.e. "subtracting" negated @rt[s_2] from the negated @rt[t_2]
we are accumulating by using intersection).

@section{Other Key Type-level Functions}

In addition to being able to decide type inhabitation, we
need to be able to semantically calculate types for the
following situations:

@itemlist[
 #:style 'ordered
 @item{projection from a product,}
 @item{a function's domain, and}
 @item{the result of function application.}]

@subsection{Product Projection}

In a language with syntactic types, calculating the type of
the first or second projection of a pair simply involves
matching on the product type and extracting the first or
second field. In a language with semantic types, however, we
could be dealing with a complex pair type which uses
numerous set-theoretic constructors and we can no longer
simply pattern match to determine the types of its
projections. Instead, we must reason semantically about the
fields.

To begin, first note that if a type is a subtype of
@rt[Any-Pair] (i.e. it is indeed a pair), we can focus on
the product portion of the type:

@(centered pict:product-portion-DNF)
   
Projecting the field @rt[i] from @prod-part (where @rt[i] ∈
{1,2}) involves unioning each positive type for field @rt[i]
in the DNF intersected with each possible combination of
negations for that field:

@(centered
  (big-vee
   (parens
    (big-vee
     (parens
      (hc-append
       (parens (big-wedge @rt[τ_i]
                          #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ P")))
       ($ " ∧ ")
       (parens (big-wedge @rt[(Not τ_i)]
                          #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ N′")))))
     #:below ($ "N′ ⊆ N")))
   #:below (hbl-append ($ "(P,N) ∈ τ" (msup "×")))))

@margin-note{This calculation follows the same line of
 reasoning involved with deciding product type inhabitation
 (see @secref["sec:prod-inhabitation"]), i.e. it considers
 each logical clause in the DNF of the type and unions them.}

Actually that equation is sound but a little too coarse: it
only considers the type of field @rt[i] and thus may include
some impossible cases where the @emph["other"] field would
have been @rt[Empty]. In other words, if @math{j} is an
index and @math{j ≠ i} (i.e. it is the index of the other
field), then as we're calculating the projection of
@math["i"], we'll want to "skip" any cases @math{N′} where
the following is true:

@(centered
  (hc-append
   (parens (big-wedge @rt[τ_j]
                      #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ P")))
   ($ "∧")
   (parens (big-wedge @rt[(Not τ_j)]
                      #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ N \\ N′")))
   ($ " <: ")
   @rt[Empty]))

i.e. cases where the other field @math{j} is uninhabited. If
we incorporate that subtlety, our inner loop will end up
containing a conditional statement:

@(centered
  (scale
   (big-vee
    (parens
     (big-vee
      (parens
       (vl-append
        10
        (hc-append
         ($ " if ")
         (hc-append
          (parens (big-wedge @rt[τ_j]
                             #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ P")))
          ($ "∧")
          (parens (big-wedge @rt[(Not τ_j)]
                             #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ N \\ N′")))
          ($ " <: ")
          @rt[Empty]))
        ($ " then " @rt[Empty])
        (hc-append
         ($ " else ")
         (hc-append
          (parens (big-wedge @rt[τ_i]
                             #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ P")))
          ($ "∧")
          (parens (big-wedge @rt[(Not τ_i)]
                             #:below ($ "(" @rt[(Pair τ_1 τ_2)] ") ∈ N′")))))))
      #:below ($ "N′ ⊆ N")))
    #:below (hbl-append ($ "(P,N) ∈ τ" (msup "×"))))
   9/10))


@subsubsection{Implementing Product Projection}

As was suggested by our use of index variables @math{i} and
@math{j} in the previous section's discussion, we implement
product projection as a single function indexed by some
@rt[i] @math["∈ {1,2}"] and use @rt[select] (defined in
@figure-ref["fig:select"]) to return the appropriate type
in non-empty clauses.

@(figure "fig:select"
         "function for selecting a type during product projection"
         (render-mf-select))

Because projection can fail, we have the function
@rt[maybe-project] as the "public interface" to projection.
@rt[maybe-project] performs important preliminary checks
(i.e. is this type actually a product?) before extracting
the product portion of the type and passing it to the
"internal" function @rt[project] where the real work begins.

@(figure "fig:project"
         "functions for projecting from a product type"
         (vl-append
          30
          (render-mf-maybe-project)
          (parameterize ([linebreaks '(#f #f #f #f #t)])
            (render-mf-project))
          (parameterize ([linebreaks '(#f #f #f #t)])
            (render-mf-project-aux))))

@rt[project] walks the BDD, accumulating for each path (i.e.
each clause in the DNF) the positive type information for
each field in variables @rt[s_1] and @rt[s_2] respectively.
Along the way, if either @rt[s_1] or @rt[s_2] are empty we
can ignore that path. Otherwise at non-trivial leaves we
call the helper function @rt[project-aux] which traverses
the possible combinations of negations, calculating and
unioning the type of field @rt[i] for each possibility.

@subsection{Function Domain}

Similar to product projection, deciding the domain of a
function in a language with set-theoretic types cannot be
done using simple pattern matching; we must reason about the
domain of a function type potentially constructed with
intersections and/or unions.

To do this, first note that for an intersection of arrows,
the domain is equivalent to the union of each of the domains
(i.e. the function can accept any value any of the various
arrows can collectively accept):

@(centered
  ($ "domain(" @rt[(Fun σ_1 τ_1)] " ∧ "
     @rt[(Fun σ_2 τ_2)] " ∧...) = " @rt[σ_1] " ∨ " @rt[σ_2] " ∨ ..."))

Second, note that for a union of arrows, the domain is
equivalent to the intersection of each of the domains (i.e.
the function can only accept values that each of the arrows
can accept):

@(centered
  ($ "domain(" @rt[(Fun σ_1 τ_1)] " ∨ "
     @rt[(Fun σ_2 τ_2)] " ∨...) = " @rt[σ_1] " ∧ " @rt[σ_2] " ∧ ..."))

With those two points in mind, we can deduce that the domain
of an @emph{arbitrary} function type

@(centered pict:function-portion-DNF)

is the following intersection of unions:

@(centered
  (big-wedge
   (parens
    (big-vee @rt[t_1]
             #:below ($ @rt[(→ t_1 t_2)] " ∈ P")))
   #:below (hbl-append ($ "(P,N) ∈ τ" (msup "→")))))



@subsubsection{Implementing Function Domain}

We perform those domain calculations with the functions
defined in @figure-ref["fig:domain"].

@figure["fig:domain"
        "domain calculation for function types"
        (vl-append
         30
         (render-mf-maybe-domain)
         (render-mf-domain))]

@rt[maybe-domain] first checks if the type is indeed a
function (i.e. is it a subtype of @rt[Any-Fun-t]), if so it
then calls @rt[domain] with the function portion of the type
(@rt[Arrowb]) to begin traversing the BDD calculating the
intersection of the union of the respective domains.

@subsection{Function Application}

When applying an arbitrary function to a value, we
must be able to determine the type of the result. If the
application is simple, e.g. a function of type
@rt[(Fun Int Str)] applied to an argument of type @rt[Int],
calculating the result is trivial (@rt[Str]). However, when
we are dealing with an arbitrarily complicated function type
which could contain set-theoretic connectives, deciding the
return type is a little more complicated. As we did in the
previous section, let us again reason separately about how
we might apply intersections and unions of function types to
guide our intuition.

In order to apply a union of function types, the argument
type @rt[σ] of course would have to be in the domain of each
function (see the discussion in the previous section). The
result type of the application would then be the union of
the ranges:

@(centered
  ($ "apply-function((" @rt[(Fun σ_1 τ_1)] " ∨ "
     @rt[(Fun σ_2 τ_2)] " ∨...), " @rt[σ] ") = "
     @rt[τ_1] " ∨ " @rt[τ_1] " ∨ ..."))

This corresponds to the logical observation that if we know
P and that @emph{either} P implies Q @emph{or} P implies R,
then we can conclude that either Q or R holds.

When applying an intersection of function types, the result
type is the @emph{combination} (i.e. intersection) of each
applicable arrow's range. This more or less corresponds to
the logical observation that if we know P and that @emph{
 both} P implies Q @emph{and} P implies R, then we can
conclude that Q @emph{and} R hold.

Combining these lines of reasoning we can deduce that when
considering a function type

@(centered pict:function-portion-DNF)

being applied to an argument of type @rt[σ], we first verify
that @rt[σ] is in the domain of @arrow-part (i.e. using
@rt[maybe-domain]) and then calculate the result type of the
application as follows:

@(centered
  (big-vee
   (parens
    (big-vee
     (parens
      (vl-append
       10
       (hc-append
        ($ " if ")
        (hc-append
         ($ @rt[σ] " <: ")
         (big-vee @rt[τ_1]
                  #:below ($ "(" @rt[(Fun τ_1 τ_2)] ") ∈ P \\ P′"))))
       ($ " then " @rt[Empty])
       (hc-append
        ($ " else ")
        (big-wedge @rt[τ_2]
                   #:below ($ "(" @rt[(Fun τ_1 τ_2)] ") ∈ P′")))))
     #:below ($ "P′ ⊆ P")))
   #:below (hbl-append ($ "(P,N) ∈ τ" (msup "→")))))

Basically, we traverse each clause in the DNF of the
function type (i.e. each pair (P,N)) unioning the results.
In each clause (P,N), we consider each possible set of
arrows P′ in P and if that set would necessarily have to
handle a value of type @rt[σ]. For those sets P′ that would
necessarily handle the input, we intersect their arrow's
result types (otherwise we ignore it by returning @rt[Empty]
for that clause). This reasoning resembles that which was
required to decide function type inhabitation (see
@secref["sec:arrow-inhabitation"]), i.e. both are
considering which combinations of arrows necessarily need to
be considered to perform the relevant calculation.

@subsubsection{Implementing Function Application}

@(figure
  "fig:funapp"
  "function application result type calculations"
  (vl-append
   30
   (render-mf-maybe-funapp)
   (render-mf-funapp)))

@Figure-ref["fig:funapp"] describes the functions which
calculate the result type for function application.
@rt[maybe-funapp] first ensures that the alleged function
type is indeed a function with the appropriate domain before
calling @rt[funapp] to calculate the result type of the
application. @rt[funapp] then traverses the BDD combining
recursive results via union. As it traverses down a BDD
node's left edge (i.e. when a function type @emph{is} a
member of a set P) it makes two recursive calls: one for
when that arrow @emph{is} in P′ (where we intersect the
arrow's range @rt[s_2] with the result type accumulator
@rt[t]) and one for when it @emph{is not} in P′ (where we
subtract @rt[s_1] from the argument type parameter @rt[t_a]
to track if the arrows in P \ P′ can handle the argument
type). At non-trivial leaves where @rt[t_a] is not empty
(i.e. when we're considering a set of arrows P′ which
necessarily @emph{would} need to handle the argument) we
return the accumulated range type (@rt[t]) for that set of
arrows. Note that we can "short-circuit" the calculation
when either of the accumulators (@rt[t] and @rt[t_a])
are empty, which is important to keeping the complexity
of this calculation reasonable.

@section{Strategies for Testing}

For testing an implementation of the data structures and
algorithms described in this tutorial there are some
convenient properties we can leverage:

@itemlist[
 @item{(1) any type generated by the grammar of types is a
  valid type;}
 @item{(2) since these types logically correspond to sets,
  we can create tests based on well-known set properties and
  ensure our types behave equivalently; and}
 @item{(3) we have "naive", inefficient mathematical
  descriptions of many of the algorithms in addition to more
  efficient algorithms which purport to perform the same
  calculation.}]

@(define redex-check-url "http://docs.racket-lang.org/redex/The_Redex_Reference.html#%28form._%28%28lib._redex%2Freduction-semantics..rkt%29._redex-check%29%29")

With these properties in mind, in addition to writing simple
"unit tests" that we write entirely by hand we can use a
tool such as @hyperlink[redex-check-url]{@tt{redex-check}}
(inspired by QuickCheck@~cite[bib:claessen-hughes-2000]) to
generate random types and verify our implementation respects
well-known set properties. Additionally, we can write two
implementations of algorithms which have both a naive and
efficient description and feed them random input while
checking that their outputs are always equivalent.

The model we used to generate the pseudo code in this
tutorial uses these testing strategies. This approach helped
us discover several subtle bugs in our implementation at
various points that simpler hand-written unit tests had not
yet exposed.

@(generate-bibliography)

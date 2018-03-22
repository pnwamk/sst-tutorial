#lang racket/base
(require scriblib/autobib)
(provide (all-defined-out))

(define-cite ~cite citet generate-bibliography)

(define bib:frisch-et-al-2008
  (make-bib
   #:title (string-append
            "Semantic subtyping: Dealing set-theoretically "
            "with function, union, intersection, and negation types.")
   #:author (authors "Alain Frisch"
		     "Giuseppe Castagna"
                     "Véronique Benzaken")
   #:location (journal-location "Journal of the ACM"
				#:volume "55"
				#:number "19")
   #:date "2008"))

(define bib:klein-et-al-2012
  (make-bib
   #:title "Run Your Research: On the Effectiveness of Lightweight Mechanization"
   #:author (authors "Casey Klein"
                     "John Clements"
                     "Christos Dimoulas"
                     "Carl Eastlund"
                     "Matthias Felleisen"
                     "Matthew Flatt"
                     "Jay A. McCarthy"
                     "Jon Rafkind"
                     "Sam Tobin-Hochstadt"
                     "Robert Bruce Findler")
   #:location (proceedings-location
               "The 39th Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL)") 
   #:date "2012"))

(define bib:castagna-2013
  (make-bib 
   #:title (string-append
            "Covariance and Contravariance: a fresh look at an "
            "old issue (a primer in advanced type systems for "
            "learning functional programmers)")
   #:author (authors "Giuseppe Castagna")
   #:date "2013"
   #:note "Unpublished manuscript, periodically updated."))

(define bib:claessen-hughes-2000
  (make-bib
   #:title "QuickCheck: a lightweight tool for random testing of Haskell programs"
   #:author (authors "Koen Claessen"
                     "John Hughes")
   #:location (proceedings-location
               "The 5th ACM SIGPLAN International Conference on Functional Programming (ICFP)") 
   #:date "2000"))



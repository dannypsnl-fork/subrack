#lang info
(define collection "subrack")
(define deps '("base"))
(define build-deps
  '("nanopass"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"))
(define scribblings '(("scribblings/subrack.scrbl" ())))
(define pkg-desc "Subset of Racket")
(define version "0.0")
(define pkg-authors '(dannypsnl))

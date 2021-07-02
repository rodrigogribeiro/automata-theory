#lang racket

(require "core.rkt"
         "../dfa/image-builder.rkt"
         "../fa.rkt"
         "image-builder.rkt"
         "subset-construction.rkt")

;; test case

(define M
  (nfa (s0) (s3)
       (s0 : 0 -> (s0 s1))
       (s0 : 1 -> (s0))
       (s1 : 0 -> (s2))
       (s2 : 0 -> (s3))))


(dfa->pict (subset-construction M))
(nfa->pict M)

#lang racket

(require "core.rkt"
         "../dfa/image-builder.rkt"
         "image-builder.rkt"
         "subset-construction.rkt")

;; test case

(define M
  (nfa (s0) (s2)
       (s0 : 0 -> (s0 s1))
       (s0 : 1 -> (s0))
       (s1 : 0 -> (s2))))


(dfa->pict (subset-construction M))
; (nfa->pict M)
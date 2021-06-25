#lang racket

(require "core.rkt"
         "image-builder.rkt")

;; test case

(define M
  (nfa (s0 s1) (s2)
       (s0 : 0 -> (s0 s1))
       (s0 : 1 -> (s0))
       (s1 : 0 -> (s2))))

(nfa->pict M)

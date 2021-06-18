#lang racket


(require "core.rkt"
         "image-builder.rkt"
         "table-minimization.rkt")


;; simple example

(define M
  (dfa s1 [s3] (s1 : 0 -> s2)
               (s2 : 1 -> s2)
               (s2 : 0 -> s3)
               (s3 : 0 -> s3)
               (s3 : 1 -> s2)))



(define even01
  (dfa pp
       (pp pi)
       (pp : 0 -> ip)
       (pp : 1 -> pi)
       (ip : 0 -> pp)
       (ip : 1 -> ii)
       (pi : 0 -> ii)
       (pi : 1 -> pp)
       (ii : 0 -> pi)
       (ii : 1 -> ip)))

(define mod6
  (dfa st0
       (st0)
       (st0 : 0 -> st0)
       (st0 : 1 -> st1)
       (st1 : 0 -> st2)
       (st1 : 1 -> st3)
       (st2 : 0 -> st4)
       (st2 : 1 -> st5)
       (st3 : 0 -> st0)
       (st3 : 1 -> st1)
       (st4 : 0 -> st2)
       (st4 : 1 -> st3)
       (st5 : 0 -> st4)
       (st5 : 1 -> st5)))


(dfa->pict (minimize mod6))

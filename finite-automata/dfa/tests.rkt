#lang racket


(require "core.rkt"
         "image-builder.rkt")


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

(dfa->pict even01)

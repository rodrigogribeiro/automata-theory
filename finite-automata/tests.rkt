#lang racket


(require "dfa-core.rkt")


;; simple example

(define M
  (dfa s1 [s3] (s1 : 0 -> s2)
               (s2 : 1 -> s2)
               (s2 : 0 -> s3)
               (s3 : 0 -> s3)
               (s3 : 1 -> s2)))

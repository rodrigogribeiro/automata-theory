#lang racket


(provide set-disjoint)


; extra functionalities for set functions

(define (set-disjoint s1 s2)
  (set-empty? (set-intersect s1 s2)))

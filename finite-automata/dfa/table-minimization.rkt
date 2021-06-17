#lang racket

;; implementation of the DFA minimization
;; algorithm using a fixpoint computation

(provide (all-defined-out))

(require "../fa.rkt"
         "core.rkt")

(define (build-table m)
  '())

(define (init-partition m)
  (define final (dfa-final m))
  (define states (dfa-states m))
  (define (memfinal v)
    (not (member v final)))
  (list (filter memfinal states) final))

;; generating all pairs that should be tested

(define (all-pairs xs)
  (match xs
    ['() '()]
    [(cons y ys)
     (append (map (lambda (z) (cons y z)) ys)
             (all-pairs ys))]))

;; checking if two states are equivalent

;; TODO fix me

(define (equiv? part m s1 s2)
  (define sig (dfa-sigma m))
  (define to-test
    (append-map (lambda (s)
                  (list (cons s1 s)
                        (cons s2 s)))
                sig))
  (define res
    (map (lambda (p)
           (dict-ref (dfa-delta m) p))
         to-test))
  (ormap
   (lambda (s)
     (andmap
      (lambda (e)
        (member e s))
     res))
   part))

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

(equiv? (init-partition even01) even01 'pp 'pi)

(define (next-partition current)
  '())

;; fixpoint computation operator

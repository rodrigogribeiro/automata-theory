#lang racket

(require "core.rkt"
         "../dfa/core.rkt"
         "../dfa/image-builder.rkt"
         "../../utils/set-extras.rkt")


;; definition of the subset construction


(struct delta-triple
  (origin symbol target)
  #:transparent)


(define (subset-construction nfa)
  (define states (power-set (nfa-states nfa)))
  (define sig (nfa-sigma nfa))
  (define delta (nfa-delta nfa))
  (define nfin (nfa-final nfa))
  (define finals
    (filter (lambda (S)
              (not (set-disjoint S nfin)))
            states))
  (define (step s c)
    (dict-ref delta (cons s c) '()))
  (define (deltap X a)
    (big-union
     (append-map (lambda (s) (step s a)) X)))
  (define trans
    (append-map (lambda (S)
                  (map (lambda (a)
                         (delta-triple S a
                            (deltap S a))) sig))
                states))
  (define (mk-trans t)
    (match t
      [(delta-triple ors sym tar)
       (cons (cons ors sym) tar)]))
  (mk-dfa states
          sig
          (map mk-trans trans)
          (nfa-start nfa)
          finals))


(define M
  (nfa (s0 s1) (s2)
       (s0 : 0 -> (s0 s1))
       (s0 : 1 -> (s0))
       (s1 : 0 -> (s2))))

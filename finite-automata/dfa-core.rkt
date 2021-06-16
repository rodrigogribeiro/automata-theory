#lang racket

(require syntax/parse
         "fa.rkt"
         "../utils/dot.rkt"
         racket/system
         pict
         (for-syntax syntax/parse
                     racket))

(provide dfa
         dfa?
         dfa-states
         dfa-sigma
         dfa-delta
         dfa-start
         dfa-accept
         dfa-delta-star
         dfa-configurations
         dfa->dot
         dfa->pict)

;; testing if something is a DFA

(define (dfa? m)
  (eq? 'dfa (fa-type m)))

;; projections 

(define (dfa-states m)
  (fa-states m))

(define (dfa-sigma m)
  (fa-sigma m))

(define (dfa-delta m)
  (fa-delta m))

(define (dfa-start m)
  (fa-start m))

(define (dfa-final m)
  (fa-final m))

;; adding missing transitions to a default error state

(define (all-pairs states sig)
  (define (pairs s)
    (map (lambda (c) (cons s c)) sig))
  (append-map pairs states))

(define (new-pairs m)
  (define dl (map car (dfa-delta m)))
  (define allp (all-pairs (dfa-states m)
                          (dfa-sigma m)))
  (filter (lambda (p) (not (member p dl))) allp))

(define (complete? m)
  (andmap (lambda (p) (member p (dfa-delta m)))
          (all-pairs (dfa-states m)
                     (dfa-sigma m))))

(define (add-error m)
  (define err (string->symbol "E"))
  (define err-edges (map (lambda (s) (cons (cons err s) err))
                         (fa-sigma m)))
  (define new (append (map (lambda (p) (cons p err))
                           (new-pairs m))
                      err-edges))
  (match m
    [(fa ty states sigma delta in final)
     (fa ty (remove-duplicates (cons err states))
            sigma
            (append new delta)
            in
            final)]))

(define (complete m)
  (if (complete? m)
      m
      (add-error m)))

;; macro for creating a deterministic finite machine

(define-syntax (dfa stx)
  (syntax-parse stx #:datum-literals (: ->)
    [(_ start:id (end:id ...) [state:id : sym:expr -> next:id] ...)
    #:fail-when (check-duplicates (syntax->datum #'(list (cons 'state 'sym) ...)))
                "This finite machine is non-deterministic."
    #'(complete (fa 'dfa 
                    (remove-duplicates (append (syntax->datum #'(state ...))
                                               (syntax->datum #'(next ...))
                                               (syntax->datum #'(start))))
                    (remove-duplicates (syntax->datum #'(sym ...)))
                    (list (cons (cons 'state 'sym) 'next) ...)
                    'start
                    (list 'end ...)))]))

;; creating a dot program for the dfa

(define (dfa->dot m)
  (match m
    [(fa ty states sigma delta in final)
     (define (build-shape s)
       (if (member s final) "doublecircle" "circle"))
     (create-dot 'digraph
                 (append (list (def-attr 'top-level 'rankdir "LR")
                               (def-attr 'top-level 'size "8.5")
                               (def-node 'qi (list (def-attr 'node 'shape "point"))))
                         (map (lambda (e) (def-node
                                            e
                                            (list (def-attr
                                                    'node
                                                    'shape
                                                    (build-shape e)))))
                              states)
                         (map (lambda (e) (def-edge
                                            'directed
                                            (car (car e))
                                            (cdr e)
                                            (list (def-attr 'edge 'label (cdr (car e))))))
                              delta)
                         (list (def-edge 'directed 'qi in '()))))]))

;; creating a picture for a DFA

(define (dfa->pict m)
  (define code (dot-code (dfa->dot m)))
  (define dot-path (find-executable-path "/usr/local/bin/dot"))
  (define dot-file (make-temporary-file "~a.dv"))
  (define fig-file (make-temporary-file ".~a.png"))
  (begin
    (display-to-file code dot-file #:exists 'truncate)
    (apply system* dot-path (list "-Tpng" dot-file "-o" fig-file))
    (bitmap fig-file)))

;; extended transition function

(define (step m e a)
  (dict-ref (dfa-delta m) (cons e a)))

(define (dfa-delta-star m e s)
  (if (null? s)
      e
      (dfa-delta-star m (step m e (car s)) (cdr s))))

;; definition of acceptance

(define (dfa-accept m s)
  (if (member (dfa-delta-star m (dfa-start m) s)
              (dfa-final m))
      #t
      #f))

;; computing the trace of configurations

(define (dfa-configurations m s)
  (define (config e inp)
    (if (null? inp)
        (list (cons e 'λ))
        (list (cons e inp)
              (config (step m e (car inp))
                      (cdr inp)))))
  (config (dfa-start m) s))

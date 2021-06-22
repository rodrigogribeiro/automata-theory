#lang racket

(require "fa.rkt"
         "../utils/dot.rkt"
         racket/system
         pict)

(provide mk-nodes
         mk-edges
         mark-initial)


;; generic functions for building images
;; from finite automata


(define (mk-nodes states final)
  (define (build-shape s)
    (if (member s final) "doublecircle" "circle"))
  (map (lambda (e)
         (def-node e
           (list (def-attr
                   'node
                   'shape
                   (build-shape e)))))
       states))


(define (mk-edges delta)
  (map (lambda (e) (def-edge
                     'directed
                     (car (car e))
                     (cdr e)
                     (list (def-attr 'edge 'label (cdr (car e))))))
       delta))

(define (mark-initial in)
  (list (def-edge 'directed 'qi in '())))

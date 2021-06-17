#lang racket

(require "../fa.rkt"
         "../../utils/dot.rkt"
         "core.rkt"
         racket/system
         pict)

(provide dfa->dot
         dfa->pict)

;; creating a dot program for the dfa

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

(define dfa-configs
  (list (def-attr 'top-level 'rankdir "LR")
        (def-attr 'top-level 'size "8.5")
        (def-node 'qi
          (list (def-attr 'node 'shape "point")))))


(define (mk-edges delta)
  (map (lambda (e) (def-edge
                     'directed
                     (car (car e))
                     (cdr e)
                     (list (def-attr 'edge 'label (cdr (car e))))))
       delta))

(define (mark-initial in)
  (list (def-edge 'directed 'qi in '())))

(define (dfa->dot m)
  (match m
    [(fa ty states sigma delta in final)
     (create-dot 'digraph
                 (append dfa-configs
                         (mk-nodes states final)
                         (mk-edges delta)
                         (mark-initial in)))]))

;; creating a picture for a DFA

(define (dfa->pict m)
  (define code (dot-code (dfa->dot m)))
  (define dot-path (find-executable-path "/usr/local/bin/dot"))
  (define dot-file (make-temporary-file "~a.dv"))
  (define fig-file (make-temporary-file "~a.png"))
  (begin
    (display-to-file code dot-file #:exists 'truncate)
    (apply system* dot-path (list "-Tpng" dot-file "-o" fig-file))
    (bitmap fig-file)))

#lang racket


;; type definition for finite automata

(provide (all-defined-out))

(struct fa
  (type      ;; kind of finite state machine: dfa, nfa or nfaλ
   states    ;; state set
   sigma     ;; alphabet
   delta     ;; transition function
   start     ;; initial states
   final)    ;; final states
  #:transparent)

(define (ren-state sub s)
  (match sub
    ['() s]
    [(cons x xs)
     (cond
       [(eq? (car x) s) (cdr x)]
       [(set=? (car x) s) (cdr x)]
       [else (ren-state xs s)])]))


(define (ren-states sub s)
  (map (lambda (x) (ren-state sub x)) s))


(define (renaming f)
  (define sts (fa-states f))
  (define (fresh n)
    (string->symbol (format "S~a" n)))
  (define (gen-sub xs ac)
    (match xs
      ['() '()]
      [(cons y ys) (cons (cons y (fresh ac))
                         (gen-sub ys (add1 ac)))]))
  (define sub (gen-sub sts 0))
  (define (ren-trans d)
    (match d
      [(cons (cons e simb) e1)
       (cons (cons (ren-state sub e) simb)
             (ren-state sub e1))]))
  (define (ren-delta ds)
    (map ren-trans ds))
  (fa (fa-type f)
      (ren-states sub (fa-states f))
      (fa-sigma f)
      (ren-delta (fa-delta f))
      (ren-state sub (fa-start f))
      (ren-states sub (fa-final f))))

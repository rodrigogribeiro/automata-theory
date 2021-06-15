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
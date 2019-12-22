#lang racket

(require match-string
         "../lib.rkt")

(define input
  (problem-input 22))

(define (parse-technique technique)
  (match technique
    ["deal into new stack" deal-into-new-stack]
    [(string-append "cut " s) (∂ cut-N-cards (string->number s))]
    [(string-append "deal with increment " s) (∂ deal-with-increment-N (string->number s))]))

(define (deal-into-new-stack cards)
  (reverse cards))

(define (cut-N-cards n cards)
  (if (negative? n)
      (cut-N-cards (+ (length cards) n) cards)
      (append (drop cards n) (take cards n))))

(define (deal-with-increment-N n cards)
  (let* ([len (length cards)]
         [vec (make-vector len)])
    (for ([index (range 0 len)]
          [card cards])
      (vector-set! vec (modulo (* index n) len) card))
    (vector->list vec)))

(define part1
  (let loop ([cards (range 0 10007)]
             [shuffle input])
    (if (empty? shuffle)
        (index-of cards 2019)
        (loop ((parse-technique (first shuffle)) cards) (rest shuffle)))))

;; egcd : number -> number -> (list number number number)
;; Extended Euclidean algorithm for computing GCD
;; Given integers a and b, return gcd(a, b), x, and y, where
;; ax + by = gcd(a, b).
(define (egcd a b)
  (if (zero? a)
      (list b 0 1)
      (match-let ([(list g x y) (egcd (remainder b a) a)])
        (list g (- y (* x (quotient b a))) x))))

;; mmi : number -> number -> number
;; Modular multiplicative inverse
;; Given an integer n and a modulus m, return x such that
;; nx ≡ 1 (mod m), i.e. nx + my = 1 for some x, y.
;; We therefore require that n and m are coprime.
(define (mmi n m)
  (match-let ([(list g x y) (egcd n m)])
    x))

;; The following functions are inverse to those from part 1
;; with respect to the card indices.
;; That is, if some shuffling technique s takes a card at index i
;; and moves it to index s(i) = j, then s^-1(j) = i.
;; Therefore, if after applying a series of shuffling techniques,
;; we want to know what card is at position p,
;; we apply the inverse functions in reverse order to find its original index.

(define (inverse-DINS len i)
  (sub1 (- len i)))

(define (inverse-CNC len n i)
  (if (positive? n)
      (modulo (+ n i) len)
      (modulo (+ n i len) len)))

(define (inverse-DWIN len n i)
  (modulo (* (mmi n len) i) len))

(define (inverse-parse len technique)
  (match technique
    ["deal into new stack" (∂ inverse-DINS len)]
    [(string-append "cut " s) (∂ inverse-CNC len (string->number s))]
    [(string-append "deal with increment " s) (∂ inverse-DWIN len (string->number s))]))

(define (inverse-shuffle len)
  (apply compose (map (∂ inverse-parse len) input)))

(define (part2)
  (let loop ([count 101741582076661]
             [index 2020])
    (if (zero? count)
        index
        (loop (sub1 count) ((inverse-shuffle 119315717514047) index)))))

(show-solution part1 #f)
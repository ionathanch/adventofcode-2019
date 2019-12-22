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

;; mexp : number -> number -> number
;; Modular exponentiation
;; Given a base b, an exponent e, and a modulus m,
;; compute b^e mod m.
;; This uses the identity ab mod b = (a mod m)(b mod m) mod m
(define (mexp b e m)
  (let loop ([e e] [result 1])
    (if (= e 0) result
        (loop (sub1 e) (modulo (* b result) m)))))

;; i -> -i + (len - 1)
(define (inverse-DINS len mo)
  (match-let ([(list m o) mo])
    (list (modulo (* m -1) len)
          (modulo (+ (* o -1) (sub1 len)) len))))

;; i -> i + n
(define (inverse-CNC len n mo)
  (match-let ([(list m o) mo])
    (list m (modulo (+ o n) len))))

;; i -> i * n^-1
(define (inverse-DWIN len n mo)
  (match-let ([(list m o) mo]
              [ninv (mmi n len)])
    (list (modulo (* ninv m) len)
          (modulo (* ninv o) len))))

(define (inverse-parse len technique mo)
  (match technique
    ["deal into new stack" (inverse-DINS len mo)]
    [(string-append "cut " s) (inverse-CNC len (string->number s) mo)]
    [(string-append "deal with increment " s) (inverse-DWIN len (string->number s) mo)]))

;; This gives m = 90109821400559, o = 119199174489885 for len = 119315717514047
(define (inverse-shuffle len)
  (foldr (∂ inverse-parse len) '(1 0) input))

;; mexp was taking too long, so I asked WolframAlpha for mn:
;; 90109821400559^101741582076661 % 119315717514047 = 20096240743059
(define (inverse-shuffle-N-times len n)
  (match-let* ([(list m o) (inverse-shuffle len)]
               [mn 20096240743059 #;(mexp m n len)]
               [on (modulo (* o (sub1 mn) (mmi (sub1 m) len)) len)])
    (list mn on)))

;; Given a modulus len, a multiple-offset pair mo, and a number i,
;; compute (m*i + o) % len
(define (apply-mo len mo i)
  (match-let ([(list m o) mo])
    (modulo (+ (* m i) o) len)))

(define part2
  (let* ([len 119315717514047]
         [mo (inverse-shuffle-N-times len 101741582076661)])
    (apply-mo len mo 2020)))

(show-solution part1 part2)
#lang racket

(require match-string
         math/number-theory
         "../lib.rkt")

(define input
  (problem-input 22))

;; A shuffle operation (technique) is
;; an affine transformation on a card's index.
;; The identity transformation is I = (1, 0).
;; Applying the transformation (m, o) to i yields (m*i + o).

(struct affine (multiple offset) #:transparent)

(define I (affine 1 0))

(define (apply-affine len mo i)
  (match-let ([(affine m o) mo])
    (% (+ (* m i) o) len)))

;; We can compose transformations and only keep track
;; of the multiple and offset factors (modulo some len);
;; <> composes two transformations as if the second were
;; applied first and the first applied last, so
;; (m1 , o1) <> (m2, o2) = (m1 * m2, m1 * o2 + o1).
(define (<> len a1 a2)
  (match-let ([(affine m1 o1) a1]
              [(affine m2 o2) a2])
    (affine (% (* m1 m2) len)
            (% (+ (* m1 o2) o1) len))))

;; Applying the transformation (m, o) n times is the same as
;; applying the transformation (m^n + o*(m^n - 1)/(m - 1)),
;; modulo some len.
(define (affine-expt mo n len)
  (match-let* ([(affine m o) mo]
               [m^n (modular-expt m n len)]
               [o* (% (* o (sub1 m^n) (modular-inverse (sub1 m) len)) len)])
    (affine m^n o*)))

;; Inverting the transformation (m, o) is the same as
;; the transformation (m^-1, -o*m^-1)
(define (affine-invert mo len)
  (match-let* ([(affine m o) mo]
               [m^-1 (modular-inverse m len)])
    (affine m^-1 (* -1 o m^-1))))

;; All shuffling transformation techniques are modulo the number of cards.
;; deal into new stack: reversing the order of the cards,
;;   corresponding to the transformation i → -1*i + (length - 1)
;; cut N cards: rotating the cards to the left by n,
;;   corresponding to the transformation i → i - n
;; deal with increment N: placing a card every n steps,
;;   corresponding to the transformation i → n*i

(define (DINS len)
  (affine -1 (sub1 len)))

(define (CNC len n)
  (affine 1 (* n -1)))

(define (DWIN len n)
  (affine n 0))

;; Shuffling combines all transformations in order.
;; Inverse shuffling is simply the inverse transformation.
;; We begin with the identity transformation, I = (1, 0).

(define (parse len T)
  (match T
    ["deal into new stack" (DINS len)]
    [(string-append "cut " s) (CNC len (string->number s))]
    [(string-append "deal with increment " s) (DWIN len (string->number s))]))

(define (shuffle len)
  (foldl (λ (T mo) (<> len (parse len T) mo)) I input))

(define (inverse-shuffle len)
  (affine-invert (shuffle len) len))

(define part1
  (let ([len 10007])
    (apply-affine len (shuffle len) 2019)))

(define part2
  (let* ([len 119315717514047]
         [mo (inverse-shuffle len)]
         [mo^n (affine-expt mo 101741582076661 len)])
    (apply-affine len mo^n 2020)))

(show-solution part1 part2)
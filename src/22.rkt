#lang racket

(require match-string
         math/number-theory
         "../lib.rkt")

(define input
  (problem-input 22))

;; A shuffle operation (technique) is
;; an affine transformation on a card's index.
;; Applying the transformation (m, o) to i yields (m*i + o).
;; We can compose transformations and only keep track
;; of the multiple and offset factors (modulo some len).
;; The identity transformation is I = (1, 0).

(struct affine (multiple offset) #:transparent)

(define (apply-affine len mo i)
  (match-let ([(affine m o) mo])
    (% (+ (* m i) o) len)))

(define I (affine 1 0))

;; Applying the transformation (m, o) n times is the same as
;; applying the transformation (m^n + o*(m^n - 1)/(m - 1)),
;; modulo some len.
(define (affine-expt mo n len)
  (match-let* ([(affine m o) mo]
               [m^n (modular-expt m n len)]
               [o* (% (* o (sub1 m^n) (modular-inverse (sub1 m) len)) len)])
    (affine m^n o*)))

;; All shuffling transformation techniques are modulo the number of cards.
;; deal into new stack: reversing the order of the cards,
;;   corresponding to the transformation i → -1*i + (length - 1)
;; cut N cards: rotating the cards to the left by n,
;;   corresponding to the transformation i → i - n
;; deal with increment N: placing a card every n steps,
;;   corresponding to the transformation i → n*i

(define (DINS len mo)
  (match-let ([(affine m o) mo])
    (affine (% (* m -1) len)
            (% (- (sub1 len) o) len))))

(define (CNC len n mo)
  (match-let ([(affine m o) mo])
    (affine m (% (- o n) len))))

(define (DWIN len n mo)
  (match-let ([(affine m o) mo])
    (affine (% (* m n) len)
            (% (* o n) len))))

;; The corresponding inverse transformations are:
;;   DINS: -1*i + (length - 1) ← i
;;   CNC: i + n ← i
;;   DWIN: n^-1*i ← i
;; where ·^-1 is the modular multiplicative inverse

(define (inverse-DINS len mo)
  (DINS len mo))

(define (inverse-CNC len n mo)
  (CNC len (* n -1) mo))

(define (inverse-DWIN len n mo)
  (DWIN len (modular-inverse n len) mo))

;; Shuffling combines all transformations in order.
;; Inverse shuffling combines all inverse transformations in reverse order.
;; We begin with the identity transformation, I = (1, 0).

(define (parse len T mo)
  (match T
    ["deal into new stack" (DINS len mo)]
    [(string-append "cut " s) (CNC len (string->number s) mo)]
    [(string-append "deal with increment " s) (DWIN len (string->number s) mo)]))

(define (inverse-parse len T mo)
  (match T
    ["deal into new stack" (inverse-DINS len mo)]
    [(string-append "cut " s) (inverse-CNC len (string->number s) mo)]
    [(string-append "deal with increment " s) (inverse-DWIN len (string->number s) mo)]))

(define (shuffle len)
  (foldl (∂ parse len) I input))

(define (inverse-shuffle len)
  (foldr (∂ inverse-parse len) I input))

(define part1
  (let ([len 10007])
    (apply-affine len (shuffle len) 2019)))

(define part2
  (let* ([len 119315717514047]
         [mo (inverse-shuffle len)]
         [mo^n (affine-expt mo 101741582076661 len)])
    (apply-affine len mo^n 2020)))

(show-solution part1 part2)
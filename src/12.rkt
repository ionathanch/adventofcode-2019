#lang racket

(require racket/set
         "../lib.rkt")

(define moons-pos
  '((-9 -1  -1)
    ( 2  9   5)
    (10 18 -12)
    (-6 15  -7)))

(define moons-vel
  '((0 0 0)
    (0 0 0)
    (0 0 0)
    (0 0 0)))

(define example
  '((-1   0  2)
    ( 2 -10 -7)
    ( 4  -8  8)
    ( 3   5 -1)))

(define positions (mutable-set))

(define (less-than->number n1 n2)
  (cond
    [(< n1 n2)  1]
    [(= n1 n2)  0]
    [(> n1 n2) -1]))

;; Calculate the velocity on the first moon
;; caused by the second moon
(define (velocity moon-on moon-by)
  (list (less-than->number (first  moon-on) (first  moon-by))
        (less-than->number (second moon-on) (second moon-by))
        (less-than->number (third  moon-on) (third  moon-by))))

(define (<+> v1 v2)
  (zip + v1 v2))

;; Calculate velocities for all moons
(define (velocities moons vels)
  (zip <+> vels
       (map (λ (moon)
              (foldl (λ (other v)
                       (<+> v (velocity moon other)))
                     '(0 0 0) moons))
            moons)))

(define (energy moons vels)
  (sum (zip (λ (moon vel)
              (* (sum (map abs moon))
                 (sum (map abs vel))))
            moons vels)))

(define (step moons vels)
  (let* ([vels (velocities moons vels)]
         [moons (zip <+> moons vels)])
    (values moons vels)))

(define (step-n n moons vels)
  (if (zero? n)
      (values moons vels)
      (let-values ([(moons vels) (step moons vels)])
        (step-n (sub1 n) moons vels))))

(define part1
  (let-values ([(moons vels)
                (step-n 1000 moons-pos moons-vel)])
    (energy moons vels)))

(define (part2-nogood)
  (let loop ([steps 0]
             [moons moons-pos]
             [vels  moons-vel])
    (if (set-member? positions moons)
        steps
        (let*-values ([(moons* vels*)
                       (step moons vels)])
          (set-add! positions moons)
          (loop (add1 steps) moons* vels*)))))

(show-solution part1 #f)
#lang racket

(require racket/set
         "../lib.rkt")

(define moons-pos
  '((-9 -1  -1)   ;; moon 1
    ( 2  9   5)   ;; moon 2
    (10 18 -12)   ;; moon 3
    (-6 15  -7))) ;; moon 4
;;    x  y   z

(define moons-vel
  '((0  0  0)   ;; moon 1
    (0  0  0)   ;; moon 2
    (0  0  0)   ;; moon 3
    (0  0  0))) ;; moon 4
;;  vx vy vz

(define (less-than->number n1 n2)
  (cond
    [(< n1 n2)  1]
    [(= n1 n2)  0]
    [(> n1 n2) -1]))

;; velocity-1D : (listof pos) -> pos -> vel -> vel
;; Calculate the final velocity on the given moon
;; due to all other moons
(define (velocity-1D moons moon vel)
  (+ vel (sum (map (∂ less-than->number moon) moons))))

;; velocities-1D : (listof pos) -> (listof vel) -> (listof vel)
;; Calculate velocities for all moons
(define (velocities-1D moons vels)
  (map (∂ velocity-1D moons) moons vels))

;; step-1D : (listof pos) -> (listof vel) -> (values (listof pos) (listof vel))
;; Step moon trajectories once
(define (step-1D moons vels)
  (let* ([vels (velocities-1D moons vels)]
         [moons (zip + moons vels)])
    (values moons vels)))

;; step-n-1D : number -> (listof pos) -> (listof vel) -> (values (listof pos) (listof vel))
;; Step moon trajectories n times
(define (step-n-1D n moons vels)
  (if (zero? n)
      (values moons vels)
      (let-values ([(moons vels) (step-1D moons vels)])
        (step-n-1D (sub1 n) moons vels))))

;; loop-steps-1D : (listof pos) -> (listof vel) -> number
;; Calculate steps to loop back to original position
(define (loop-steps-1D moons* vels*)
  (let loop ([steps 1]
             [moons moons*]
             [vels vels*])
    (if (and (> steps 1) (equal? moons moons*))
        steps
        (let*-values ([(moons* vels*)
                       (step-1D moons vels)])
          (loop (add1 steps) moons* vels*)))))

;; energy : (listof (list xpos ypos zpos)) -> (listof (list xvel yvel zvel)) -> number
;; Calculate total energy (in three dimensions)
(define (energy moons vels)
  (sum (map (λ (moon vel)
              (* (sum (map abs moon))
                 (sum (map abs vel))))
            moons vels)))

(define part1
  (let* ([moons (transpose moons-pos)]  ;; moons : (list (listof xpos) (listof ypos) (listof zpos))
         [vels  (transpose moons-vel)]) ;; vels  : (list (listof xvel) (listof yvel) (listof zvel))
    (let*-values ([(moons-x vels-x) (step-n-1D 1000 (first  moons) (first  vels))]
                  [(moons-y vels-y) (step-n-1D 1000 (second moons) (second vels))]
                  [(moons-z vels-z) (step-n-1D 1000 (third  moons) (third  vels))])
      (let ([moonsT (transpose (list moons-x moons-y moons-z))]
            [velsT  (transpose (list vels-x  vels-y  vels-z))])
        (energy moonsT velsT)))))

(define part2
  (let* ([moons (transpose moons-pos)] ;; moons : (list (listof xpos) (listof ypos) (listof zpos))
         [vels  (transpose moons-vel)] ;; vels  : (list (listof xvel) (listof yvel) (listof zvel))
         [steps-x (loop-steps-1D (first  moons) (first  vels))]
         [steps-y (loop-steps-1D (second moons) (second vels))]
         [steps-z (loop-steps-1D (third  moons) (third  vels))])
    (lcm steps-x steps-y steps-z)))

(show-solution part1 part2)
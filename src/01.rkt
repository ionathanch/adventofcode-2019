#lang racket

(require "../lib.rkt")

(define input (map string->number (problem-input 1)))

;; calc-fuel : number -> number
(define (calc-fuel mass)
  (- (floor (/ mass 3)) 2))

(define part1
  (sum (map calc-fuel input)))

;; calc-fuel-rec : number -> number
;; Calculate the fuel given a mass,
;; then add the fuel required for THAT fuel,
;; and so on until the fuel required is 0 or negative,
;; for which the fuel should just be 0.
(define (calc-fuel-rec mass)
  (let ([fuel (calc-fuel mass)])
    (if (<= fuel 0) 0
        (+ fuel (calc-fuel-rec fuel)))))

(define part2
  (sum (map calc-fuel-rec input)))

(show-solution part1 part2)
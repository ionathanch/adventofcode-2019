#lang racket

(require "../lib.rkt")

#|
DAY 1: The Tyranny of the Rocket Equation (excerpt)

PART 1:
...
Fuel required to launch a given module is based on its mass.
Specifically, to find the fuel required for a module, take its mass,
divide by three, round down, and subtract 2.
...
What is the sum of the fuel requirements for all of the modules
on your spacecraft?

PART 2:
...
So, for each module mass, calculate its fuel and add it to the total.
Then, treat the fuel amount you just calculated as the input mass
and repeat the process, continuing until a fuel requirement is zero
or negative.
...
What is the sum of the fuel requirements for all of the modules
on your spacecraft when also taking into account the mass of the
added fuel?
|#

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
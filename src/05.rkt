#lang racket

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 5))))

(define part1
  (let-values ([(_ out) (exec input #:in '(1))])
    (last out)))

(define part2
  (let-values ([(_ out) (exec input #:in '(5))])
    (last out)))

(show-solution part1 part2)
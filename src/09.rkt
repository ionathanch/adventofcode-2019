#lang racket

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 9))))

(define-values (_ part1)
  (exec input #:in '(1)))

(define-values (__ part2)
  (exec input #:in '(2)))

(show-solution (car part1) (car part2))
#lang racket

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 9))))

(define part1
  (car (resume-with-io (exec input) '(1))))

(define part2
  (car (resume-with-io (exec input) '(2))))

(show-solution part1 part2)
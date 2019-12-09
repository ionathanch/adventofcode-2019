#lang racket

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 5))))

(define part1
  (last (resume-with-io (exec input) '(1))))

(define part2
  (last (resume-with-io (exec input) '(5))))

(show-solution part1 part2)
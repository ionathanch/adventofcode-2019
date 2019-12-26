#lang racket/load

(require "../lib.rkt")

(define filename "../input/18.txt")

(define part1
  (load "18-helper.rkt"))

(set! filename "../input/18b1.txt")

(define part2-1
  (load "18-helper.rkt"))

(set! filename "../input/18b2.txt")

(define part2-2
  (load "18-helper.rkt"))

(set! filename "../input/18b3.txt")

(define part2-3
  (load "18-helper.rkt"))

(set! filename "../input/18b4.txt")

(define part2-4
  (load "18-helper.rkt"))

(define part2
  (+ part2-1 part2-2 part2-3 part2-4))

(show-solution part1 part2)
#lang racket

(require "../lib.rkt")

(define input
  (car (problem-input 8)))

(define width 25)
(define height 6)

(define pixel-hash
  (make-hash '((#\0 . #\ ) (#\1 . #\█))))

(define layers
  (let* ([area (* width height)]
         [chars (string->list input)])
    (chunks-of chars area)))

(define part1
  (let* ([zeroes (map (∂ count (∂ eq? #\0)) layers)]
         [min-index (index-of zeroes (apply min zeroes))]
         [min-layer (list-ref layers min-index)]
         [ones (count (∂ eq? #\1) min-layer)]
         [twos (count (∂ eq? #\2) min-layer)])
    (* ones twos)))

(define part2
  (let* ([image (map (∂ findf (∂ neq? #\2)) (transpose layers))])
    (show-msg pixel-hash (chunks-of image width))))

(show-solution part1 #f)
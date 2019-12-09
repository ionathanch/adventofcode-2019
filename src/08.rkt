#lang racket

(require "../lib.rkt")

(define input
  (car (problem-input 8)))

(define width 25)
(define height 6)

(define layers
  (let* ([area (* width height)]
         [chars (string->list input)])
    (chunks-of chars area)))

(define part1
  (let* ([zeroes (map (curry count (curry eq? #\0)) layers)]
         [min-index (index-of zeroes (apply min zeroes))]
         [min-layer (list-ref layers min-index)]
         [ones (count (curry eq? #\1) min-layer)]
         [twos (count (curry eq? #\2) min-layer)])
    (* ones twos)))

(define part2
  (let* ([image (map (curry findf (curry neq? #\2)) (transpose layers))]
         [image* (map (λ (pixel) (if (eq? pixel #\1) #\█ #\ )) image)]
         [msg (map list->string (chunks-of image* width))])
    (for-each displayln msg)))

(show-solution part1 #f)
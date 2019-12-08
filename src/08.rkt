#lang racket

(require "../lib.rkt")

(define input
  (car (problem-input 8)))

(define width 25)
(define height 6)

;; partition-into : (listof any) -> nonzero? -> (listof (listof any))
;; Partitions a list into lists of the given size in order,
;; with the final list possibly being smaller
;; e.g. '(1 2 3 4 5) 2 => '((1 2) (3 4) (5))
(define (partition-into lst size)
  (if (< (length lst) size) lst
      (cons (take lst size)
            (partition-into (drop lst size) size))))

;; pivot : (listof (listof any)) -> (listof (listof any))
;; Turns a list of lists into a list of lists of
;; the first elements of the lists, ..., the nth elements
;; where n is the length of the shortest list.
;; In short, it pivots a list of rows into a list of columns.
;; e.g. '((1 2 3 4)          '((1 5 8)
;;        (5 6 7)         =>   (2 6 9)
;;        (8 9 10 11 12))      (3 7 10))
(define (pivot layers)
  (if (ormap empty? layers) '()
      (let ([pixels (map car layers)]
            [layers (map cdr layers)])
        (cons pixels (pivot layers)))))

(define layers
  (let* ([area (* width height)]
         [chars (string->list input)])
    (partition-into chars area)))

(define part1
  (let* ([zeroes (map (curry count (curry eq? #\0)) layers)]
         [min-index (index-of zeroes (apply min zeroes))]
         [min-layer (list-ref layers min-index)]
         [ones (count (curry eq? #\1) min-layer)]
         [twos (count (curry eq? #\2) min-layer)])
    (* ones twos)))

(define part2
  (let* ([image (map (curry findf (curry neq? #\2)) (pivot layers))]
         [image* (map (λ (pixel) (if (eq? pixel #\1) #\█ #\ )) image)]
         [msg (map list->string (partition-into image* width))])
    (for-each displayln msg)))

(show-solution part1 #f)
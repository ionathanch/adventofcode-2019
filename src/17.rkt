#lang racket

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 17))))

(define scaffolds
  (string-trim (list->string (map integer->char (resume-with-io (exec input) '())))))

(define part1
  (let* ([list-grid (map string->list (string-split scaffolds "\n"))]
         [vector-grid (list->vector (map list->vector list-grid))]
         [width (length (car list-grid))]
         [height (length list-grid)])
    (foldl (λ (xy sum)
             (match-let ([(list x y) xy])
               (if (and (char=? #\# (vector-ref (vector-ref vector-grid (add1 y)) x))
                        (char=? #\# (vector-ref (vector-ref vector-grid (sub1 y)) x))
                        (char=? #\# (vector-ref (vector-ref vector-grid y) (add1 x)))
                        (char=? #\# (vector-ref (vector-ref vector-grid y) (sub1 x))))
                   (+ sum (* x y))
                   sum)))
           0 (cartesian-product (range 1 (sub1 width)) (range 1 (sub1 height))))))

;; I did this manually. sue me
(define A "L,10,R,10,L,10,L,10\n")
(define B "R,10,R,12,L,12\n")
(define C "R,12,L,12,R,6\n")
(define main "A,B,A,B,C,C,B,A,B,C\n")

(define part2
  (let* ([program (vector-set!* input 0 2)]
         [input (append (map char->integer (string->list main))
                        (map char->integer (string->list A))
                        (map char->integer (string->list B))
                        (map char->integer (string->list C))
                        (map char->integer (string->list "n\n")))]
         [output (resume-with-io (exec program) input)]
         [map-list (take output (sub1 (length output)))]
         [map-string (list->string (map integer->char map-list))])
    (display map-string)
    (last output)))

(show-solution part1 part2)
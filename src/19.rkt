#lang racket

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 19))))

(define space-hash (make-hash '((0 . #\.) (1 . #\#))))

(define-values (part1 hash-grid)
  (let* ([coords (cartesian-product (range 0 50) (range 0 50))]
         [readings (append* (map (λ (coord) (resume-with-io (exec input) coord)) coords))]
         [hash-grid (make-hash)])
    (for ([coord coords] [reading readings])
      (match-let ([(list x y) coord])
        (hash-set! hash-grid (cons x y) reading)))
    (values (sum readings) hash-grid)))

;; The bottom bounding diagonal follows the following pattern:
;; - 4, 4, 4, 4, 4, 4, 4, 5,
;;   4, 4, 4, 4, 4, 4, 5 for all subsequent rows
;; The top bounding diagonal follows the following pattern:
;; - 3, 3, 3, 2,
;;   3, 3, 3, 2, 3, 3, 2 for all subsequent rows
;; Where n1, ... indicates we go 1 across, then n1 down, etc.
;; The bottom diagonal has a slope of around 29/7;
;; The top diagonal has a slope of around 19/7.
;; The diagonal of the 100x100 square is given by x+c for some c.
;; We solve for the intersection points of the square diagonal
;; with the top and bottom diagonals by solving the equations
;; - xb - c = -27/9 xb
;; - xt - c = -19/7 xt
;; - xt - xb = 100
;; Where (xb, yb) is the lower left corner and (xt, yt) is the top right:
;;   (xb, yt) .... (xt, yt)
;;     ....          ....
;;   (xb, yb) .... (xt, yb)
;; This gives us
;; - xb = 260, yb = 1077
;; - xt = 360, yt = 977
;; But the ranges [260, 360] and [977, 1077] contain 101 points,
;; when we only need to fit 100 points. We have two options to test:
;; - (360, 977) and (261, 1076), or
;; - (359, 978) and (260, 1077).

(define corner-360x977?  ; #f
  (= 1 (first (resume-with-io (exec input) '(360 977)))))
(define corner-261x1076? ; #t
  (= 1 (first (resume-with-io (exec input) '(261 1076)))))

(define corner-359x978?  ; #t
  (= 1 (first (resume-with-io (exec input) '(359 978)))))
(define corner-260x1077? ; #f
  (= 1 (first (resume-with-io (exec input) '(260 1077)))))

;; Neither of the pairs simultaneously have both points in the beam!
;; We need to shift right and move down.

(define xb ; 261
  (let loop ([xb 261])
    (if (= 1 (first (resume-with-io (exec input) (list xb 1077))))
        xb (loop (add1 xb)))))

(define yt ; 980
  (let loop ([yt 978])
    (if (= 1 (first (resume-with-io (exec input) (list 360 yt))))
        yt (loop (add1 yt)))))

;; x ∈ [261, 360] contains 100 points. y ∈ [980, 1077] doesn't.
;; We can't expand upwards; we can only expand downwards.
;; Then the bottom right corner is (360, 1079),
;; and the top left corner is (261, 980).

(define part2 (+ (* 261 10000) 980))

(show-solution part1 part2)
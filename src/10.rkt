#lang racket

(require "../lib.rkt")

(define input
  (problem-input 10))

(define width (string-length (car input)))
(define height (length input))

(define (out-of-range? x y)
  (or (< x 0)
      (< y 0)
      (>= x width)
      (>= y height)))

(define (coprime? i j)
  (= 1 (gcd i j)))

(define (offsets x y)
  (let ([is (range (negate x) (- width  x))]
        [js (range (negate y) (- height y))])
    (filter ($ coprime?) (cartesian-product is js))))

(define (asteroid? x y)
  (define row (list-ref input y))
  (eq? #\# (string-ref row x)))

(define (asteroid-offset x y i j)
  (let loop ([m 1])
    (let ([x* (+ x (* i m))]
          [y* (+ y (* j m))])
      (cond
        [(out-of-range? x* y*) #f]
        [(asteroid? x* y*) (list x* y*)]
        [else (loop (add1 m))]))))

(define (asteroids x y)
  (filter-map ($ (∂ asteroid-offset x y)) (offsets x y)))

(define-values (part1 location in-view)
  (let* ([cols (range width)]
         [rows (range height)]
         [locations (filter ($ asteroid?) (cartesian-product cols rows))]
         [in-views (map ($ asteroids) locations)]
         [counts (map length in-views)]
         [maximum (apply max counts)]
         [index (index-of counts maximum)])
    (values maximum
            (list-ref locations index)
            (list-ref in-views index))))

(define (offset<? xy1 xy2)
  (let* ([x1 (first  xy1)] [y1 (second xy1)]
         [x2 (first  xy2)] [y2 (second xy2)]
         [θ1 (atan y1 x1)] [θ2 (atan y2 x2)]
         [θ1 (+ θ1 (if (< θ1 (/ pi -2)) (* 2 pi) 0))]
         [θ2 (+ θ2 (if (< θ2 (/ pi -2)) (* 2 pi) 0))])
    (< θ1 θ2)))

(define part2
  (let* ([offsets (map (λ (ast) (map - ast location)) in-view)]
         [offsets (sort offsets offset<?)]
         [200th (map + location (list-ref offsets (sub1 200)))])
    (+ (* (first 200th) 100) (second 200th))))

(show-solution part1 part2)
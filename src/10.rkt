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
    (filter (uncurry coprime?) (cartesian-product is js))))

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
  (filter-map (uncurry (curry asteroid-offset x y)) (offsets x y)))

(define-values (part1 location in-view)
  (let* ([cols (range width)]
         [rows (range height)]
         [locations (filter (uncurry asteroid?) (cartesian-product cols rows))]
         [in-views (map (uncurry asteroids) locations)]
         [counts (map length in-views)]
         [maximum (apply max counts)]
         [index (index-of counts maximum)])
    (values maximum
            (list-ref locations index)
            (list-ref in-views index))))

(define (offset<? xy1 xy2)
  (let ([x1 (first  xy1)]
        [y1 (second xy1)]
        [x2 (first  xy2)]
        [y2 (second xy2)])
    (or ; (0, -) < (+, y) < (0, +) < (-, y)
     (and (zero? x1)  (negative? y1))
     (and (positive? x1)
          (or (negative? x2)
              (and (zero? x2) (positive? y2))))
     (and (zero? x1)  (negative? x2))
     (and (nzero? x1) (nzero? x2)
          (< (atan (/ y1 x1)) (atan (/ y2 x2)))))))

(define part2
  (let* ([offsets (map (λ (ast) (zip - ast location)) in-view)]
         [offsets (sort offsets offset<?)]
         [200th (zip + location (list-ref offsets (sub1 200)))])
    (+ (* (first 200th) 100) (second 200th))))

(show-solution part1 part2)
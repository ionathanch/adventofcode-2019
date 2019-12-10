#lang racket

(require "../lib.rkt")

(define input
  (problem-input 10))

(define width (string-length (car input)))
(define height (length input))

(define (coprime? ij)
  (= 1 (gcd (first  ij)
            (second ij))))

(define (offsets x y)
  (define is
    (range (negate x) (- width x)))
  (define js
    (range (negate y) (- height y)))
  (filter coprime? (cartesian-product is js)))

(define (asteroid? xy)
  (define row (list-ref input (second xy)))
  (eq? #\# (string-ref row (first xy))))

(define (asteroid-offset? x y ij)
  (define i (first  ij))
  (define j (second ij))
  (define max-multiple
    (let ([max-x
           (cond
             [(zero? i) width]
             [(positive? i) (truncate (/ (- (sub1 width) x) i))]
             [else (abs (truncate (/ x i)))])]
          [max-y
           (cond
             [(zero? j) height]
             [(positive? j) (truncate (/ (- (sub1 height) y) j))]
             [else (abs (truncate (/ y j)))])])
      (min max-x max-y)))
  (ormap (λ (m) (asteroid? `(,(+ x (* m i))
                             ,(+ y (* m j)))))
         (range 1 (add1 max-multiple))))

(define (asteroids xy)
  (define x (first xy))
  (define y (second xy))
  (count (curry asteroid-offset? x y) (offsets x y)))

(define-values (part1 location)
  (let* ([cols (range width)]
         [rows (range height)]
         [locations (filter asteroid? (cartesian-product cols rows))]
         [asteroid-counts (map asteroids locations)]
         [maximum (apply max asteroid-counts)]
         [index (index-of asteroid-counts maximum)]
         [location (list-ref locations index)])
    (values maximum location)))

(show-solution part1 #f)
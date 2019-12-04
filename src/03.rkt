#lang racket

(require "../lib.rkt")

;; wire : (listof path)
;; path : (symbol . number)
(define-values (wire1 wire2)
  (let* ([input (problem-input 3)]
         [string->path (λ (str) (cons (string->symbol (substring str 0 1))
                                      (string->number (substring str 1))))]
         [string->wire (λ (str) (map string->path (string-split str ",")))]
         [wire1 (string->wire (first input))]
         [wire2 (string->wire (second input))])
    (values wire1 wire2)))

;; To store the paths of each wire, we will use a hashtable.
;; The keys of the hashtable are positions on the grid,
;;   stored as a pair of numbers (x . y).
;; The values of the hashtable are points on the grid,
;;   which contain four pieces of information:
;;   - first-visited? : Whether the first wire has traversed the point;
;;   - first-steps : How many steps from the origin the first wire has taken;
;;   - second-visited?, second-steps: idem but for the second wire.
(define pos-x car)
(define pos-y cdr)
(define (pos x y)
  (cons x y))

(struct point
  (first-visited?
   first-steps
   second-visited?
   second-steps))

(define origin (pos 0 0))
(define default-point (point #f 0 #f 0))

;; step-path : boolean -> path -> hashtable -> (pos . number) -> (pos . number)
;;   first-wire? : Indicates whether we're stepping for the first or second wire.
;;   path : A pair (s: symbol . n: number), where s indicates the direction we step
;;     (R, L, U, D for right (+x), left (-x), up (+y), down (-y))
;;     and n indicates the number of units we step in that direction
;;   pos-count : A pair (pos . number) that indicates the current location and the steps taken
;;   hashtable : A map from pos to point
;; Returns the new position after stepping
(define (step-path first-wire? path ht pos-count)
  (let* ([position (car pos-count)]
         [count    (cdr pos-count)]
         [x (car position)]
         [y (cdr position)]
         [update (curry
                  (λ (dir i count)
                    (let* ([key (match dir
                                  ['x (pos i y)]
                                  ['y (pos x i)])]
                           [value (hash-ref ht key default-point)]
                           [new-count (add1 count)]
                           [new-value
                            (if first-wire?
                                (point #t new-count
                                       (point-second-visited? value)
                                       (point-second-steps value))
                                (point (point-first-visited? value)
                                       (point-first-steps value)
                                       #t new-count))])
                      (hash-set! ht key new-value)
                      new-count)))])
    (match path
      [`(R . ,(? number? n))
       (cons (pos (+ x n) y)
             (foldl (update 'x) count (range (add1 x) (add1 (+ x n)))))]
      [`(L . ,(? number? n))
       (cons (pos (- x n) y)
             (foldl (update 'x) count (reverse (range (- x n) x))))]
      [`(U . ,(? number? n))
       (cons (pos x (+ y n))
             (foldl (update 'y) count (range (add1 y) (add1 (+ y n)))))]
      [`(D . ,(? number? n))
       (cons (pos x (- y n))
             (foldl (update 'y) count (reverse (range (- y n) y))))])))

(define (step-wire first-wire? wire ht)
  (foldl (λ (path pos-count) (step-path first-wire? path ht pos-count)) `(,origin . 0) wire))

(define-values (part1 intersect-points)
  (let* ([hashtable (make-hash)]
         [_ (step-wire #t wire1 hashtable)]
         [_ (step-wire #f wire2 hashtable)]
         [hashlist (hash->list hashtable)]
         [intersections
          (filter (λ (kv)
                    (let ([v (cdr kv)])
                      (and (point-first-visited? v)
                           (point-second-visited? v))))
                  hashlist)]
         [distances
          (map (λ (kv)
                 (let ([k (car kv)])
                   (+ (abs (pos-x k))
                      (abs (pos-y k)))))
               intersections)])
    (values (apply min distances)
            (map cdr intersections))))

(define part2
  (let* ([steps
          (map (λ (v)
                 (+ (point-first-steps v)
                    (point-second-steps v)))
               intersect-points)])
    (apply min steps)))

(show-solution part1 part2)